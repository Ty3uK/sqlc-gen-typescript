use std::{
    collections::HashMap,
    io::{BufRead, Write},
};

use oxc_allocator::{Allocator, Dummy};
use oxc_ast::{
    AstBuilder,
    ast::{SourceType, Span, Statement},
};
use oxc_codegen::Codegen;
use prost::Message;

use crate::{
    drivers::Driver,
    drivers::bun_sqlite::BunSqliteDriver,
    plugin::{File, GenerateRequest, GenerateResponse},
    shared::{args_decl, query_decl, row_decl, row_values_decl},
};

mod drivers;
mod shared;
mod plugin {
    include!(concat!(env!("OUT_DIR"), "/plugin.rs"));
}

fn main() -> std::io::Result<()> {
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    let buffer = stdin.fill_buf()?;
    let req = GenerateRequest::decode(buffer)?;
    let mut res = GenerateResponse::default();
    let mut files = HashMap::<String, File>::new();

    let allocator = Allocator::default();
    let builder = AstBuilder::new(&allocator);
    let driver = BunSqliteDriver::new(&allocator, &builder);

    for query in req.queries {
        let mut need_preamble = false;
        let file = match files.get_mut(&query.filename) {
            Some(val) => val,
            None => {
                need_preamble = true;
                let file = File {
                    name: format!("{}.ts", query.filename),
                    contents: vec![],
                };
                files.insert(query.filename.clone(), file);
                files.get_mut(&query.filename).unwrap()
            }
        };
        let mut body: oxc_allocator::Vec<'_, Statement> = oxc_allocator::Vec::new_in(&allocator);
        if need_preamble {
            body.push(driver.preamble());
        }
        body.push(query_decl(&allocator, &builder, &query));
        if query.params.len() > 0 {
            body.push(args_decl(&allocator, &builder, &driver, &query));
        }
        if query.columns.len() > 0 {
            body.push(row_decl(&allocator, &builder, &driver, &query));
            body.push(row_values_decl(&allocator, &builder, &driver, &query));
        }
        match query.cmd.as_str() {
            ":exec" => body.push(driver.exec_decl(&query)),
            ":one" => body.push(driver.one_decl(&query)),
            ":many" => body.push(driver.many_decl(&query)),
            _ => {}
        }

        let program = builder.program(
            Span::dummy(&allocator),
            SourceType::ts(),
            "",
            builder.vec(),
            None,
            builder.vec(),
            body,
        );
        let code = Codegen::new().build(&program);
        file.contents.append(&mut code.code.into_bytes())
    }

    res.files = files.into_values().collect();

    let buf = res.encode_to_vec();
    std::io::stdout().write_all(&buf)?;

    return Result::Ok(());
}
