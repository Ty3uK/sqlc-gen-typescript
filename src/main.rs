use std::io::{BufRead, Write};

use oxc_allocator::{Allocator, Dummy, HashMap, Vec};
use oxc_ast::{
    AstBuilder,
    ast::{Program, SourceType, Span},
};
use oxc_codegen::{Codegen, Context, Gen};
use prost::Message;

use crate::{
    drivers::shared::{args_decl, query_decl, row_decl, row_values_decl},
    drivers::{Driver, Query, bun_sqlite::BunSqliteDriver},
    plugin::{File, GenerateRequest, GenerateResponse},
};

mod ast_utils;
mod drivers;
mod plugin {
    include!(concat!(env!("OUT_DIR"), "/plugin.rs"));
}

fn main() -> std::io::Result<()> {
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();
    let buffer = stdin.fill_buf()?;

    let req = GenerateRequest::decode(buffer)?;
    let mut res = GenerateResponse::default();

    let allocator = Allocator::default();
    let builder = AstBuilder::new(&allocator);
    let driver = BunSqliteDriver::new(&builder);

    let mut queries = Vec::<Query>::new_in(&allocator);
    let mut programs = HashMap::<&str, Program>::new_in(&allocator);
    let mut execresults = HashMap::<&str, bool>::new_in(&allocator);

    // Prepare all calculations upfront
    for query in req.queries.iter() {
        let mut chars = query.name.chars();
        let func_name = match chars.next() {
            None => "".to_string(),
            Some(first_char) => first_char.to_lowercase().collect::<String>() + chars.as_str(),
        };
        let name = format!("{}Query", func_name);
        let args = format!("{}Args", query.name);
        let row = format!("{}Row", query.name);
        let row_values = format!("{}RowValues", query.name);
        let mut arg_names = Vec::with_capacity_in(query.params.len(), &allocator);
        for (index, param) in query.params.iter().enumerate() {
            if let Some(column) = &param.column {
                arg_names.push(allocator.alloc_str(column.name.as_str()));
            } else {
                arg_names.push(allocator.alloc_str(format!("arg_{index}").as_str()));
            }
        }
        queries.push(Query {
            func_name: allocator.alloc_str(func_name.as_str()),
            name: allocator.alloc_str(name.as_str()),
            args: allocator.alloc_str(args.as_str()),
            arg_names,
            row: allocator.alloc_str(row.as_str()),
            row_values: allocator.alloc_str(row_values.as_str()),
            query,
        });
        if query.cmd == ":execresult" {
            execresults.entry(query.filename.as_str()).or_insert(true);
        }
    }

    for query in queries.iter() {
        let filename = query.query.filename.as_str();
        let has_execresult = execresults.get(filename).unwrap_or(&false);
        let program = programs.entry(filename).or_insert(builder.program(
            Span::dummy(&allocator),
            SourceType::ts(),
            "",
            builder.vec(),
            None,
            builder.vec(),
            builder.vec1(driver.preamble(*has_execresult)),
        ));
        program.body.push(query_decl(&allocator, &builder, query));
        if query.query.params.len() > 0 {
            program
                .body
                .push(args_decl(&allocator, &builder, &driver, query));
        }
        if query.query.columns.len() > 0 {
            program
                .body
                .push(row_decl(&allocator, &builder, &driver, query));
            program
                .body
                .push(row_values_decl(&allocator, &builder, &driver, query));
        }
        match query.query.cmd.as_str() {
            ":exec" => program.body.push(driver.exec_decl(query)),
            ":one" => program.body.push(driver.one_decl(query)),
            ":many" => program.body.push(driver.many_decl(query)),
            ":execresult" => program.body.push(driver.execresult_decl(query)),
            _ => {}
        }
    }

    for (filename, program) in programs {
        let mut codegen = Codegen::new();
        let mut iter = program.body.iter().peekable();
        while let Some(stmt) = iter.next() {
            stmt.print(&mut codegen, Context::TYPESCRIPT);
            if iter.peek().is_some() {
                codegen.print_str("\n");
            }
        }
        res.files.push(File {
            name: format!("{filename}.ts"),
            contents: codegen.into_source_text().into_bytes(),
        });
    }

    let buf = res.encode_to_vec();
    std::io::stdout().write_all(&buf)?;

    return Result::Ok(());
}
