use crate::plugin::Column;
use oxc_allocator::Vec;
use oxc_ast::ast::{Statement, TSType};

pub mod bun_sqlite;
pub mod shared;

pub trait Driver<'a> {
    fn column_type(&'a self, column: Option<&'a Column>) -> TSType<'a>;
    fn preamble(&'a self, has_execresult: bool) -> Statement<'a>;
    fn exec_decl(&'a self, query: &'a Query) -> Statement<'a>;
    fn one_decl(&'a self, query: &'a Query) -> Statement<'a>;
    fn many_decl(&'a self, query: &'a Query) -> Statement<'a>;
    fn execresult_decl(&'a self, query: &'a Query) -> Statement<'a>;
}

pub struct Query<'a> {
    pub func_name: &'a str,
    pub name: &'a str,
    pub args: &'a str,
    pub arg_names: Vec<'a, &'a str>,
    pub row: &'a str,
    pub row_values: &'a str,
    pub query: &'a crate::plugin::Query,
}
