use oxc_ast::ast::{Statement, TSType};
use crate::plugin::{Column, Query};

pub mod bun_sqlite;

pub trait Driver<'a> {
    fn column_type(&'a self, column: Option<&'a Column>) -> TSType<'a>;
    fn preamble(&'a self) -> Statement<'a>;
    fn exec_decl(&'a self, query: &'a Query) -> Statement<'a>;
    fn one_decl(&'a self, query: &'a Query) -> Statement<'a>;
    fn many_decl(&'a self, query: &'a Query) -> Statement<'a>;
}
