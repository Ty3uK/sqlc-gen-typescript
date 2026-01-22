use oxc_allocator::Dummy;
use oxc_ast::{
    AstBuilder, NONE,
    ast::{
        BinaryOperator, FormalParameter, FormalParameterKind, FormalParameters, FunctionType,
        ImportOrExportKind, NumberBase, Span, Statement, TSType, UnaryOperator, WithClause,
    },
};

use crate::{
    ast_utils::{
        new_const_decl_statement, new_func_param, new_import_decl, new_method_expr_call,
        new_obj_member_expr,
    },
    drivers::{
        Driver, Query,
        shared::{args_from_params, row_object},
    },
    plugin::Column,
};

pub struct BunSqliteDriver<'a> {
    builder: &'a AstBuilder<'a>,
}

impl<'a> BunSqliteDriver<'a> {
    pub fn new(builder: &'a AstBuilder) -> Self {
        Self { builder }
    }

    fn func_params_decl(&self, query: &'a Query) -> FormalParameters<'a> {
        let span = Span::dummy(self.builder.allocator);
        let mut params = self.builder.vec_with_capacity::<FormalParameter>(2);
        params.push(new_func_param(self.builder, "database", "Database"));
        if query.query.params.len() > 0 {
            params.push(new_func_param(self.builder, "args", query.args));
        }
        return self.builder.formal_parameters(
            span,
            FormalParameterKind::FormalParameter,
            params,
            NONE,
        );
    }

    fn query_decl(&self, query: &'a Query) -> Statement<'a> {
        let span = Span::dummy(self.builder.allocator);
        // const query = {queryName}Query;
        let mut expr = self.builder.expression_identifier(span, query.name);
        let columns = query
            .query
            .params
            .iter()
            .filter_map(|param| param.column.as_ref().filter(|column| column.is_sqlc_slice));
        for column in columns {
            let source = format!("/*SLICE:{}*/?", column.name);
            // () => "?"
            let arrow_fn_expr = self.builder.expression_arrow_function(
                span,
                true,
                false,
                NONE,
                self.builder.formal_parameters(
                    span,
                    FormalParameterKind::ArrowFormalParameters,
                    self.builder.vec(),
                    NONE,
                ),
                NONE,
                self.builder.function_body(
                    span,
                    self.builder.vec(),
                    self.builder.vec1(self.builder.statement_expression(
                        span,
                        self.builder.expression_string_literal(span, "?", None),
                    )),
                ),
            );
            // args.{argName}
            let args_field_expr = new_obj_member_expr(
                self.builder,
                self.builder.expression_identifier(span, "args"),
                column.name.as_str(),
            );
            // args.{argName}.map(() => "?").join(", ")
            let args_map_expr = self.builder.expression_call(
                span,
                // args.{argName}.map(() => "?")
                new_obj_member_expr(
                    self.builder,
                    self.builder.expression_call(
                        span,
                        new_obj_member_expr(self.builder, args_field_expr, "map"),
                        NONE,
                        self.builder.vec1(arrow_fn_expr.into()),
                        false,
                    ),
                    "join",
                ),
                NONE,
                self.builder.vec1(
                    self.builder
                        .expression_string_literal(span, ", ", None)
                        .into(),
                ),
                false,
            );
            // query = {queryName}Query.replace(...)
            expr = self.builder.expression_call(
                span,
                self.builder
                    .member_expression_static(
                        span,
                        expr,
                        self.builder.identifier_name(span, "replace"),
                        false,
                    )
                    .into(),
                NONE,
                self.builder.vec_from_array([
                    self.builder
                        .expression_string_literal(span, self.builder.atom(source.as_str()), None)
                        .into(),
                    args_map_expr.into(),
                ]),
                false,
            );
        }
        return new_const_decl_statement(self.builder, "query", expr);
    }
}

impl<'a> Driver<'a> for BunSqliteDriver<'a> {
    fn column_type(&self, column: Option<&'a Column>) -> TSType<'a> {
        let span = Span::dummy(self.builder.allocator);
        let column = match column {
            Some(val) => val,
            _ => return self.builder.ts_type_any_keyword(span),
        };
        let column_type = match &column.r#type {
            Some(val) => val.name.as_str(),
            _ => return self.builder.ts_type_any_keyword(span),
        };
        let mut result_type = self.builder.ts_type_any_keyword(span);
        match column_type {
            "int" | "integer" | "tinyint" | "smallint" | "mediumint" | "bigint"
            | "unsignedbigint" | "int2" | "int8" => {
                result_type = self.builder.ts_type_number_keyword(span)
            }
            "varchar" | "text" => result_type = self.builder.ts_type_string_keyword(span),
            "blob" => {
                result_type = self.builder.ts_type_type_reference(
                    span,
                    self.builder
                        .ts_type_name_identifier_reference(span, "Buffer"),
                    NONE,
                )
            }
            "real" | "double" | "doubleprecision" | "float" | "timestamp" => {
                result_type = self.builder.ts_type_number_keyword(span)
            }
            "date" | "datetime" => result_type = self.builder.ts_type_string_keyword(span),
            "bool" | "boolean" => result_type = self.builder.ts_type_boolean_keyword(span),
            _ => {}
        }
        if column.is_sqlc_slice {
            result_type = self.builder.ts_type_array_type(span, result_type);
        }
        if column.not_null {
            return result_type;
        }

        return self.builder.ts_type_union_type(
            span,
            self.builder
                .vec_from_array([result_type, self.builder.ts_type_null_keyword(span)]),
        );
    }

    fn preamble(&self, has_execresult: bool) -> Statement<'a> {
        let span = Span::dummy(self.builder.allocator);
        let mut import_specs = self.builder.vec1(new_import_decl(
            self.builder,
            "Database",
            "Database",
            ImportOrExportKind::Value,
        ));
        if has_execresult {
            import_specs.push(new_import_decl(
                self.builder,
                "Changes",
                "Changes",
                ImportOrExportKind::Type,
            ));
        }
        let import_decl = self.builder.module_declaration_import_declaration(
            span,
            Some(import_specs),
            self.builder.string_literal(span, "bun:sqlite", None),
            None,
            Option::<WithClause>::None,
            ImportOrExportKind::Value,
        );
        return import_decl.into();
    }

    fn exec_decl(&self, query: &'a Query) -> Statement<'a> {
        let span = Span::dummy(self.builder.allocator);
        let func_body = self.builder.function_body(
            span,
            self.builder.vec(),
            self.builder.vec_from_array([
                self.query_decl(query),
                // const stmt = database.prepare(listGroupsQuery);
                new_const_decl_statement(
                    self.builder,
                    "stmt",
                    new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "database"),
                        "prepare",
                        self.builder
                            .vec1(self.builder.expression_identifier(span, "query").into()),
                    ),
                ),
                // stmt.run(args.*);
                self.builder.statement_expression(
                    span,
                    new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "stmt"),
                        "run",
                        args_from_params(self.builder, query),
                    ),
                ),
            ]),
        );
        let func_decl = self.builder.declaration_function(
            span,
            FunctionType::FunctionDeclaration,
            Some(self.builder.binding_identifier(span, query.func_name)),
            false,
            false,
            false,
            NONE,
            NONE,
            self.func_params_decl(query),
            Some(
                self.builder
                    .ts_type_annotation(span, self.builder.ts_type_void_keyword(span)),
            ),
            Some(func_body),
        );
        let export_decl = self.builder.module_declaration_export_named_declaration(
            span,
            Some(func_decl),
            self.builder.vec(),
            None,
            ImportOrExportKind::Value,
            NONE,
        );
        return export_decl.into();
    }

    fn one_decl(&'a self, query: &'a Query) -> Statement<'a> {
        let span = Span::dummy(self.builder.allocator);
        let func_return_type = Some(
            self.builder.ts_type_annotation(
                span,
                self.builder.ts_type_union_type(
                    span,
                    self.builder.vec_from_array([
                        self.builder.ts_type_type_reference(
                            span,
                            self.builder
                                .ts_type_name_identifier_reference(span, query.row),
                            NONE,
                        ),
                        self.builder.ts_type_null_keyword(span),
                    ]),
                ),
            ),
        );
        let func_body = self.builder.function_body(
            span,
            self.builder.vec(),
            self.builder.vec_from_array([
                self.query_decl(query),
                // const stmt = database.prepare(listGroupsQuery);
                new_const_decl_statement(
                    self.builder,
                    "stmt",
                    new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "database"),
                        "prepare",
                        self.builder
                            .vec1(self.builder.expression_identifier(span, "query").into()),
                    ),
                ),
                // const rows = stmt.values(args.*);
                new_const_decl_statement(
                    self.builder,
                    "rows",
                    new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "stmt"),
                        "values",
                        args_from_params(self.builder, query),
                    ),
                ),
                // if (rows.length !== 1) {}
                self.builder.statement_if(
                    span,
                    self.builder.expression_binary(
                        span,
                        new_obj_member_expr(
                            self.builder,
                            self.builder.expression_identifier(span, "rows"),
                            "length",
                        ),
                        BinaryOperator::StrictInequality,
                        self.builder.expression_numeric_literal(
                            span,
                            1.0,
                            Some(self.builder.atom("1")),
                            NumberBase::Decimal,
                        ),
                    ),
                    self.builder.statement_block(
                        span,
                        self.builder.vec1(self.builder.statement_return(
                            span,
                            Some(self.builder.expression_null_literal(span)),
                        )),
                    ),
                    None,
                ),
                // const row = rows[0];
                new_const_decl_statement(
                    self.builder,
                    "row",
                    self.builder
                        .member_expression_computed(
                            span,
                            self.builder.expression_identifier(span, "rows"),
                            self.builder.expression_numeric_literal(
                                span,
                                0.0,
                                Some(self.builder.atom("0")),
                                NumberBase::Decimal,
                            ),
                            false,
                        )
                        .into(),
                ),
                // if (!row) {}
                self.builder.statement_if(
                    span,
                    self.builder.expression_unary(
                        span,
                        UnaryOperator::LogicalNot,
                        self.builder.expression_identifier(span, "row"),
                    ),
                    self.builder.statement_block(
                        span,
                        self.builder.vec1(self.builder.statement_return(
                            span,
                            Some(self.builder.expression_null_literal(span)),
                        )),
                    ),
                    None,
                ),
                self.builder.statement_return(
                    span,
                    Some(row_object(
                        self.builder.allocator,
                        self.builder,
                        self,
                        query,
                    )),
                ),
            ]),
        );
        let func_decl = self.builder.declaration_function(
            span,
            FunctionType::FunctionDeclaration,
            Some(self.builder.binding_identifier(span, query.func_name)),
            false,
            false,
            false,
            NONE,
            NONE,
            self.func_params_decl(query),
            func_return_type,
            Some(func_body),
        );
        let export_decl = self.builder.module_declaration_export_named_declaration(
            span,
            Some(func_decl),
            self.builder.vec(),
            None,
            ImportOrExportKind::Value,
            NONE,
        );
        return export_decl.into();
    }

    fn many_decl(&'a self, query: &'a Query) -> Statement<'a> {
        let span = Span::dummy(self.builder.allocator);
        let func_return_type = Some(
            self.builder.ts_type_annotation(
                span,
                self.builder.ts_type_array_type(
                    span,
                    self.builder.ts_type_type_reference(
                        span,
                        self.builder
                            .ts_type_name_identifier_reference(span, query.row),
                        NONE,
                    ),
                ),
            ),
        );
        let arrow_func_body = self.builder.function_body(
            span,
            self.builder.vec(),
            self.builder.vec1(self.builder.statement_expression(
                span,
                self.builder.expression_parenthesized(
                    span,
                    row_object(self.builder.allocator, self.builder, self, query),
                ),
            )),
        );
        let arrow_func_expr = self.builder.expression_arrow_function(
            span,
            true,
            false,
            NONE,
            self.builder.formal_parameters(
                span,
                FormalParameterKind::ArrowFormalParameters,
                self.builder.vec1(self.builder.formal_parameter(
                    span,
                    self.builder.vec(),
                    self.builder.binding_pattern_binding_identifier(span, "row"),
                    NONE,
                    NONE,
                    false,
                    None,
                    false,
                    false,
                )),
                NONE,
            ),
            NONE,
            arrow_func_body,
        );
        let func_body = self.builder.function_body(
            span,
            self.builder.vec(),
            self.builder.vec_from_array([
                self.query_decl(query),
                // const stmt = database.prepare(listGroupsQuery);
                new_const_decl_statement(
                    self.builder,
                    "stmt",
                    new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "database"),
                        "prepare",
                        self.builder
                            .vec1(self.builder.expression_identifier(span, "query").into()),
                    ),
                ),
                // const rows = stmt.values();
                new_const_decl_statement(
                    self.builder,
                    "rows",
                    new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "stmt"),
                        "values",
                        args_from_params(self.builder, query),
                    ),
                ),
                // return rows.map(row => ({ ... });
                self.builder.statement_return(
                    span,
                    Some(new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "rows"),
                        "map",
                        self.builder.vec1(arrow_func_expr.into()),
                    )),
                ),
            ]),
        );
        let func_decl = self.builder.declaration_function(
            span,
            FunctionType::FunctionDeclaration,
            Some(self.builder.binding_identifier(span, query.func_name)),
            false,
            false,
            false,
            NONE,
            NONE,
            self.func_params_decl(query),
            func_return_type,
            Some(func_body),
        );
        let export_decl = self.builder.module_declaration_export_named_declaration(
            span,
            Some(func_decl),
            self.builder.vec(),
            None,
            ImportOrExportKind::Value,
            NONE,
        );
        return export_decl.into();
    }

    fn execresult_decl(&self, query: &'a Query) -> Statement<'a> {
        let span = Span::dummy(self.builder.allocator);
        let func_body = self.builder.function_body(
            span,
            self.builder.vec(),
            self.builder.vec_from_array([
                self.query_decl(query),
                // const stmt = database.prepare(listGroupsQuery);
                new_const_decl_statement(
                    self.builder,
                    "stmt",
                    new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "database"),
                        "prepare",
                        self.builder
                            .vec1(self.builder.expression_identifier(span, "query").into()),
                    ),
                ),
                // return stmt.run(args.*);
                self.builder.statement_return(
                    span,
                    Some(new_method_expr_call(
                        self.builder,
                        self.builder.expression_identifier(span, "stmt"),
                        "run",
                        args_from_params(self.builder, query),
                    )),
                ),
            ]),
        );
        let func_decl = self.builder.declaration_function(
            span,
            FunctionType::FunctionDeclaration,
            Some(self.builder.binding_identifier(span, query.func_name)),
            false,
            false,
            false,
            NONE,
            NONE,
            self.func_params_decl(query),
            Some(
                self.builder.ts_type_annotation(
                    span,
                    self.builder.ts_type_type_reference(
                        span,
                        self.builder
                            .ts_type_name_identifier_reference(span, "Changes"),
                        NONE,
                    ),
                ),
            ),
            Some(func_body),
        );
        let export_decl = self.builder.module_declaration_export_named_declaration(
            span,
            Some(func_decl),
            self.builder.vec(),
            None,
            ImportOrExportKind::Value,
            NONE,
        );
        return export_decl.into();
    }
}
