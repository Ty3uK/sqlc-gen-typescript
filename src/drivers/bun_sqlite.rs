use oxc_allocator::{Allocator, Dummy};
use oxc_ast::{
    AstBuilder, NONE,
    ast::{
        Argument, Atom, BinaryOperator, Expression, FormalParameter, FormalParameterKind,
        FormalParameters, FunctionType, ImportOrExportKind, NumberBase, Span, Statement, TSType,
        UnaryOperator, VariableDeclarationKind, WithClause,
    },
};

use crate::{
    drivers::Driver, plugin::{Column, Query}, shared::{lowercase_first_letter, row_object}
};

pub struct BunSqliteDriver<'a> {
    allocator: &'a Allocator,
    builder: &'a AstBuilder<'a>,
}

impl<'a> BunSqliteDriver<'a> {
    pub fn new(allocator: &'a Allocator, builder: &'a AstBuilder) -> Self {
        Self { allocator, builder }
    }

    fn func_params_decl(&self, query: &'a Query) -> FormalParameters<'a> {
        let span = Span::dummy(self.allocator);
        let mut params = self.builder.vec_with_capacity::<FormalParameter>(2);
        params.push(
            self.builder.formal_parameter(
                span,
                self.builder.vec(),
                self.builder
                    .binding_pattern_binding_identifier(span, "database"),
                Some(
                    self.builder.ts_type_annotation(
                        span,
                        self.builder.ts_type_type_reference(
                            span,
                            self.builder
                                .ts_type_name_identifier_reference(span, "Database"),
                            NONE,
                        ),
                    ),
                ),
                NONE,
                false,
                None,
                false,
                false,
            ),
        );
        if query.params.len() > 0 {
            let args_param_name = self.builder.atom(format!("{}Args", query.name).as_str());
            params.push(
                self.builder.formal_parameter(
                    span,
                    self.builder.vec(),
                    self.builder
                        .binding_pattern_binding_identifier(span, "args"),
                    Some(
                        self.builder.ts_type_annotation(
                            span,
                            self.builder.ts_type_type_reference(
                                span,
                                self.builder
                                    .ts_type_name_identifier_reference(span, args_param_name),
                                NONE,
                            ),
                        ),
                    ),
                    NONE,
                    false,
                    None,
                    false,
                    false,
                ),
            );
        }
        return self.builder.formal_parameters(
            span,
            FormalParameterKind::FormalParameter,
            params,
            NONE,
        );
    }
}

impl<'a> Driver<'a> for BunSqliteDriver<'a> {
    fn column_type(&self, column: Option<&'a Column>) -> TSType<'a> {
        let mut not_null = false;
        let mut column_type = Atom::empty();
        if let Some(c) = column {
            not_null = c.not_null;
            if let Some(t) = &c.r#type {
                column_type = self.builder.atom(&t.name);
            }
        }
        let span = Span::dummy(self.allocator);
        let mut result_type = self.builder.ts_type_any_keyword(span);
        match column_type.as_str() {
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
        if not_null {
            return result_type;
        }

        return self.builder.ts_type_union_type(
            span,
            self.builder
                .vec_from_array([result_type, self.builder.ts_type_null_keyword(span)]),
        );
    }

    fn preamble(&self) -> Statement<'a> {
        let span = Span::dummy(self.allocator);
        let import_spec = self.builder.import_declaration_specifier_import_specifier(
            span,
            self.builder
                .module_export_name_identifier_name(span, "Database"),
            self.builder.binding_identifier(span, "Database"),
            ImportOrExportKind::Value,
        );
        let import_decl = self.builder.module_declaration_import_declaration(
            span,
            Some(self.builder.vec1(import_spec)),
            self.builder.string_literal(span, "bun:sqlite", None),
            None,
            Option::<WithClause>::None,
            ImportOrExportKind::Value,
        );
        return Statement::from(import_decl);
    }

    fn exec_decl(&self, query: &'a Query) -> Statement<'a> {
        let span = Span::dummy(self.allocator);
        let func_name = self.builder.atom(&lowercase_first_letter(&query.name));
        let func_body = self.builder.function_body(
            span,
            self.builder.vec(),
            self.builder.vec_from_array([
                // const stmt = database.prepare(listGroupsQuery);
                Statement::from(
                    self.builder.declaration_variable(
                        span,
                        VariableDeclarationKind::Const,
                        self.builder.vec1(
                            self.builder.variable_declarator(
                                span,
                                VariableDeclarationKind::Const,
                                self.builder
                                    .binding_pattern_binding_identifier(span, "stmt"),
                                NONE,
                                Some(self.builder.expression_call(
                                    span,
                                    Expression::from(self.builder.member_expression_static(
                                        span,
                                        self.builder.expression_identifier(span, "database"),
                                        self.builder.identifier_name(span, "prepare"),
                                        false,
                                    )),
                                    NONE,
                                    self.builder.vec1(Argument::from(
                                        self.builder.expression_identifier(
                                            span,
                                            self.builder.atom(&format!(
                                                "{}Query",
                                                lowercase_first_letter(&query.name)
                                            )),
                                        ),
                                    )),
                                    false,
                                )),
                                false,
                            ),
                        ),
                        false,
                    ),
                ),
                // stmt.run(args.*);
                self.builder.statement_expression(
                    span,
                    self.builder.expression_call(
                        span,
                        Expression::from(self.builder.member_expression_static(
                            span,
                            self.builder.expression_identifier(span, "stmt"),
                            self.builder.identifier_name(span, "run"),
                            false,
                        )),
                        NONE,
                        self.builder
                            .vec_from_iter(query.params.iter().enumerate().map(
                                |(index, param)| {
                                    let column_name: Atom;
                                    if let Some(column) = &param.column {
                                        column_name = self.builder.atom(column.name.as_str());
                                    } else {
                                        column_name =
                                            self.builder.atom(format!("arg_{index}").as_str());
                                    }
                                    return Argument::from(
                                        self.builder.member_expression_static(
                                            span,
                                            self.builder.expression_identifier(span, "args"),
                                            self.builder.identifier_name(
                                                span,
                                                self.builder.atom(&column_name),
                                            ),
                                            false,
                                        ),
                                    );
                                },
                            )),
                        false,
                    ),
                ),
            ]),
        );
        let func_decl = self.builder.declaration_function(
            span,
            FunctionType::FunctionDeclaration,
            Some(self.builder.binding_identifier(span, func_name)),
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
        return Statement::from(export_decl);
    }

    fn one_decl(&'a self, query: &'a Query) -> Statement<'a> {
        let span = Span::dummy(self.allocator);
        let func_name = self.builder.atom(&lowercase_first_letter(&query.name));
        let func_return_type = Some(self.builder.ts_type_annotation(
            span,
            self.builder.ts_type_union_type(
                span,
                self.builder.vec_from_array([
                    self.builder.ts_type_type_reference(
                        span,
                        self.builder.ts_type_name_identifier_reference(
                            span,
                            self.builder.atom(&format!("{}Row", query.name)),
                        ),
                        NONE,
                    ),
                    self.builder.ts_type_null_keyword(span),
                ]),
            ),
        ));
        let func_body = self.builder.function_body(
            span,
            self.builder.vec(),
            self.builder.vec_from_array([
                // const stmt = database.prepare(listGroupsQuery);
                Statement::from(
                    self.builder.declaration_variable(
                        span,
                        VariableDeclarationKind::Const,
                        self.builder.vec1(
                            self.builder.variable_declarator(
                                span,
                                VariableDeclarationKind::Const,
                                self.builder
                                    .binding_pattern_binding_identifier(span, "stmt"),
                                NONE,
                                Some(self.builder.expression_call(
                                    span,
                                    Expression::from(self.builder.member_expression_static(
                                        span,
                                        self.builder.expression_identifier(span, "database"),
                                        self.builder.identifier_name(span, "prepare"),
                                        false,
                                    )),
                                    NONE,
                                    self.builder.vec1(Argument::from(
                                        self.builder.expression_identifier(
                                            span,
                                            self.builder.atom(&format!(
                                                "{}Query",
                                                lowercase_first_letter(&query.name)
                                            )),
                                        ),
                                    )),
                                    false,
                                )),
                                false,
                            ),
                        ),
                        false,
                    ),
                ),
                // const rows = stmt.values(args.*);
                Statement::from(
                    self.builder.declaration_variable(
                        span,
                        VariableDeclarationKind::Const,
                        self.builder.vec1(
                            self.builder.variable_declarator(
                                span,
                                VariableDeclarationKind::Const,
                                self.builder
                                    .binding_pattern_binding_identifier(span, "rows"),
                                NONE,
                                Some(self.builder.expression_call(
                                    span,
                                    Expression::from(self.builder.member_expression_static(
                                        span,
                                        self.builder.expression_identifier(span, "stmt"),
                                        self.builder.identifier_name(span, "values"),
                                        false,
                                    )),
                                    NONE,
                                    self.builder.vec_from_iter(
                                        query.params.iter().enumerate().map(|(index, param)| {
                                            let column_name: Atom;
                                            if let Some(column) = &param.column {
                                                column_name =
                                                    self.builder.atom(column.name.as_str());
                                            } else {
                                                column_name = self
                                                    .builder
                                                    .atom(format!("arg_{index}").as_str());
                                            }
                                            return Argument::from(
                                                self.builder.member_expression_static(
                                                    span,
                                                    self.builder
                                                        .expression_identifier(span, "args"),
                                                    self.builder.identifier_name(
                                                        span,
                                                        self.builder.atom(&column_name),
                                                    ),
                                                    false,
                                                ),
                                            );
                                        }),
                                    ),
                                    false,
                                )),
                                false,
                            ),
                        ),
                        false,
                    ),
                ),
                self.builder.statement_if(
                    span,
                    self.builder.expression_binary(
                        span,
                        Expression::from(self.builder.member_expression_static(
                            span,
                            self.builder.expression_identifier(span, "rows"),
                            self.builder.identifier_name(span, "length"),
                            false,
                        )),
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
                Statement::from(self.builder.declaration_variable(
                    span,
                    VariableDeclarationKind::Const,
                    self.builder.vec1(self.builder.variable_declarator(
                        span,
                        VariableDeclarationKind::Const,
                        self.builder.binding_pattern_binding_identifier(span, "row"),
                        NONE,
                        Some(Expression::from(self.builder.member_expression_computed(
                            span,
                            self.builder.expression_identifier(span, "rows"),
                            self.builder.expression_numeric_literal(
                                span,
                                0.0,
                                Some(self.builder.atom("0")),
                                NumberBase::Decimal,
                            ),
                            false,
                        ))),
                        false,
                    )),
                    false,
                )),
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
                    Some(row_object(self.allocator, self.builder, self, query)),
                ),
            ]),
        );
        let func_decl = self.builder.declaration_function(
            span,
            FunctionType::FunctionDeclaration,
            Some(self.builder.binding_identifier(span, func_name)),
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
        return Statement::from(export_decl);
    }

    fn many_decl(&'a self, query: &'a Query) -> Statement<'a> {
        let span = Span::dummy(self.allocator);
        let func_name = self.builder.atom(&lowercase_first_letter(&query.name));
        let func_return_type = Some(self.builder.ts_type_annotation(
            span,
            self.builder.ts_type_array_type(
                span,
                self.builder.ts_type_type_reference(
                    span,
                    self.builder.ts_type_name_identifier_reference(
                        span,
                        self.builder.atom(&format!("{}Row", query.name)),
                    ),
                    NONE,
                ),
            ),
        ));
        let arrow_func_body = self.builder.function_body(
            span,
            self.builder.vec(),
            self.builder.vec1(self.builder.statement_expression(
                span,
                self.builder.expression_parenthesized(
                    span,
                    row_object(self.allocator, self.builder, self, query),
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
                // const stmt = database.prepare(listGroupsQuery);
                Statement::from(
                    self.builder.declaration_variable(
                        span,
                        VariableDeclarationKind::Const,
                        self.builder.vec1(
                            self.builder.variable_declarator(
                                span,
                                VariableDeclarationKind::Const,
                                self.builder
                                    .binding_pattern_binding_identifier(span, "stmt"),
                                NONE,
                                Some(self.builder.expression_call(
                                    span,
                                    Expression::from(self.builder.member_expression_static(
                                        span,
                                        self.builder.expression_identifier(span, "database"),
                                        self.builder.identifier_name(span, "prepare"),
                                        false,
                                    )),
                                    NONE,
                                    self.builder.vec1(Argument::from(
                                        self.builder.expression_identifier(
                                            span,
                                            self.builder.atom(&format!(
                                                "{}Query",
                                                lowercase_first_letter(&query.name)
                                            )),
                                        ),
                                    )),
                                    false,
                                )),
                                false,
                            ),
                        ),
                        false,
                    ),
                ),
                // const rows = stmt.values();
                Statement::from(
                    self.builder.declaration_variable(
                        span,
                        VariableDeclarationKind::Const,
                        self.builder.vec1(
                            self.builder.variable_declarator(
                                span,
                                VariableDeclarationKind::Const,
                                self.builder
                                    .binding_pattern_binding_identifier(span, "rows"),
                                NONE,
                                Some(self.builder.expression_call(
                                    span,
                                    Expression::from(self.builder.member_expression_static(
                                        span,
                                        self.builder.expression_identifier(span, "stmt"),
                                        self.builder.identifier_name(span, "values"),
                                        false,
                                    )),
                                    NONE,
                                    self.builder.vec(),
                                    false,
                                )),
                                false,
                            ),
                        ),
                        false,
                    ),
                ),
                // return rows.map(row => ({ ... });
                self.builder.statement_return(
                    span,
                    Some(self.builder.expression_call(
                        span,
                        Expression::from(self.builder.member_expression_static(
                            span,
                            self.builder.expression_identifier(span, "rows"),
                            self.builder.identifier_name(span, "map"),
                            false,
                        )),
                        NONE,
                        self.builder.vec1(Argument::from(arrow_func_expr)),
                        false,
                    )),
                ),
            ]),
        );
        let func_decl = self.builder.declaration_function(
            span,
            FunctionType::FunctionDeclaration,
            Some(self.builder.binding_identifier(span, func_name)),
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
        return Statement::from(export_decl);
    }
}
