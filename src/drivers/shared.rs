use oxc_allocator::{Allocator, Dummy, Vec};
use oxc_ast::{
    AstBuilder, NONE,
    ast::{
        Argument, Expression, ImportOrExportKind, NumberBase, PropertyKind, Span, Statement,
        TemplateElementValue, VariableDeclarationKind,
    },
};

use crate::{
    ast_utils::new_obj_member_expr,
    drivers::{Driver, Query},
};

pub fn args_decl<'a>(
    allocator: &'a Allocator,
    builder: &'a AstBuilder,
    driver: &'a impl Driver<'a>,
    query: &'a Query,
) -> Statement<'a> {
    let span = Span::dummy(allocator);
    let iface_body = builder.ts_interface_body(
        span,
        builder.vec_from_iter(query.query.params.iter().enumerate().map(|(index, param)| {
            builder.ts_signature_property_signature(
                span,
                false,
                false,
                false,
                builder
                    .expression_identifier(span, query.arg_names[index])
                    .into(),
                Some(builder.ts_type_annotation(span, driver.column_type(param.column.as_ref()))),
            )
        })),
    );
    let iface_decl = builder.declaration_ts_interface(
        span,
        builder.binding_identifier(span, query.args),
        NONE,
        builder.vec(),
        iface_body,
        false,
    );
    let export_decl = builder.module_declaration_export_named_declaration(
        span,
        Some(iface_decl),
        builder.vec(),
        None,
        ImportOrExportKind::Value,
        NONE,
    );
    return export_decl.into();
}

pub fn row_decl<'a>(
    allocator: &'a Allocator,
    builder: &'a AstBuilder,
    driver: &'a impl Driver<'a>,
    query: &'a Query,
) -> Statement<'a> {
    let span = Span::dummy(allocator);
    let iface_body = builder.ts_interface_body(
        span,
        builder.vec_from_iter(query.query.columns.iter().map(|column| {
            builder.ts_signature_property_signature(
                span,
                false,
                false,
                false,
                builder
                    .expression_identifier(span, column.name.as_str())
                    .into(),
                Some(builder.ts_type_annotation(span, driver.column_type(Some(column)))),
            )
        })),
    );
    let iface_decl = builder.declaration_ts_interface(
        span,
        builder.binding_identifier(span, query.row),
        NONE,
        builder.vec(),
        iface_body,
        false,
    );
    let export_decl = builder.module_declaration_export_named_declaration(
        span,
        Some(iface_decl),
        builder.vec(),
        None,
        ImportOrExportKind::Value,
        NONE,
    );
    return export_decl.into();
}

pub fn row_values_decl<'a>(
    allocator: &'a Allocator,
    builder: &'a AstBuilder,
    driver: &'a impl Driver<'a>,
    query: &'a Query,
) -> Statement<'a> {
    let span = Span::dummy(allocator);
    let type_alias_body = builder.ts_type_tuple_type(
        span,
        builder.vec_from_iter(
            query
                .query
                .columns
                .iter()
                .map(|column| driver.column_type(Some(column)).into()),
        ),
    );
    let type_alias_decl = builder.declaration_ts_type_alias(
        span,
        builder.binding_identifier(span, query.row_values),
        NONE,
        type_alias_body,
        false,
    );
    let export_decl = builder.module_declaration_export_named_declaration(
        span,
        Some(type_alias_decl),
        builder.vec(),
        None,
        ImportOrExportKind::Value,
        NONE,
    );
    return export_decl.into();
}

pub fn row_object<'a>(
    allocator: &'a Allocator,
    builder: &'a AstBuilder,
    driver: &'a impl Driver<'a>,
    query: &'a Query,
) -> Expression<'a> {
    let span = Span::dummy(allocator);
    return builder.expression_object(
        span,
        builder.vec_from_iter(
            query
                .query
                .columns
                .iter()
                .enumerate()
                .map(|(index, column)| {
                    builder.object_property_kind_object_property(
                        span,
                        PropertyKind::Init,
                        builder.property_key_static_identifier(span, column.name.as_str()),
                        builder.expression_ts_as(
                            span,
                            builder
                                .member_expression_computed(
                                    span,
                                    builder.expression_identifier(span, "row"),
                                    builder.expression_numeric_literal(
                                        span,
                                        index as f64,
                                        Some(builder.atom(&index.to_string())),
                                        NumberBase::Decimal,
                                    ),
                                    false,
                                )
                                .into(),
                            driver.column_type(Some(column)),
                        ),
                        false,
                        false,
                        false,
                    )
                }),
        ),
    );
}

pub fn query_decl<'a>(
    allocator: &'a Allocator,
    builder: &'a AstBuilder,
    query: &'a Query,
) -> Statement<'a> {
    let sql = format!(
        "\n-- name: {} {}\n{}\n",
        query.query.name, query.query.cmd, query.query.text
    );
    let sql_atom = builder.atom(sql.as_str());

    let span = Span::dummy(allocator);
    let var_decl = builder.declaration_variable(
        span,
        VariableDeclarationKind::Const,
        builder.vec1(builder.variable_declarator(
            span,
            VariableDeclarationKind::Const,
            builder.binding_pattern_binding_identifier(span, query.name),
            NONE,
            Some(builder.expression_template_literal(
                span,
                builder.vec1(builder.template_element(
                    span,
                    TemplateElementValue {
                        raw: sql_atom,
                        cooked: Some(sql_atom),
                    },
                    true,
                )),
                builder.vec(),
            )),
            false,
        )),
        false,
    );
    let export_decl = builder.module_declaration_export_named_declaration(
        span,
        Some(var_decl),
        builder.vec(),
        None,
        ImportOrExportKind::Value,
        NONE,
    );
    return export_decl.into();
}

pub fn args_from_params<'a>(
    builder: &'a AstBuilder<'a>,
    query: &'a Query,
) -> Vec<'a, Argument<'a>> {
    return builder.vec_from_iter(
        query
            .query
            .params
            .iter()
            .enumerate()
            .map(|(index, _param)| {
                new_obj_member_expr(builder, "args", query.arg_names[index]).into()
            }),
    );
}
