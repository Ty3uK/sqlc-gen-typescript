use oxc_allocator::{Allocator, Dummy};
use oxc_ast::{
    AstBuilder, NONE,
    ast::{
        Atom, Expression, ImportOrExportKind, NumberBase, PropertyKey, PropertyKind, Span,
        Statement, TSTupleElement, TemplateElementValue, VariableDeclarationKind,
    },
};

use crate::{drivers::Driver, plugin::Query};

pub fn args_decl<'a>(
    allocator: &'a Allocator,
    builder: &'a AstBuilder,
    driver: &'a impl Driver<'a>,
    query: &'a Query,
) -> Statement<'a> {
    let span = Span::dummy(allocator);
    let iface_body = builder.ts_interface_body(
        span,
        builder.vec_from_iter(query.params.iter().enumerate().map(|(index, param)| {
            let column_name: Atom;
            if let Some(column) = &param.column {
                column_name = builder.atom(column.name.as_str());
            } else {
                column_name = builder.atom(format!("arg_{index}").as_str());
            }
            return builder.ts_signature_property_signature(
                span,
                false,
                false,
                false,
                PropertyKey::from(builder.expression_identifier(span, column_name)),
                Some(builder.ts_type_annotation(span, driver.column_type(param.column.as_ref()))),
            );
        })),
    );
    let iface_name_atom = builder.atom(format!("{}Args", query.name).as_str());
    let iface_decl = builder.declaration_ts_interface(
        span,
        builder.binding_identifier(span, iface_name_atom),
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
    return Statement::from(export_decl);
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
        builder.vec_from_iter(query.columns.iter().map(|column| {
            return builder.ts_signature_property_signature(
                span,
                false,
                false,
                false,
                PropertyKey::from(builder.expression_identifier(span, column.name.as_str())),
                Some(builder.ts_type_annotation(span, driver.column_type(Some(column)))),
            );
        })),
    );
    let iface_name_atom = builder.atom(format!("{}Row", query.name).as_str());
    let iface_decl = builder.declaration_ts_interface(
        span,
        builder.binding_identifier(span, iface_name_atom),
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
    return Statement::from(export_decl);
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
        builder.vec_from_iter(query.columns.iter().map(|column| {
            return TSTupleElement::from(driver.column_type(Some(column)));
        })),
    );
    let type_alias_name = builder.atom(format!("{}RowValues", query.name).as_str());
    let type_alias_decl = builder.declaration_ts_type_alias(
        span,
        builder.binding_identifier(span, type_alias_name),
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
    return Statement::from(export_decl);
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
        builder.vec_from_iter(query.columns.iter().enumerate().map(|(index, column)| {
            return builder.object_property_kind_object_property(
                span,
                PropertyKind::Init,
                builder.property_key_static_identifier(span, column.name.as_str()),
                builder.expression_ts_as(
                    span,
                    Expression::from(builder.member_expression_computed(
                        span,
                        builder.expression_identifier(span, "row"),
                        builder.expression_numeric_literal(
                            span,
                            index as f64,
                            Some(builder.atom(&index.to_string())),
                            NumberBase::Decimal,
                        ),
                        false,
                    )),
                    driver.column_type(Some(column)),
                ),
                false,
                false,
                false,
            );
        })),
    );
}

pub fn lowercase_first_letter(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        None => "".to_string(),
        Some(first_char) => first_char.to_lowercase().collect::<String>() + chars.as_str(),
    }
}

pub fn query_decl<'a>(
    allocator: &'a Allocator,
    builder: &'a AstBuilder,
    query: &'a Query,
) -> Statement<'a> {
    let text_name = builder.atom(&lowercase_first_letter(&format!("{}Query", query.name)));
    let sql = format!("\n-- name: {} {}\n{}\n", query.name, query.cmd, query.text);
    let sql_atom = builder.atom(sql.as_str());

    let span = Span::dummy(allocator);
    let var_decl = builder.declaration_variable(
        span,
        VariableDeclarationKind::Const,
        builder.vec1(builder.variable_declarator(
            span,
            VariableDeclarationKind::Const,
            builder.binding_pattern_binding_identifier(span, text_name),
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
    return Statement::from(export_decl);
}
