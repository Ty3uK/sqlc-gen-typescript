use oxc_allocator::{Dummy, Vec};
use oxc_ast::{
    AstBuilder, NONE,
    ast::{
        Argument, Expression, FormalParameter, ImportDeclarationSpecifier, ImportOrExportKind,
        Span, Statement, VariableDeclarationKind,
    },
};

pub fn new_func_param<'a>(
    builder: &'a AstBuilder<'a>,
    name: &'a str,
    r#type: &'a str,
) -> FormalParameter<'a> {
    let span = Span::dummy(builder.allocator);
    return builder.formal_parameter(
        span,
        builder.vec(),
        builder.binding_pattern_binding_identifier(span, name),
        Some(builder.ts_type_annotation(
            span,
            builder.ts_type_type_reference(
                span,
                builder.ts_type_name_identifier_reference(span, r#type),
                NONE,
            ),
        )),
        NONE,
        false,
        None,
        false,
        false,
    );
}

pub fn new_import_decl<'a>(
    builder: &'a AstBuilder<'a>,
    name: &'a str,
    r#as: &'a str,
    kind: ImportOrExportKind,
) -> ImportDeclarationSpecifier<'a> {
    let span = Span::dummy(builder.allocator);
    return builder.import_declaration_specifier_import_specifier(
        span,
        builder.module_export_name_identifier_name(span, name),
        builder.binding_identifier(span, r#as),
        kind,
    );
}

pub fn new_obj_member_expr<'a>(
    builder: &'a AstBuilder<'a>,
    object: Expression<'a>,
    member: &'a str,
) -> Expression<'a> {
    let span = Span::dummy(builder.allocator);
    return builder
        .member_expression_static(span, object, builder.identifier_name(span, member), false)
        .into();
}

pub fn new_method_expr_call<'a>(
    builder: &'a AstBuilder<'a>,
    object: Expression<'a>,
    method: &'a str,
    args: Vec<'a, Argument>,
) -> Expression<'a> {
    let span = Span::dummy(builder.allocator);
    return builder.expression_call(
        span,
        new_obj_member_expr(builder, object, method),
        NONE,
        args,
        false,
    );
}

pub fn new_const_decl_statement<'a>(
    builder: &'a AstBuilder<'a>,
    name: &'a str,
    init: Expression<'a>,
) -> Statement<'a> {
    let span = Span::dummy(builder.allocator);
    return builder
        .declaration_variable(
            span,
            VariableDeclarationKind::Const,
            builder.vec1(builder.variable_declarator(
                span,
                VariableDeclarationKind::Const,
                builder.binding_pattern_binding_identifier(span, name),
                NONE,
                Some(init),
                false,
            )),
            false,
        )
        .into();
}
