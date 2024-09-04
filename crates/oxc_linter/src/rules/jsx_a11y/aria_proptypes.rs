use oxc_ast::{
    ast::{JSXAttributeItem, JSXAttributeValue, JSXExpression},
    AstKind,
};
use oxc_diagnostics::OxcDiagnostic;
use oxc_macros::declare_oxc_lint;
use oxc_span::Span;

use crate::{
    context::LintContext,
    globals::VALID_ARIA_PROPS,
    rule::Rule,
    utils::{get_jsx_attribute_name, parse_jsx_value},
    AstNode,
};

fn aria_proptypes_diagnostic(span: Span) -> OxcDiagnostic {
    // TODO: Update the message and help text
    OxcDiagnostic::warn("The value for ${name} must be a ${type}.").with_label(span)
}

#[derive(Debug, Default, Clone)]
pub struct AriaProptypes;

declare_oxc_lint!(
    /// ### What it does
    ///
    ///
    /// ### Why is this bad?
    ///
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```jsx
    /// FIXME: Tests will fail if examples are missing or syntactically incorrect.
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```jsx
    /// FIXME: Tests will fail if examples are missing or syntactically incorrect.
    /// ```
    AriaProptypes,
    correctness,
    pending  // TODO: describe fix capabilities. Remove if no fix can be done,
             // keep at 'pending' if you think one could be added but don't know how.
             // Options are 'fix', 'fix_dangerous', 'suggestion', and 'conditional_fix_suggestion'
);

impl Rule for AriaProptypes {
    fn run<'a>(&self, node: &AstNode<'a>, ctx: &LintContext<'a>) {
        let AstKind::JSXOpeningElement(jsx_el) = node.kind() else {
            return;
        };

        for attr in &jsx_el.attributes {
            let JSXAttributeItem::Attribute(attr) = attr else {
                continue;
            };
            let attr_name = get_jsx_attribute_name(&attr.name).to_lowercase();
            let attr_value = attr.value.as_ref();
            if VALID_ARIA_PROPS.contains(attr_name.as_str()) {
                if !is_aria_prop_valid(attr_name, attr_value) {
                    ctx.diagnostic(aria_proptypes_diagnostic(attr.span));
                    return;
                }
            }
        }
    }
}

#[test]
fn test() {
    use crate::tester::Tester;

    let pass = vec![
        r#"<div aria-foo="true" />"#,
        r#"<div abcaria-foo="true" />"#,
        "<div aria-hidden={true} />",
        "<div aria-hidden={true && false} />", // added test
        r#"<div aria-hidden="true" />"#,
        r#"<div aria-hidden={"false"} />"#,
        "<div aria-hidden={!false} />",
        "<div aria-hidden />",
        "<div aria-hidden={false} />",
        "<div aria-hidden={!true} />",
        r#"<div aria-hidden={!"yes"} />"#,
        "<div aria-hidden={foo} />",
        "<div aria-hidden={foo.bar} />",
        "<div aria-hidden={null} />",
        "<div aria-hidden={undefined} />",
        "<div aria-hidden={<div />} />",
        r#"<div aria-label="Close" />"#,
        "<div aria-label={`Close`} />",
        "<div aria-label={foo} />",
        "<div aria-label={foo.bar} />",
        "<div aria-label={null} />",
        "<div aria-label={undefined} />",
        r#"<input aria-invalid={error ? "true" : "false"} />"#,
        r#"<input aria-invalid={undefined ? "true" : "false"} />"#,
        "<div aria-checked={true} />",
        r#"<div aria-checked="true" />"#,
        r#"<div aria-checked={"false"} />"#,
        "<div aria-checked={!false} />",
        "<div aria-checked />",
        "<div aria-checked={false} />",
        "<div aria-checked={!true} />",
        r#"<div aria-checked={!"yes"} />"#,
        "<div aria-checked={foo} />",
        "<div aria-checked={foo.bar} />",
        r#"<div aria-checked="mixed" />"#,
        "<div aria-checked={`mixed`} />",
        "<div aria-checked={null} />",
        "<div aria-checked={undefined} />",
        "<div aria-level={123} />",
        "<div aria-level={-123} />",
        "<div aria-level={+123} />",
        "<div aria-level={~123} />",
        r#"<div aria-level={"123"} />"#,
        "<div aria-level={`123`} />",
        r#"<div aria-level="123" />"#,
        "<div aria-level={foo} />",
        "<div aria-level={foo.bar} />",
        "<div aria-level={null} />",
        "<div aria-level={undefined} />",
        "<div aria-valuemax={123} />",
        "<div aria-valuemax={-123} />",
        "<div aria-valuemax={+123} />",
        "<div aria-valuemax={~123} />",
        r#"<div aria-valuemax={"123"} />"#,
        "<div aria-valuemax={`123`} />",
        r#"<div aria-valuemax="123" />"#,
        "<div aria-valuemax={foo} />",
        "<div aria-valuemax={foo.bar} />",
        "<div aria-valuemax={null} />",
        "<div aria-valuemax={undefined} />",
        r#"<div aria-sort="ascending" />"#,
        r#"<div aria-sort="ASCENDING" />"#,
        r#"<div aria-sort={"ascending"} />"#,
        "<div aria-sort={`ascending`} />",
        r#"<div aria-sort="descending" />"#,
        r#"<div aria-sort={"descending"} />"#,
        "<div aria-sort={`descending`} />",
        r#"<div aria-sort="none" />"#,
        r#"<div aria-sort={"none"} />"#,
        "<div aria-sort={`none`} />",
        r#"<div aria-sort="other" />"#,
        r#"<div aria-sort={"other"} />"#,
        "<div aria-sort={`other`} />",
        "<div aria-sort={foo} />",
        "<div aria-sort={foo.bar} />",
        "<div aria-invalid={true} />",
        r#"<div aria-invalid="true" />"#,
        "<div aria-invalid={false} />",
        r#"<div aria-invalid="false" />"#,
        r#"<div aria-invalid="grammar" />"#,
        r#"<div aria-invalid="spelling" />"#,
        "<div aria-invalid={null} />",
        "<div aria-invalid={undefined} />",
        r#"<div aria-relevant="additions" />"#,
        r#"<div aria-relevant={"additions"} />"#,
        "<div aria-relevant={`additions`} />",
        r#"<div aria-relevant="additions removals" />"#,
        r#"<div aria-relevant="additions additions" />"#,
        r#"<div aria-relevant={"additions removals"} />"#,
        "<div aria-relevant={`additions removals`} />",
        r#"<div aria-relevant="additions removals text" />"#,
        r#"<div aria-relevant={"additions removals text"} />"#,
        "<div aria-relevant={`additions removals text`} />",
        r#"<div aria-relevant="additions removals text all" />"#,
        r#"<div aria-relevant={"additions removals text all"} />"#,
        "<div aria-relevant={`removals additions text all`} />",
        "<div aria-relevant={foo} />",
        "<div aria-relevant={foo.bar} />",
        "<div aria-relevant={null} />",
        "<div aria-relevant={undefined} />",
        r#"<div aria-activedescendant="ascending" />"#,
        r#"<div aria-activedescendant="ASCENDING" />"#,
        r#"<div aria-activedescendant={"ascending"} />"#,
        "<div aria-activedescendant={`ascending`} />",
        r#"<div aria-activedescendant="descending" />"#,
        r#"<div aria-activedescendant={"descending"} />"#,
        "<div aria-activedescendant={`descending`} />",
        r#"<div aria-activedescendant="none" />"#,
        r#"<div aria-activedescendant={"none"} />"#,
        "<div aria-activedescendant={`none`} />",
        r#"<div aria-activedescendant="other" />"#,
        r#"<div aria-activedescendant={"other"} />"#,
        "<div aria-activedescendant={`other`} />",
        "<div aria-activedescendant={foo} />",
        "<div aria-activedescendant={foo.bar} />",
        "<div aria-activedescendant={null} />",
        "<div aria-activedescendant={undefined} />",
        r#"<div aria-labelledby="additions" />"#,
        r#"<div aria-labelledby={"additions"} />"#,
        "<div aria-labelledby={`additions`} />",
        r#"<div aria-labelledby="additions removals" />"#,
        r#"<div aria-labelledby="additions additions" />"#,
        r#"<div aria-labelledby={"additions removals"} />"#,
        "<div aria-labelledby={`additions removals`} />",
        r#"<div aria-labelledby="additions removals text" />"#,
        r#"<div aria-labelledby={"additions removals text"} />"#,
        "<div aria-labelledby={`additions removals text`} />",
        r#"<div aria-labelledby="additions removals text all" />"#,
        r#"<div aria-labelledby={"additions removals text all"} />"#,
        "<div aria-labelledby={`removals additions text all`} />",
        "<div aria-labelledby={foo} />",
        "<div aria-labelledby={foo.bar} />",
        "<div aria-labelledby={null} />",
        "<div aria-labelledby={undefined} />",
    ];

    let fail = vec![
        r#"<div aria-hidden="yes" />"#,
        r#"<div aria-hidden="no" />"#,
        r#"<div aria-hidden={"no"} />"#, // added test
        "<div aria-hidden={1234} />",
        "<div aria-hidden={`${abc}`} />",
        "<div aria-label />",
        "<div aria-label={true} />",
        "<div aria-label={false} />",
        "<div aria-label={1234} />",
        "<div aria-label={!true} />",
        r#"<div aria-checked="yes" />"#,
        r#"<div aria-checked="no" />"#,
        "<div aria-checked={1234} />",
        "<div aria-checked={`${abc}`} />",
        r#"<div aria-level="yes" />"#,
        r#"<div aria-level="no" />"#,
        "<div aria-level={`abc`} />",
        "<div aria-level={true} />",
        "<div aria-level />",
        r#"<div aria-level={"false"} />"#,
        r#"<div aria-level={!"false"} />"#,
        r#"<div aria-valuemax="yes" />"#,
        r#"<div aria-valuemax="no" />"#,
        "<div aria-valuemax={`abc`} />",
        "<div aria-valuemax={true} />",
        "<div aria-valuemax />",
        r#"<div aria-valuemax={"false"} />"#,
        r#"<div aria-valuemax={!"false"} />"#,
        r#"<div aria-sort="" />"#,
        r#"<div aria-sort="descnding" />"#,
        "<div aria-sort />",
        "<div aria-sort={true} />",
        r#"<div aria-sort={"false"} />"#,
        r#"<div aria-sort="ascending descending" />"#,
        r#"<div aria-relevant="" />"#,
        r#"<div aria-relevant="foobar" />"#,
        "<div aria-relevant />",
        "<div aria-relevant={true} />",
        r#"<div aria-relevant={"false"} />"#,
        r#"<div aria-relevant="additions removalss" />"#,
        r#"<div aria-relevant="additions removalss " />"#,
    ];

    Tester::new(AriaProptypes::NAME, pass, fail).test_and_snapshot();
}

/// Given an ARIA property name and its value, determine if the value is valid
/// for the type of the property.
fn is_aria_prop_valid(prop_name: String, prop_value: Option<&JSXAttributeValue>) -> bool {
    let Some(prop_type) = ARIA_PROPERTY_TYPES.get(prop_name.as_str()) else {
        // Unless we have an explicit type, we should assume it's valid to prevent false positives
        return true;
    };
    dbg!(prop_name, prop_type, prop_value);

    // Ignore the attribute if its prop value is null or undefined
    if let Some(JSXAttributeValue::ExpressionContainer(expr)) = prop_value {
        if let JSXExpression::NullLiteral(_) = &expr.expression {
            return true;
        }
        if let JSXExpression::Identifier(ident) = &expr.expression {
            if ident.name == "undefined" {
                return true;
            }
        }
    }

    match prop_type {
        AriaPropertyType::Boolean { allow_undefined } => match prop_value {
            Some(JSXAttributeValue::StringLiteral(value)) => is_boolean_value(&value.value),
            Some(JSXAttributeValue::ExpressionContainer(expr)) => match &expr.expression {
                JSXExpression::StringLiteral(str_lit) => is_boolean_value(&str_lit.value),
                JSXExpression::TemplateLiteral(template) => {
                    template.expressions.is_empty()
                        && is_boolean_value(&template.quasis[0].value.raw)
                }
                JSXExpression::BooleanLiteral(_)
                | JSXExpression::LogicalExpression(_)
                | JSXExpression::UnaryExpression(_) => true,
                JSXExpression::NumericLiteral(_) => false,
                _ => true,
            },
            None => *allow_undefined,
            _ => false,
        },
        AriaPropertyType::String => match prop_value {
            Some(JSXAttributeValue::StringLiteral(_)) => true,
            Some(JSXAttributeValue::ExpressionContainer(expr)) => match &expr.expression {
                JSXExpression::StringLiteral(_) | JSXExpression::TemplateLiteral(_) => true,
                JSXExpression::NumericLiteral(_)
                | JSXExpression::BooleanLiteral(_)
                | JSXExpression::UnaryExpression(_)
                | JSXExpression::LogicalExpression(_) => false,
                _ => true,
            },
            _ => false,
        },
        AriaPropertyType::Id => match prop_value {
            Some(JSXAttributeValue::StringLiteral(_)) => true,
            Some(JSXAttributeValue::ExpressionContainer(expr)) => match &expr.expression {
                JSXExpression::StringLiteral(_) | JSXExpression::TemplateLiteral(_) => true,
                JSXExpression::NumericLiteral(_) => false,
                JSXExpression::BooleanLiteral(_) => false,
                _ => true,
            },
            _ => false,
        },
        AriaPropertyType::IdList => todo!(),
        AriaPropertyType::Integer => {
            let Some(prop_value) = prop_value else {
                return false;
            };
            let is_integer_value =
                parse_jsx_value(prop_value).map_or(false, |num| num.fract() == 0.0);
            match prop_value {
                JSXAttributeValue::StringLiteral(_) => is_integer_value,
                JSXAttributeValue::ExpressionContainer(expr) => match &expr.expression {
                    JSXExpression::Identifier(_) => true,
                    JSXExpression::StaticMemberExpression(_) => true,
                    _ => is_integer_value,
                },
                _ => false,
            }
        }
        AriaPropertyType::Number => todo!(),
        AriaPropertyType::Token { values: _ } => todo!(),
        AriaPropertyType::TokenList { values: _ } => todo!(),
        AriaPropertyType::Tristate => match prop_value {
            Some(JSXAttributeValue::StringLiteral(value)) => is_tristate_value(&value.value),
            Some(JSXAttributeValue::ExpressionContainer(expr)) => match &expr.expression {
                JSXExpression::StringLiteral(str_lit) => is_tristate_value(&str_lit.value),
                JSXExpression::BooleanLiteral(_)
                | JSXExpression::LogicalExpression(_)
                | JSXExpression::UnaryExpression(_) => true,
                JSXExpression::NumericLiteral(_) => false,
                JSXExpression::TemplateLiteral(template) => {
                    template.expressions.is_empty()
                        && is_tristate_value(&template.quasis[0].value.raw)
                }
                _ => true,
            },
            None => true,
            _ => false,
        },
    }
}

fn is_boolean_value(value: &str) -> bool {
    value == "true" || value == "false"
}

fn is_tristate_value(value: &str) -> bool {
    is_boolean_value(value) || value == "mixed"
}

/// https://www.w3.org/TR/wai-aria-1.2/#propcharacteristic_value
#[derive(Debug)]
enum AriaPropertyType {
    /// Unconstrained value type
    String,
    /// Reference to the ID of another element in the same document
    Id,
    /// A list of one or more ID references.
    IdList,
    /// A numerical value without a fractional component.
    Integer,
    /// Value representing either true or false. The default value for this value type is false unless otherwise specified.
    Boolean { allow_undefined: bool },
    /// Any real numerical value.
    Number,
    /// One of a limited set of allowed values.
    Token { values: &'static [&'static str] },
    /// A list of one or more tokens.
    TokenList { values: &'static [&'static str] },
    /// Value representing true, false, mixed, or undefined values. The default value for this value type is undefined unless otherwise specified.
    Tristate,
}

const ARIA_PROPERTY_TYPES: phf::Map<&'static str, AriaPropertyType> = phf::phf_map! {
    "aria-activedescendant" => AriaPropertyType::Id,
    "aria-atomic" => AriaPropertyType::Boolean { allow_undefined: false },
    "aria-autocomplete" => AriaPropertyType::Token { values: &["inline", "list", "both", "none"] },
    "aria-braillelabel" => AriaPropertyType::String,
    "aria-brailleroledescription" => AriaPropertyType::String,
    "aria-busy" => AriaPropertyType::Boolean { allow_undefined: false },
    "aria-checked" => AriaPropertyType::Tristate,
    "aria-colcount" => AriaPropertyType::Integer,
    "aria-colindex" => AriaPropertyType::Integer,
    "aria-colspan" => AriaPropertyType::Integer,
    "aria-controls" => AriaPropertyType::IdList,
    "aria-current" => AriaPropertyType::Token { values: &["page", "step", "location", "date", "time", "true", "false"] },
    "aria-describedby" => AriaPropertyType::IdList,
    "aria-description" => AriaPropertyType::String,
    "aria-details" => AriaPropertyType::Id,
    "aria-disabled" => AriaPropertyType::Boolean { allow_undefined: false },
    "aria-dropeffect" => AriaPropertyType::TokenList { values: &["none", "copy", "execute", "link", "move", "popup"] },
    "aria-errormessage" => AriaPropertyType::Id,
    "aria-expanded" => AriaPropertyType::Boolean { allow_undefined:true },
    "aria-flowto" => AriaPropertyType::IdList,
    "aria-grabbed" => AriaPropertyType::Boolean { allow_undefined: true },
    "aria-haspopup" => AriaPropertyType::Token { values: &["menu", "listbox", "tree", "grid", "dialog", "true", "false"] },
    "aria-hidden" => AriaPropertyType::Boolean { allow_undefined: true },
    "aria-invalid" => AriaPropertyType::Token { values: &["grammar", "spelling", "true", "false"] },
    "aria-keyshortcuts" => AriaPropertyType::String,
    "aria-label" => AriaPropertyType::String,
    "aria-labelledby" => AriaPropertyType::IdList,
    "aria-level" => AriaPropertyType::Integer,
    "aria-live" => AriaPropertyType::Token { values: &["off", "assertive", "polite"] },
    "aria-modal" => AriaPropertyType::Boolean { allow_undefined: false },
    "aria-multiline" => AriaPropertyType::Boolean { allow_undefined: false },
    "aria-multiselectable" => AriaPropertyType::Boolean { allow_undefined: false },
    "aria-orientation" => AriaPropertyType::Token { values: &["horizontal", "vertical", "undefined"] },
    "aria-owns" => AriaPropertyType::IdList,
    "aria-placeholder" => AriaPropertyType::String,
    "aria-posinset" => AriaPropertyType::Integer,
    "aria-pressed" => AriaPropertyType::Tristate,
    "aria-readonly" => AriaPropertyType::Boolean { allow_undefined: false },
    "aria-relevant" => AriaPropertyType::TokenList { values: &["additions", "removals", "text", "all"] },
    "aria-required" => AriaPropertyType::Boolean { allow_undefined: false },
    "aria-roledescription" => AriaPropertyType::String,
    "aria-rowcount" => AriaPropertyType::Integer,
    "aria-rowindex" => AriaPropertyType::Integer,
    "aria-rowspan" => AriaPropertyType::Integer,
    "aria-selected" => AriaPropertyType::Boolean { allow_undefined: true },
    "aria-setsize" => AriaPropertyType::Integer,
    "aria-sort" => AriaPropertyType::Token { values: &["ascending", "descending", "none", "other"] },
    "aria-valuemax" => AriaPropertyType::Number,
    "aria-valuemin" => AriaPropertyType::Number,
    "aria-valuenow" => AriaPropertyType::Number,
    "aria-valuetext" => AriaPropertyType::String,
};
