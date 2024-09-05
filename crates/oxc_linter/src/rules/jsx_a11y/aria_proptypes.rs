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

fn aria_proptypes_diagnostic(
    prop_name: &str,
    prop_type: &AriaPropertyType,
    span: Span,
) -> OxcDiagnostic {
    let message = match prop_type {
        AriaPropertyType::String => format!("The value for {} must be a string.", prop_name),
        AriaPropertyType::Id => {
            format!("The value for {} must be a string that represents a DOM element ID", prop_name)
        }
        AriaPropertyType::IdList => format!(
            "The value for {} must be a list of strings that represent DOM element IDs",
            prop_name
        ),
        AriaPropertyType::Integer => format!("The value for {} must be an integer.", prop_name),
        AriaPropertyType::Boolean { .. } => {
            format!("The value for {} must be a boolean.", prop_name)
        }
        AriaPropertyType::Number => format!("The value for {} must be a number.", prop_name),
        AriaPropertyType::Token { values } => format!(
            "The value for {} must be a single token from the following: {}",
            prop_name,
            values.join(", ")
        ),
        AriaPropertyType::TokenList { values } => format!(
            "The value for {} must be a list of one or more tokens from the following: {}",
            prop_name,
            values.join(", ")
        ),
        AriaPropertyType::Tristate => {
            format!("The value for {} must be a boolean or the string \"mixed\".", prop_name)
        }
    };
    OxcDiagnostic::warn(message).with_label(span)
}

#[derive(Debug, Default, Clone)]
pub struct AriaProptypes;

declare_oxc_lint!(
    /// ### What it does
    ///
    /// Enforces that values of ARIA attributes are valid.
    ///
    /// ### Why is this bad?
    ///
    /// ARIA attributes must conform to certain values, depending on their type, in order to be understood correctly by assistive technologies.
    ///
    /// ### Examples
    ///
    /// Examples of **incorrect** code for this rule:
    /// ```jsx
    /// <div aria-hidden="yes" />
    /// <div aria-level={true} />
    /// <div aria-sort="ascending descending" />
    /// <div aria-valuemax />
    /// <div aria-label={1234} />
    /// ```
    ///
    /// Examples of **correct** code for this rule:
    /// ```jsx
    /// <div aria-hidden="true" />
    /// <div aria-level={2} />
    /// <div aria-sort="ascending" />
    /// <div aria-valuemax={100} />
    /// <div aria-label="Close" />
    /// ```
    AriaProptypes,
    correctness
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
            let attr_name = &get_jsx_attribute_name(&attr.name);
            let attr_value = attr.value.as_ref();
            if VALID_ARIA_PROPS.contains(attr_name) {
                // Ignore attribute if it's not a valid ARIA property
                let Some(prop_type) = ARIA_PROPERTY_TYPES.get(attr_name) else {
                    // Unless we have an explicit type, we should assume it's valid to prevent false positives
                    continue;
                };

                if !is_aria_prop_valid(prop_type, attr_value) {
                    ctx.diagnostic(aria_proptypes_diagnostic(attr_name, prop_type, attr.span));
                    return;
                }
            }
        }
    }
}

/// Given an ARIA property type and its value, determine if the value is valid
fn is_aria_prop_valid(
    prop_type: &AriaPropertyType,
    prop_value: Option<&JSXAttributeValue>,
) -> bool {
    if let Some(JSXAttributeValue::ExpressionContainer(expr)) = prop_value {
        match &expr.expression {
            // Ignore the attribute if its prop value is null or undefined
            JSXExpression::NullLiteral(_) => return true,
            JSXExpression::Identifier(ident) if ident.name == "undefined" => return true,
            // Generally allow identifiers and member expressions
            JSXExpression::Identifier(_) | JSXExpression::StaticMemberExpression(_) => return true,
            _ => {}
        }
    }

    // If we don't recognize the syntax specifically, we will assume it's valid to try and prevent false positives.
    let assume_valid = true;

    // TODO: Improve the resolution of static values so that we can type-check more complex expressions. We should
    // ultimately have a way of trying to parse a boolean/string/number from a given AST node.
    // For example, we currently assume this is valid, but we could check it: `<div aria-invalid={true ? 0 : -1}>` just
    // based on the fact that neither of the two branches are possibly "true" or "false".

    dbg!(prop_type, prop_value);
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
                JSXExpression::ConditionalExpression(_) => true,
                _ => true,
            },
            None => *allow_undefined,
            _ => assume_valid,
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
            None => false,
            _ => assume_valid,
        },
        AriaPropertyType::Id => match prop_value {
            Some(JSXAttributeValue::StringLiteral(_)) => true,
            Some(JSXAttributeValue::ExpressionContainer(expr)) => match &expr.expression {
                JSXExpression::StringLiteral(_) | JSXExpression::TemplateLiteral(_) => true,
                JSXExpression::NumericLiteral(_) => false,
                JSXExpression::BooleanLiteral(_) => false,
                _ => true,
            },
            _ => assume_valid,
        },
        AriaPropertyType::IdList => match prop_value {
            Some(JSXAttributeValue::StringLiteral(_)) => true,
            Some(JSXAttributeValue::ExpressionContainer(expr)) => match &expr.expression {
                JSXExpression::StringLiteral(_) | JSXExpression::TemplateLiteral(_) => true,
                JSXExpression::NumericLiteral(_) => false,
                JSXExpression::BooleanLiteral(_) => false,
                _ => true,
            },
            _ => assume_valid,
        },
        AriaPropertyType::Integer => {
            let Some(prop_value) = prop_value else {
                return false;
            };
            let is_integer_value =
                parse_jsx_value(prop_value).map_or(false, |num| num.fract() == 0.0);
            match prop_value {
                JSXAttributeValue::StringLiteral(_) => is_integer_value,
                JSXAttributeValue::ExpressionContainer(_) => is_integer_value,
                _ => assume_valid,
            }
        }
        AriaPropertyType::Number => {
            let Some(prop_value) = prop_value else {
                return false;
            };
            let Ok(parsed_value) = parse_jsx_value(prop_value) else {
                return false;
            };
            parsed_value.is_finite()
        }
        AriaPropertyType::Token { values } => match prop_value {
            Some(JSXAttributeValue::StringLiteral(value)) => {
                is_token_value(&value.value, prop_type)
            }
            Some(JSXAttributeValue::ExpressionContainer(expr)) => match &expr.expression {
                JSXExpression::StringLiteral(str_lit) => is_token_value(&str_lit.value, prop_type),
                JSXExpression::TemplateLiteral(template) => {
                    template.expressions.is_empty()
                        && is_token_value(&template.quasis[0].value.raw, prop_type)
                }
                JSXExpression::BooleanLiteral(_)
                | JSXExpression::UnaryExpression(_)
                | JSXExpression::LogicalExpression(_) => {
                    values.contains(&"true") && values.contains(&"false")
                }
                JSXExpression::NumericLiteral(_) => false,
                _ => assume_valid,
            },
            None => false,
            _ => assume_valid,
        },
        AriaPropertyType::TokenList { .. } => match prop_value {
            Some(JSXAttributeValue::StringLiteral(value)) => {
                is_token_list_value(&value.value, prop_type)
            }
            Some(JSXAttributeValue::ExpressionContainer(expr)) => match &expr.expression {
                JSXExpression::StringLiteral(str_lit) => {
                    is_token_list_value(&str_lit.value, prop_type)
                }
                JSXExpression::TemplateLiteral(template) => {
                    template.expressions.is_empty()
                        && is_token_list_value(&template.quasis[0].value.raw, prop_type)
                }
                JSXExpression::BooleanLiteral(_) => false,
                _ => assume_valid,
            },
            None => false,
            _ => assume_valid,
        },
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
                _ => assume_valid,
            },
            None => true,
            _ => assume_valid,
        },
    }
}

fn is_boolean_value(value: &str) -> bool {
    value == "true" || value == "false"
}

fn is_tristate_value(value: &str) -> bool {
    is_boolean_value(value) || value == "mixed"
}

fn is_token_value(value: &str, prop_type: &AriaPropertyType) -> bool {
    if value.is_empty() {
        return false;
    }
    match prop_type {
        // CHeck if the value is in the list of valid tokens, case insensitive
        AriaPropertyType::Token { values } | AriaPropertyType::TokenList { values } => {
            values.iter().any(|v| v.eq_ignore_ascii_case(value))
        }
        _ => false,
    }
}

fn is_token_list_value(value: &str, prop_type: &AriaPropertyType) -> bool {
    if value.is_empty() {
        return false;
    }
    match prop_type {
        // Check if all values in the list are in the list of valid tokens, case insensitive
        AriaPropertyType::TokenList { .. } => {
            value.split_whitespace().all(|v| is_token_value(v, prop_type))
        }
        _ => false,
    }
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
        "<div aria-valuemax={123.123} />",
        r#"<div aria-valuemax="123.123" />"#,
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
        // Test for each attribute
        r#"<div aria-activedescendant={true} />"#,
        r#"<div aria-atomic="yes" />"#,
        r#"<div aria-autocomplete="no" />"#,
        r#"<div aria-busy="yes" />"#,
        r#"<div aria-checked="no" />"#,
        r#"<div aria-colcount={1.3} />"#,
        r#"<div aria-colindex={1.3} />"#,
        r#"<div aria-colspan={1.3} />"#,
        r#"<div aria-controls={false} />"#,
        r#"<div aria-current="yes" />"#,
        r#"<div aria-current={1.23} />"#,
        r#"<div aria-describedby={true} />"#,
        r#"<div aria-description={1} />"#,
        r#"<div aria-details={1} />"#,
        r#"<div aria-disabled={"yes"} />"#,
        r#"<div aria-dropeffect="none copy test 123" />"#,
        r#"<div aria-errormessage="this is not an id" />"#,
        r#"<div aria-expanded={!"true"} />"#,
        r#"<div aria-flowto={1} />"#,
        r#"<div aria-grabbed={1} />"#,
        r#"<div aria-haspopup={1} />"#,
        r#"<div aria-haspopup="menu listbox" />"#,
        r#"<div aria-hidden={1} />"#,
        r#"<div aria-invalid="yeah" />"#,
        r#"<div aria-invalid={0} />"#,
        r#"<div aria-keyshortcuts={1} />"#,
        r#"<div aria-label={1} />"#,
        r#"<div aria-labelledby={1} />"#,
        r#"<div aria-level={1.3} />"#,
        r#"<div aria-live="assertive polite" />"#,
        r#"<div aria-live={!!true} />"#,
        r#"<div aria-modal="yes" />"#,
        r#"<div aria-multiline="yes" />"#,
        r#"<div aria-multiselectable="yes" />"#,
        r#"<div aria-orientation="yes" />"#,
        r#"<div aria-orientation="horizontal vertical" />"#,
        r#"<div aria-owns={1} />"#,
        r#"<div aria-placeholder={1} />"#,
        r#"<div aria-placeholder={false} />"#,
        r#"<div aria-posinset={1.3} />"#,
        r#"<div aria-pressed="yes" />"#,
        r#"<div aria-readonly="yes" />"#,
        r#"<div aria-relevant="test" />"#,
        r#"<div aria-relevant={123} />"#,
        r#"<div aria-required="yes" />"#,
        r#"<div aria-roledescription={1} />"#,
        r#"<div aria-rowcount={1.3} />"#,
        r#"<div aria-rowindex={1.3} />"#,
        r#"<div aria-rowspan={1.3} />"#,
        r#"<div aria-selected="yes" />"#,
        r#"<div aria-setsize={1.3} />"#,
        r#"<div aria-sort="test" />"#,
        r#"<div aria-valuemax={!0} />"#,
        r#"<div aria-valuemin={!0)} />"#,
        r#"<div aria-valuenow={!0} />"#,
        r#"<div aria-valuetext={99} />"#,
    ];

    Tester::new(AriaProptypes::NAME, pass, fail).test_and_snapshot();
}
