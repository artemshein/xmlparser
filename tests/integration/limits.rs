// Tokens whose compact span representation cannot address their length
// must produce an error instead of silently truncating offsets.

fn tokens(text: &str) -> Vec<Result<xml::Token, xml::Error>> {
    xml::Tokenizer::from(text).collect()
}

#[test]
fn attribute_over_64k_is_an_error() {
    let xml = format!("<r a=\"{}\"/>", "x".repeat(70_000));
    let last = tokens(&xml).pop().unwrap();
    assert!(last.is_err(), "expected an error, got {:?}", last);
}

#[test]
fn attribute_under_64k_roundtrips() {
    let value = "x".repeat(65_000);
    let xml = format!("<r a=\"{}\"/>", value);
    let attr = tokens(&xml)
        .into_iter()
        .find_map(|t| match t {
            Ok(xml::Token::Attribute { start, value, .. }) => Some((start, value)),
            _ => None,
        })
        .unwrap();
    assert_eq!(attr.1.as_str(&xml, attr.0), value);
}

#[test]
fn element_name_over_64k_is_an_error() {
    let xml = format!("<{0}></{0}>", "x".repeat(70_000));
    let first = tokens(&xml).into_iter().next().unwrap();
    assert!(first.is_err(), "expected an error, got {:?}", first);
}

#[test]
fn close_element_name_over_64k_is_an_error() {
    let xml = format!("<r></{}>", "x".repeat(70_000));
    let last = tokens(&xml).pop().unwrap();
    assert!(last.is_err(), "expected an error, got {:?}", last);
}

#[test]
fn pi_content_over_64k_is_an_error() {
    let xml = format!("<?pi {}?><r/>", "x".repeat(70_000));
    let first = tokens(&xml).into_iter().next().unwrap();
    assert!(first.is_err(), "expected an error, got {:?}", first);
}

#[test]
fn entity_value_over_64k_is_an_error() {
    let xml = format!(
        "<!DOCTYPE d [<!ENTITY e \"{}\">]><r/>",
        "x".repeat(70_000)
    );
    let has_err = tokens(&xml).into_iter().any(|t| t.is_err());
    assert!(has_err);
}

#[test]
fn text_over_64k_roundtrips() {
    // Text spans are u32-based, so 64 KiB+ must still work.
    let body = "y".repeat(200_000);
    let xml = format!("<r>{}</r>", body);
    let text = tokens(&xml)
        .into_iter()
        .find_map(|t| match t {
            Ok(xml::Token::Text { start, text, .. }) => Some((start, text)),
            _ => None,
        })
        .unwrap();
    assert_eq!(text.1.as_str(&xml, text.0), body);
}

#[test]
fn comment_over_64k_roundtrips() {
    let body = "y".repeat(200_000);
    let xml = format!("<!--{}--><r/>", body);
    let comment = tokens(&xml)
        .into_iter()
        .find_map(|t| match t {
            Ok(xml::Token::Comment { start, text, .. }) => Some((start, text)),
            _ => None,
        })
        .unwrap();
    assert_eq!(comment.1.as_str(&xml, comment.0), body);
}

#[test]
fn cdata_over_64k_roundtrips() {
    let body = "y".repeat(200_000);
    let xml = format!("<r><![CDATA[{}]]></r>", body);
    let cdata = tokens(&xml)
        .into_iter()
        .find_map(|t| match t {
            Ok(xml::Token::Cdata { start, text, .. }) => Some((start, text)),
            _ => None,
        })
        .unwrap();
    assert_eq!(cdata.1.as_str(&xml, cdata.0), body);
}
