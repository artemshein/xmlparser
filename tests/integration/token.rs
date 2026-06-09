type Range = ::std::ops::Range<usize>;

#[derive(PartialEq, Debug)]
pub enum Token<'a> {
    Declaration(&'a str, Option<&'a str>, Option<bool>, Range),
    PI(&'a str, Option<&'a str>, Range),
    Comment(&'a str, Range),
    DtdStart(&'a str, Option<ExternalId<'a>>, Range),
    EmptyDtd(&'a str, Option<ExternalId<'a>>, Range),
    EntityDecl(&'a str, EntityDefinition<'a>, Range),
    DtdEnd(Range),
    ElementStart(&'a str, &'a str, Range),
    Attribute(&'a str, &'a str, &'a str, Range),
    ElementEnd(ElementEnd<'a>, Range),
    Text(&'a str, Range),
    Cdata(&'a str, Range),
    Error(String),
}

#[derive(PartialEq, Debug)]
pub enum ElementEnd<'a> {
    Open,
    Close(&'a str, &'a str),
    Empty,
}

#[derive(PartialEq, Debug)]
pub enum ExternalId<'a> {
    System(&'a str),
    Public(&'a str, &'a str),
}

#[derive(PartialEq, Debug)]
pub enum EntityDefinition<'a> {
    EntityValue(&'a str),
    ExternalId(ExternalId<'a>),
}

#[macro_export]
macro_rules! test {
    ($name:ident, $text:expr, $($token:expr),*) => (
        #[test]
        fn $name() {
            let text = $text;
            let mut p = xml::Tokenizer::from(text);
            $(
                let t = p.next().unwrap();
                assert_eq!(to_test_token(t, text), $token);
            )*
            assert!(p.next().is_none());
        }
    )
}

#[inline(never)]
pub fn to_test_token(token: Result<xml::Token, xml::Error>, text: &str) -> Token<'_> {
    let tok = match token {
        Ok(t) => t,
        Err(ref e) => return Token::Error(e.to_string()),
    };

    // Token spans are validated through range(), which derives the end
    // offset from the last sub-span for variants that do not store it.
    let range = tok.range();

    match tok {
        xml::Token::Declaration {
            start,
            version,
            encoding,
            standalone,
            ..
        } => {
            let start = start as usize;
            Token::Declaration(
                version.as_str(text, start),
                if encoding.is_empty() {
                    None
                } else {
                    Some(encoding.as_str(text, start))
                },
                standalone,
                range,
            )
        }
        xml::Token::ProcessingInstruction {
            start,
            target,
            content,
            ..
        } => {
            let start = start as usize;
            Token::PI(
                target.as_str(text, start),
                if content.is_empty() {
                    None
                } else {
                    Some(content.as_str(text, start))
                },
                range,
            )
        }
        xml::Token::Comment { start, text: text_ } => {
            Token::Comment(text_.as_str(text, start as usize), range)
        }
        xml::Token::DtdStartNoExternalId { start, name, .. } => {
            Token::DtdStart(name.as_str(text, start as usize), None, range)
        }
        xml::Token::DtdStartSystemExternalId {
            start,
            name,
            external_id,
            ..
        } => {
            let start = start as usize;
            Token::DtdStart(
                name.as_str(text, start),
                Some(to_test_external_id(
                    xml::ExternalId::System(external_id),
                    text,
                    start,
                )),
                range,
            )
        }
        xml::Token::DtdStartPublicExternalId {
            start,
            name,
            public1,
            public2,
            ..
        } => {
            let start = start as usize;
            Token::DtdStart(
                name.as_str(text, start),
                Some(to_test_external_id(
                    xml::ExternalId::Public(public1, public2),
                    text,
                    start,
                )),
                range,
            )
        }
        xml::Token::EmptyDtdNoExternalId { start, name, .. } => {
            Token::EmptyDtd(name.as_str(text, start as usize), None, range)
        }
        xml::Token::EmptyDtdSystemExternalId {
            start,
            name,
            external_id,
            ..
        } => {
            let start = start as usize;
            Token::EmptyDtd(
                name.as_str(text, start),
                Some(to_test_external_id(
                    xml::ExternalId::System(external_id),
                    text,
                    start,
                )),
                range,
            )
        }
        xml::Token::EmptyDtdPublicExternalId {
            start,
            name,
            public1,
            public2,
            ..
        } => {
            let start = start as usize;
            Token::EmptyDtd(
                name.as_str(text, start),
                Some(to_test_external_id(
                    xml::ExternalId::Public(public1, public2),
                    text,
                    start,
                )),
                range,
            )
        }
        xml::Token::EntityDeclarationEntityValue {
            start,
            name,
            entity_value,
            ..
        } => {
            let start = start as usize;
            Token::EntityDecl(
                name.as_str(text, start),
                EntityDefinition::EntityValue(entity_value.as_str(text, start)),
                range,
            )
        }
        xml::Token::EntityDeclarationSystemExternalId {
            start,
            name,
            external_id,
            ..
        } => {
            let start = start as usize;
            Token::EntityDecl(
                name.as_str(text, start),
                EntityDefinition::ExternalId(to_test_external_id(
                    xml::ExternalId::System(external_id),
                    text,
                    start,
                )),
                range,
            )
        }
        xml::Token::EntityDeclarationPublicExternalId {
            start,
            name,
            public1,
            public2,
            ..
        } => {
            let start = start as usize;
            Token::EntityDecl(
                name.as_str(text, start),
                EntityDefinition::ExternalId(to_test_external_id(
                    xml::ExternalId::Public(public1, public2),
                    text,
                    start,
                )),
                range,
            )
        }
        xml::Token::DtdEnd { .. } => Token::DtdEnd(range),
        xml::Token::ElementStart {
            start,
            prefix,
            local,
        } => {
            let start = start as usize;
            Token::ElementStart(prefix.as_str(text, start), local.as_str(text, start), range)
        }
        xml::Token::Attribute {
            start,
            prefix,
            local,
            value,
        } => {
            let start = start as usize;
            Token::Attribute(
                prefix.as_str(text, start),
                local.as_str(text, start),
                value.as_str(text, start),
                range,
            )
        }
        xml::Token::ElementEnd { start, el_end, .. } => {
            let start = start as usize;
            Token::ElementEnd(
                match el_end {
                    xml::ElementEnd::Open => ElementEnd::Open,
                    xml::ElementEnd::Close(prefix, local) => {
                        ElementEnd::Close(prefix.as_str(text, start), local.as_str(text, start))
                    }
                    xml::ElementEnd::Empty => ElementEnd::Empty,
                },
                range,
            )
        }
        xml::Token::Text { start, text: text_ } => {
            Token::Text(text_.as_str(text, start as usize), range)
        }
        xml::Token::Cdata { start, text: text_ } => {
            Token::Cdata(text_.as_str(text, start as usize), range)
        }
    }
}

fn to_test_external_id(id: xml::ExternalId, text: &str, start: usize) -> ExternalId<'_> {
    match id {
        xml::ExternalId::System(name) => ExternalId::System(name.as_str(text, start)),
        xml::ExternalId::Public(name, value) => {
            ExternalId::Public(name.as_str(text, start), value.as_str(text, start))
        }
    }
}
