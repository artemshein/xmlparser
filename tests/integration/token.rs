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
pub fn to_test_token(token: Result<xml::Token, xml::Error>, text: &str) -> Token {
    match token {
        Ok(xml::Token::Declaration {
            start,
            version,
            encoding,
            standalone,
            end,
        }) => Token::Declaration(
            version.as_str(text, start),
            if encoding.is_empty() {
                None
            } else {
                Some(encoding.as_str(text, start))
            },
            standalone,
            start..start + end as usize,
        ),
        Ok(xml::Token::ProcessingInstruction {
            start,
            end,
            target,
            content,
        }) => Token::PI(
            target.as_str(text, start),
            if content.is_empty() {
                None
            } else {
                Some(content.as_str(text, start))
            },
            start..start + end as usize,
        ),
        Ok(xml::Token::Comment {
            start,
            end,
            text: text_,
        }) => Token::Comment(text_.as_str(text, start), start..start + end as usize),
        Ok(xml::Token::DtdStartNoExternalId { start, end, name }) => {
            Token::DtdStart(name.as_str(text, start), None, start..start + end as usize)
        }
        Ok(xml::Token::DtdStartSystemExternalId {
            start,
            end,
            name,
            external_id,
        }) => Token::DtdStart(
            name.as_str(text, start),
            Some(to_test_external_id(
                xml::ExternalId::System(external_id),
                text,
                start,
            )),
            start..start + end as usize,
        ),
        Ok(xml::Token::DtdStartPublicExternalId {
            start,
            end,
            name,
            public1,
            public2,
        }) => Token::DtdStart(
            name.as_str(text, start),
            Some(to_test_external_id(
                xml::ExternalId::Public(public1, public2),
                text,
                start,
            )),
            start..start + end as usize,
        ),
        Ok(xml::Token::EmptyDtdNoExternalId { start, end, name }) => {
            Token::EmptyDtd(name.as_str(text, start), None, start..start + end as usize)
        }
        Ok(xml::Token::EmptyDtdSystemExternalId {
            start,
            end,
            name,
            external_id,
        }) => Token::EmptyDtd(
            name.as_str(text, start),
            Some(to_test_external_id(
                xml::ExternalId::System(external_id),
                text,
                start,
            )),
            start..start + end as usize,
        ),
        Ok(xml::Token::EmptyDtdPublicExternalId {
            start,
            end,
            name,
            public1,
            public2,
        }) => Token::EmptyDtd(
            name.as_str(text, start),
            Some(to_test_external_id(
                xml::ExternalId::Public(public1, public2),
                text,
                start,
            )),
            start..start + end as usize,
        ),
        Ok(xml::Token::EntityDeclarationEntityValue {
            start,
            end,
            name,
            entity_value,
        }) => Token::EntityDecl(
            name.as_str(text, start),
            EntityDefinition::EntityValue(entity_value.as_str(text, start)),
            start..start + end as usize,
        ),
        Ok(xml::Token::EntityDeclarationSystemExternalId {
            start,
            end,
            name,
            external_id,
        }) => Token::EntityDecl(
            name.as_str(text, start),
            EntityDefinition::ExternalId(to_test_external_id(
                xml::ExternalId::System(external_id),
                text,
                start,
            )),
            start..start + end as usize,
        ),
        Ok(xml::Token::EntityDeclarationPublicExternalId {
            start,
            end,
            name,
            public1,
            public2,
        }) => Token::EntityDecl(
            name.as_str(text, start),
            EntityDefinition::ExternalId(to_test_external_id(
                xml::ExternalId::Public(public1, public2),
                text,
                start,
            )),
            start..start + end as usize,
        ),
        Ok(xml::Token::DtdEnd { start, end }) => Token::DtdEnd(start..start + end as usize),
        Ok(xml::Token::ElementStart {
            start,
            end,
            prefix,
            local,
        }) => Token::ElementStart(
            prefix.as_str(text, start),
            local.as_str(text, start),
            start..start + end as usize,
        ),
        Ok(xml::Token::Attribute {
            start,
            end,
            prefix,
            local,
            value,
        }) => Token::Attribute(
            prefix.as_str(text, start),
            local.as_str(text, start),
            value.as_str(text, start),
            start..start + end as usize,
        ),
        Ok(xml::Token::ElementEnd { start, end, el_end }) => Token::ElementEnd(
            match el_end {
                xml::ElementEnd::Open => ElementEnd::Open,
                xml::ElementEnd::Close(prefix, local) => {
                    ElementEnd::Close(prefix.as_str(text, 0), local.as_str(text, 0))
                }
                xml::ElementEnd::Empty => ElementEnd::Empty,
            },
            start..start + end as usize,
        ),
        Ok(xml::Token::Text {
            start,
            end,
            text: text_,
        }) => Token::Text(text_.as_str(text, start), start..start + end as usize),
        Ok(xml::Token::Cdata {
            start,
            end,
            text: text_,
        }) => Token::Cdata(text_.as_str(text, start), start..start + end as usize),
        Err(ref e) => Token::Error(e.to_string()),
    }
}

fn to_test_external_id(id: xml::ExternalId, text: &str, start: usize) -> ExternalId {
    match id {
        xml::ExternalId::System(name) => ExternalId::System(name.as_str(text, start)),
        xml::ExternalId::Public(name, value) => {
            ExternalId::Public(name.as_str(text, start), value.as_str(text, start))
        }
    }
}
