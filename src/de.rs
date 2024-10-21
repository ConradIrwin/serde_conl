use std::borrow::Cow;
use std::fmt::Display;
use std::str::FromStr;

use base64::{engine::general_purpose::STANDARD, Engine as _};
use conl::Token;
use serde::de::{
    self, DeserializeSeed, EnumAccess, IntoDeserializer, MapAccess, SeqAccess, VariantAccess,
    Visitor,
};
use serde::Deserialize;

use crate::error::{Error, Result};

pub struct Deserializer<'de> {
    parser: conl::Parser<'de>,
    is_top_of_file: bool,
    lno: usize,
    is_list_item: bool,
    peek: Vec<conl::Token<'de>>,
}

impl<'de> Deserializer<'de> {
    pub fn from_slice(input: &'de [u8]) -> Self {
        Deserializer {
            is_top_of_file: true,
            is_list_item: false,
            parser: conl::parse(input),
            lno: 1,
            peek: Vec::new(),
        }
    }
}

pub fn from_slice<'a, T>(s: &'a [u8]) -> Result<T>
where
    T: Deserialize<'a>,
{
    let mut deserializer = Deserializer::from_slice(s);
    let t = T::deserialize(&mut deserializer)?;
    match deserializer.get_token()? {
        None => Ok(t),
        Some(tok) => Err(Error::new(
            tok.line_number(),
            format!("unexpected token: {:?}", tok),
        )),
    }
}

pub fn from_str<'a, T>(s: &'a str) -> Result<T>
where
    T: Deserialize<'a>,
{
    from_slice(s.as_bytes())
}

impl<'de> Deserializer<'de> {
    fn read_token(&mut self) -> Result<Option<conl::Token<'de>>> {
        for next in self.parser.by_ref() {
            if matches!(
                next,
                Ok(Token::Newline(..) | Token::Comment(..) | Token::MultilineIndicator(..))
            ) {
                continue;
            }
            let next = next?;
            return Ok(Some(next));
        }
        Ok(None)
    }

    fn unget_token(&mut self, tok: conl::Token<'de>) {
        self.peek.push(tok);
    }

    fn get_token(&mut self) -> Result<Option<conl::Token<'de>>> {
        if let Some(next) = self.peek.pop() {
            self.lno = next.line_number();
            return Ok(Some(next.clone()));
        }
        self.is_top_of_file = false;
        let token = self.read_token()?;
        if let Some(token) = &token {
            self.lno = token.line_number();
            match &token {
                Token::ListItem(..) => self.is_list_item = true,
                Token::MapKey(..) => self.is_list_item = false,
                _ => {}
            }
        }
        Ok(token)
    }

    fn consume_number<T: FromStr, R>(&mut self, f: impl FnOnce(T) -> Result<R>) -> Result<R>
    where
        T::Err: Display,
    {
        let (lno, mut str) = self.consume_value()?;
        if str == "" {
            str = Cow::Borrowed("0")
        };
        let num: T = str.parse().map_err(|_| {
            Error::new(
                lno,
                format!("invalid {}: {:?}", std::any::type_name::<T>(), str),
            )
        })?;
        f(num).map_err(|e| e.set_lno(lno))
    }

    fn consume_value(&mut self) -> Result<(usize, Cow<'de, str>)> {
        match self.get_token()? {
            Some(
                tok @ Token::MapKey(..) | tok @ Token::Value(..) | tok @ Token::MultilineValue(..),
            ) => Ok((tok.line_number(), tok.unescape()?)),
            _ => Err(Error::new(self.lno, "expected value")),
        }
    }

    fn consume_seq_common<V>(&mut self, len: Option<usize>, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let is_top = self.is_top_of_file;

        match self.get_token()? {
            Some(tok @ Token::ListItem(..)) if is_top => {
                self.unget_token(tok);
            }
            Some(Token::Indent(..)) if !is_top => {
                let Some(Token::ListItem(lno)) = self.get_token()? else {
                    return Err(Error::new(self.lno, "expected list"));
                };
                self.unget_token(Token::ListItem(lno));
            }
            _ => {
                return Err(Error::new(self.lno, "expected list"));
            }
        }

        let value = visitor
            .visit_seq(IndentedSection::new(self))
            .map_err(|e| e.set_lno(self.lno))?;

        match self.get_token()? {
            None if is_top => {}
            Some(Token::Outdent(..)) if !is_top => {}
            Some(Token::ListItem(lno)) if len.is_some() => {
                return Err(Error::new(
                    lno,
                    format!("invalid length, expected a tuple of size {}", len.unwrap()),
                ));
            }
            _ => return Err(Error::new(self.lno, "unexpected token")),
        }

        Ok(value)
    }
}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.get_token()? {
            Some(tok @ Token::Value(..) | tok @ Token::MultilineValue(..)) => {
                self.unget_token(tok);
                self.deserialize_string(visitor)
            }
            Some(tok1 @ Token::Indent(_)) => match self.read_token()? {
                Some(tok2 @ Token::ListItem(..)) => {
                    self.unget_token(tok2);
                    self.unget_token(tok1);
                    self.deserialize_seq(visitor)
                }
                Some(tok2 @ Token::MapKey(..)) => {
                    self.unget_token(tok2);
                    self.unget_token(tok1);
                    self.deserialize_map(visitor)
                }
                _ => Err(Error::new(tok1.line_number(), "unexpected section")),
            },
            _ => Err(Error::new(self.lno, "expected value")),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (lno, s) = self.consume_value()?;
        let bool = match &s as &str {
            "true" => true,
            "" | "false" => false,
            _ => return Err(Error::new(lno, format!("invalid bool {:?}", s))),
        };
        visitor.visit_bool(bool).map_err(|e: Error| e.set_lno(lno))
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_i8(n))
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_i16(n))
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_i32(n))
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_i64(n))
    }

    fn deserialize_i128<V>(self, visitor: V) -> std::result::Result<V::Value, Self::Error>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_i128(n))
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        dbg!("deserialize_u8");
        self.consume_number(|n| visitor.visit_u8(n))
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_u16(n))
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_u32(n))
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_u64(n))
    }
    fn deserialize_u128<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_u128(n))
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_f32(n))
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_number(|n| visitor.visit_f64(n))
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (lno, str) = self.consume_value()?;
        if str.len() != 1 {
            return Err(Error::new(lno, "invalid char. expected value of length 1"));
        }
        visitor
            .visit_char(str.chars().next().unwrap())
            .map_err(|e: Error| e.set_lno(lno))
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (lno, value) = self.consume_value()?;
        match value {
            Cow::Borrowed(v) => visitor.visit_borrowed_str(v),
            Cow::Owned(v) => visitor.visit_string(v),
        }
        .map_err(|mut e: Error| {
            e.lno = lno;
            e
        })
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    // The `Serializer` implementation on the previous page serialized byte
    // arrays as JSON arrays of bytes. Handle that representation here.
    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let (lno, str) = self.consume_value()?;
        let str = str.replace(char::is_whitespace, "");
        let bytes = STANDARD
            .decode(str)
            .map_err(|e| Error::new(lno, format!("{}", e)))?;
        visitor
            .visit_byte_buf(bytes)
            .map_err(|e: Error| e.set_lno(lno))
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        // A None is typically represented by ommitting the key.
        // That said, we cannot do so in map keys or in list items,
        // in which case we use the empty string to represent None.
        // This means that roundtripping Some("") in a map key or a list
        // will come back as None.
        // (this is probably not a problem for configuration files)
        match self.get_token()? {
            Some(tok @ Token::MapKey(..)) if tok.unescape()?.is_empty() => {
                return visitor.visit_none()
            }
            Some(tok @ Token::Value(..)) if self.is_list_item && tok.unescape()?.is_empty() => {
                return visitor.visit_none()
            }
            Some(tok) => {
                self.unget_token(tok);
            }
            None => {}
        }

        visitor.visit_some(self)
    }

    // type a = ();
    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.get_token()? {
            Some(tok @ Token::Value(..) | tok @ Token::MultilineValue(..)) => {
                if !tok.unescape()?.is_empty() {
                    return Err(Error::new(self.lno, "expected empty value"));
                }
            }
            Some(Token::Indent(..)) => {
                if !matches!(self.get_token()?, Some(Token::Outdent(..))) {
                    return Err(Error::new(self.lno, "expected empty value"));
                }
            }
            _ => {}
        }
        visitor.visit_unit().map_err(|e: Error| e.set_lno(self.lno))
    }

    // struct A;
    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
            .map_err(|e| e.set_lno(self.lno))
    }

    // struct A(inner) is serialized as inner
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    // vec![a, b, c] is serialized as a conl list
    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_seq_common(None, visitor)
    }

    // (a,b,c) is serialized as a conl list
    fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_seq_common(Some(len), visitor)
    }

    // struct A(a,b,c) is serialized as (a,b,c)
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.consume_seq_common(Some(len), visitor)
    }

    // HashMap<X,Y> is serialized as a conl map
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let is_top = self.is_top_of_file;
        match self.get_token()? {
            Some(tok @ Token::MapKey(..)) if is_top => {
                self.unget_token(tok);
            }
            None if is_top => {}
            Some(Token::Indent(..)) if !is_top => {
                let Some(Token::MapKey(lno, key)) = self.get_token()? else {
                    return Err(Error::new(self.lno, "expected list"));
                };
                self.unget_token(Token::MapKey(lno, key));
            }
            _ => {
                return Err(Error::new(self.lno, "expected map"));
            }
        }

        let value = visitor
            .visit_map(IndentedSection::new(self))
            .map_err(|e| e.set_lno(self.lno))?;

        match self.get_token()? {
            None if is_top => {}
            Some(Token::Outdent(..)) if !is_top => {}
            _ => return Err(Error::new(self.lno, "unexpected token")),
        }

        Ok(value)
    }

    // a struct is serialized as a conl map
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    /// There are four forms of enum to care about:
    /// enum {
    ///   A            // "unit variant"
    ///   B(u8)        // "newtype variant"
    ///   C(u8, u8)    // "tuple variant"
    ///   D {a: u8}    // "struct variant"
    /// }
    ///
    /// in conl:
    ///   key = a
    ///   key
    ///     b = 1
    ///   key
    ///     c
    ///       = 1
    ///       = 2
    ///   key
    ///     d
    ///       a = 1
    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        if self.is_top_of_file {
            let Some(Token::MapKey(key, lno)) = self.get_token()? else {
                return Err(Error::new(self.lno, "expected map"));
            };
            self.unget_token(Token::MapKey(key, lno));

            return visitor
                .visit_enum(IndentedSection::new(self))
                .map_err(|e| e.set_lno(self.lno));
        }

        // "unit variant"
        let next = self.get_token()?;
        match next {
            Some(tok @ Token::Value(..) | tok @ Token::MultilineValue(..)) => {
                return visitor
                    .visit_enum(tok.unescape()?.into_deserializer())
                    .map_err(|e: Error| e.set_lno(self.lno));
            }
            Some(Token::Indent(..)) => {
                let Some(Token::MapKey(key, lno)) = self.get_token()? else {
                    return Err(Error::new(self.lno, "expected map"));
                };
                self.unget_token(Token::MapKey(key, lno));
                let value = visitor
                    .visit_enum(IndentedSection::new(self))
                    .map_err(|e| e.set_lno(self.lno))?;

                let Some(Token::Outdent(..)) = self.get_token()? else {
                    return Err(Error::new(self.lno, "expected end of map"));
                };

                Ok(value)
            }
            _ => Err(Error::new(self.lno, "expected map")),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

struct IndentedSection<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
}

impl<'a, 'de> IndentedSection<'a, 'de> {
    fn new(de: &'a mut Deserializer<'de>) -> Self {
        IndentedSection { de }
    }
}

impl<'de, 'a> SeqAccess<'de> for IndentedSection<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        match self.de.get_token()? {
            Some(Token::ListItem(..)) => seed.deserialize(&mut *self.de).map(Some),
            Some(tok) => {
                self.de.unget_token(tok);
                Ok(None)
            }
            None => Ok(None),
        }
    }
}

impl<'de, 'a> MapAccess<'de> for IndentedSection<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        match self.de.get_token()? {
            Some(tok @ Token::MapKey(..)) => {
                self.de.unget_token(tok);
                seed.deserialize(&mut *self.de).map(Some)
            }
            Some(tok) => {
                self.de.unget_token(tok);
                Ok(None)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        // Deserialize a map value.
        seed.deserialize(&mut *self.de)
    }
}

impl<'de, 'a> EnumAccess<'de> for IndentedSection<'a, 'de> {
    type Error = Error;
    type Variant = Self;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
    where
        V: DeserializeSeed<'de>,
    {
        let val = seed.deserialize(&mut *self.de)?;

        Ok((val, self))
    }
}

impl<'de, 'a> VariantAccess<'de> for IndentedSection<'a, 'de> {
    type Error = Error;

    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
    where
        T: DeserializeSeed<'de>,
    {
        seed.deserialize(self.de)
    }

    fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_seq(self.de, visitor)
    }

    fn struct_variant<V>(self, _fields: &'static [&'static str], visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        de::Deserializer::deserialize_map(self.de, visitor)
    }
}

#[cfg(test)]
mod test {
    use indoc::indoc;
    use serde::Deserialize;
    use std::collections::HashMap;

    use crate::from_str;

    #[test]
    fn test_de() {
        assert_eq!(
            HashMap::<String, String>::new(),
            from_str("# test").unwrap()
        );

        assert_eq!(HashMap::from([("a", "a")]), from_str("a = a").unwrap());
        assert_eq!(
            HashMap::from([("a".to_string(), "a".to_string())]),
            from_str("a = a").unwrap()
        );

        assert_eq!(
            HashMap::from([("=ö".to_string(), "\"# \t\r\n".to_string())]),
            from_str(&r##""="{f6} = """#"_">"\"/"##).unwrap()
        );

        assert_eq!(vec!["b"], from_str::<Vec<String>>("= b").unwrap());
        assert_eq!(vec!["b", "c"], from_str::<Vec<String>>("= b\n= c").unwrap());

        assert_eq!(
            HashMap::from([("a", vec!["a", "b"])]),
            from_str(indoc! {r#"
                a
                  = a
                  = b
            "#})
            .unwrap(),
        );
        assert_eq!(
            HashMap::from([("a", HashMap::from([("b", "1"), ("d", "c")]))]),
            from_str(indoc! {r#"
                a
                  b = 1
                  d = c
            "#})
            .unwrap(),
        );

        assert_eq!(
            HashMap::from([("a", HashMap::from([("b", "1"), ("d", "c")]))]),
            from_str(indoc! {r#"
                a =

                  b = 1
                  # test
                  d = c
            "#})
            .unwrap(),
        );

        assert_eq!(
            HashMap::from([("a", "#!/usr/bin/env bash\necho `whoami`".to_string())]),
            from_str(indoc! {r#"
                a = """
                  #!/usr/bin/env bash
                  echo `whoami`
                "#})
            .unwrap(),
        );

        assert_eq!(
            HashMap::from([
                ("a", "#!/usr/bin/env bash\n\n echo `whoami`".to_string()),
                ("b", "5".to_string())
            ]),
            from_str(indoc! {r#"
                a = """bash
                  #!/usr/bin/env bash

                   echo `whoami`

                b = 5
                "#})
            .unwrap(),
        );

        #[derive(Default, Debug, PartialEq, Deserialize)]
        #[serde(default)]
        struct Test {
            a: String,
            b: Option<bool>,
            c: bool,
            d: f32,
            e: Tiny,
            f: Vec<u64>,
            g: HashMap<String, String>,
        }

        #[derive(Debug, PartialEq, Deserialize, Default)]
        struct Tiny {
            a: bool,
        }

        assert_eq!(
            Test {
                a: "".into(),
                b: None,
                c: false,
                d: 0.,
                e: Tiny { a: false },
                f: vec![],
                g: HashMap::default(),
            },
            from_str("all_gone = nothing to see").unwrap()
        );

        assert_eq!(
            Test {
                a: " string#".into(),
                b: Some(true),
                c: false,
                d: 1.5,
                e: Tiny { a: true },
                f: vec![1, 2],
                g: HashMap::from([("a".into(), "a".into())])
            },
            from_str(indoc! {r#"
                a = "@ string# #comment
                b = true
                c = """ # comment?
                  false
                d = 1.5
                e
                  a = true
                f
                  = 1
                  = 2
                g =# comment
                 a = a
                 "#})
            .unwrap()
        )
    }

    #[test]
    fn test_errors() {
        assert_eq!(
            from_str::<(String, String)>("= b").unwrap_err(),
            crate::Error {
                lno: 1,
                msg: "invalid length 1, expected a tuple of size 2".to_string(),
            }
        );

        assert_eq!(
            from_str::<Vec<u8>>("= 1\n= 2\n= -5").unwrap_err(),
            crate::Error {
                lno: 3,
                msg: "invalid u8: \"-5\"".to_string(),
            }
        );
        #[derive(Deserialize, Debug, PartialEq, Default)]
        #[serde(default, deny_unknown_fields)]
        struct A {
            b: bool,
        }
        assert_eq!(
            from_str::<A>("# b = false\nc = true").unwrap_err(),
            crate::Error {
                lno: 2,
                msg: "unknown field `c`, expected `b`".to_string(),
            }
        );
    }
}
