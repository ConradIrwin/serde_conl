use serde::{ser, Serialize};

use crate::error::{Error, Result};
use base64::{engine::general_purpose::STANDARD, Engine as _};

#[derive(Debug)]
enum Ctx {
    List(String),
    MapKey(String),
    MapValue(String, String),
}

pub struct Serializer {
    output: String,
    stack: Vec<Ctx>,
}

fn encode_string(s: &str, key: bool, indent: &str, output: &mut String) {
    let must_quote = s.len() == 0
        || s.chars().next().unwrap().is_whitespace()
        || s.chars().last().unwrap().is_whitespace()
        || s.chars().any(|c| c.is_control() && c != '\n');

    let is_multiline = s.chars().any(|c| c == '\n');

    if !must_quote && !key && is_multiline {
        let indent = indent.to_owned() + "  ";
        output.push_str("\"\"\"\n");
        for line in s.lines() {
            output.push_str(&indent);
            output.push_str(line);
            output.push('\n')
        }
        return;
    }

    if must_quote || is_multiline || matches!(s.chars().next(), Some('#' | '"' | '=')) {
        output.push_str("\"");
        for c in s.chars() {
            match c {
                '"' => output.push_str("\\\""),
                '\\' => output.push_str("\\\\"),
                '\r' => output.push_str("\\r"),
                '\n' => output.push_str("\\n"),
                '\t' => output.push_str("\\t"),
                _ if c.is_control() => output.push_str(&format!("\\{{{:02X}}}", c as u32)),
                _ => output.push(c),
            }
        }
        output.push_str("\"");
    } else {
        output.push_str(s)
    }

    if !key {
        output.push('\n')
    }
}

pub fn to_string<T>(value: &T) -> Result<String>
where
    T: Serialize,
{
    let mut serializer = Serializer {
        output: String::new(),
        stack: Vec::new(),
    };
    value.serialize(&mut serializer)?;
    Ok(serializer.output)
}

impl<'a> ser::Serializer for &'a mut Serializer {
    type Ok = ();

    // The error type when some error occurs during serialization.
    type Error = Error;

    // Associated types for keeping track of additional state while serializing
    // compound data structures like sequences and maps. In this case no
    // additional state is required beyond what is already stored in the
    // Serializer struct.
    type SerializeSeq = Self;
    type SerializeTuple = Self;
    type SerializeTupleStruct = Self;
    type SerializeTupleVariant = Self;
    type SerializeMap = Self;
    type SerializeStruct = Self;
    type SerializeStructVariant = Self;

    // Here we go with the simple methods. The following 12 methods receive one
    // of the primitive types of the data model and map it to JSON by appending
    // into the output string.
    fn serialize_bool(self, v: bool) -> Result<()> {
        self.serialize_str(if v { "true" } else { "false" })
    }

    fn serialize_i8(self, v: i8) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_i16(self, v: i16) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_i32(self, v: i32) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_i64(self, v: i64) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_i128(self, v: i128) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u8(self, v: u8) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u16(self, v: u16) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u32(self, v: u32) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u64(self, v: u64) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_u128(self, v: u128) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_f32(self, v: f32) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_f64(self, v: f64) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_char(self, v: char) -> Result<()> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<()> {
        match self.stack.pop() {
            None => {
                Err(Error {
                    lno: 0,
                    msg: "top level value must be a map or a list".to_string(),
                })?;
            }
            Some(Ctx::List(indent)) => {
                self.output.push_str(&indent);
                self.output.push_str("= ");
                encode_string(v, false, &indent, &mut self.output);
                self.stack.push(Ctx::List(indent))
            }
            Some(Ctx::MapKey(indent)) => self.stack.push(Ctx::MapValue(indent, v.to_string())),
            Some(Ctx::MapValue(indent, key)) => {
                self.output.push_str(&indent);
                encode_string(&key, true, &indent, &mut self.output);
                self.output.push_str(" = ");
                encode_string(v, false, &indent, &mut self.output);
                self.stack.push(Ctx::MapKey(indent))
            }
        }
        Ok(())
    }

    fn serialize_bytes(self, mut v: &[u8]) -> Result<()> {
        let mut output = String::new();
        loop {
            let len = 60.min(v.len());
            STANDARD.encode_string(&v[..len], &mut output);
            v = &v[len..];
            if v.is_empty() {
                break;
            }
            output.push('\n')
        }
        self.serialize_str(&output)
    }

    fn serialize_none(self) -> Result<()> {
        match self.stack.pop() {
            None => {
                return Err(Error {
                    lno: 0,
                    msg: "top level value must be a map or a list".to_string(),
                });
            }
            Some(Ctx::List(indent)) => {
                self.output.push_str(&indent);
                self.output.push_str("= # none\n");
                self.stack.push(Ctx::List(indent));
            }
            Some(Ctx::MapKey(_)) => {
                return Err(Error {
                    lno: 0,
                    msg: "cannot serialize none as a map key".to_string(),
                });
            }
            Some(Ctx::MapValue(indent, _)) => {
                self.stack.push(Ctx::MapKey(indent.to_owned()));
            }
        }
        Ok(())
    }

    fn serialize_some<T>(self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<()> {
        self.serialize_none()
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<()> {
        self.serialize_none()
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<()> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        let this = self.serialize_map(Some(1))?;
        variant.serialize(&mut *this)?;
        value.serialize(&mut *this)?;

        this.stack.pop();
        Ok(())
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq> {
        match self.stack.pop() {
            None => self.stack.push(Ctx::List("".to_string())),
            Some(Ctx::MapKey(_)) => {
                return Err(Error {
                    lno: 0,
                    msg: "cannot use lists as map keys".to_string(),
                })?
            }
            Some(Ctx::List(indent)) => {
                self.output.push_str(&indent);
                self.output.push_str("=\n");
                self.stack.push(Ctx::List(indent.clone()));
                if len.is_none() || len == Some(0) {
                    self.output.push_str(&indent);
                    self.output.push_str("  #\n")
                }
                self.stack.push(Ctx::List(indent + "  "));
            }
            Some(Ctx::MapValue(indent, key)) => {
                if len != Some(0) {
                    self.output.push_str(&indent);
                    encode_string(&key, true, &indent, &mut self.output);
                    self.output.push('\n');
                    if len.is_none() {
                        self.output.push_str(&indent);
                        self.output.push_str("  #\n")
                    }
                }
                self.stack.push(Ctx::MapKey(indent.clone()));
                self.stack.push(Ctx::List(indent + "  "));
            }
        }
        Ok(self)
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        match self.stack.pop() {
            None => {
                encode_string(variant, true, "", &mut self.output);
                self.output.push('\n');
                self.stack.push(Ctx::List("  ".to_string()));
            }
            Some(Ctx::MapKey(_)) => {
                return Err(Error {
                    lno: 0,
                    msg: "cannot use lists as map keys".to_string(),
                })?
            }
            Some(Ctx::List(indent)) => {
                self.output.push_str(&indent);
                self.output.push_str("=\n");
                self.output.push_str(&indent);
                self.output.push_str("  ");
                encode_string(variant, true, &indent, &mut self.output);
                self.output.push('\n');
                self.stack.push(Ctx::List(indent.clone()));
                self.stack.push(Ctx::List(indent + "    "));
            }
            Some(Ctx::MapValue(indent, key)) => {
                self.output.push_str(&indent);
                encode_string(&key, true, &indent, &mut self.output);
                self.output.push('\n');
                self.output.push_str(&indent);
                self.output.push_str("  ");
                encode_string(variant, true, &indent, &mut self.output);
                self.output.push('\n');
                self.stack.push(Ctx::MapKey(indent.clone()));
                self.stack.push(Ctx::List(indent.clone() + "    "));
            }
        }
        Ok(self)
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        match self.stack.pop() {
            None => {
                self.stack.push(Ctx::MapKey("".to_string()));
            }
            Some(Ctx::MapKey(_)) => {
                return Err(Error {
                    lno: 0,
                    msg: "cannot use maps as map keys".to_string(),
                })?
            }
            Some(Ctx::List(indent)) => {
                self.output.push_str(&indent);
                self.output.push_str("=\n");
                self.stack.push(Ctx::List(indent.clone()));
                self.stack.push(Ctx::MapKey(indent + "  "));
            }
            Some(Ctx::MapValue(indent, key)) => {
                self.output.push_str(&indent);
                encode_string(&key, true, &indent, &mut self.output);
                self.output.push('\n');
                self.stack.push(Ctx::MapKey(indent.clone()));
                self.stack.push(Ctx::MapKey(indent + "  "));
            }
        }
        Ok(self)
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        match self.stack.pop() {
            None => {
                encode_string(variant, true, "", &mut self.output);
                self.output.push('\n');
                self.stack.push(Ctx::List("  ".to_string()));
            }
            Some(Ctx::MapKey(_)) => {
                return Err(Error {
                    lno: 0,
                    msg: "cannot use lists as map keys".to_string(),
                })?
            }
            Some(Ctx::List(indent)) => {
                self.output.push_str(&indent);
                self.output.push_str("=\n");
                self.output.push_str(&indent);
                self.output.push_str("  ");
                encode_string(variant, true, &indent, &mut self.output);
                self.output.push('\n');
                self.stack.push(Ctx::List(indent.clone()));
                self.stack.push(Ctx::MapKey(indent + "    "));
            }
            Some(Ctx::MapValue(indent, key)) => {
                self.output.push_str(&indent);
                encode_string(&key, true, &indent, &mut self.output);
                self.output.push('\n');
                self.output.push_str(&indent);
                self.output.push_str("  ");
                encode_string(variant, true, &indent, &mut self.output);
                self.output.push('\n');
                self.stack.push(Ctx::MapKey(indent.clone()));
                self.stack.push(Ctx::MapKey(indent + "    "));
            }
        }
        Ok(self)
    }
}

impl<'a> ser::SerializeSeq for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    // Close the sequence.
    fn end(self) -> Result<()> {
        self.stack.pop();
        Ok(())
    }
}

impl<'a> ser::SerializeTuple for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.stack.pop();
        Ok(())
    }
}

impl<'a> ser::SerializeTupleStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.stack.pop();
        Ok(())
    }
}

impl<'a> ser::SerializeTupleVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.stack.pop();
        Ok(())
    }
}

impl<'a> ser::SerializeMap for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        key.serialize(&mut **self)
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.stack.pop();
        Ok(())
    }
}

impl<'a> ser::SerializeStruct for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        key.serialize(&mut **self)?;
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.stack.pop();
        Ok(())
    }
}

impl<'a> ser::SerializeStructVariant for &'a mut Serializer {
    type Ok = ();
    type Error = Error;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        key.serialize(&mut **self)?;
        value.serialize(&mut **self)
    }

    fn end(self) -> Result<()> {
        self.stack.pop();
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::collections::BTreeMap;

    use indoc::indoc;
    use serde::{Deserialize, Serialize};

    use crate::{from_str, ser::to_string};

    #[test]
    fn test_ser() {
        assert_eq!(
            to_string(&vec!["a", "b", "c"]).unwrap(),
            indoc! {r#"
                = a
                = b
                = c
            "#}
        );

        assert_eq!(
            to_string(&vec!["\0", "", " c", "abc\ndef"]).unwrap(),
            indoc! {r#"
                = "\{00}"
                = ""
                = " c"
                = """
                  abc
                  def
            "#}
        );

        assert_eq!(
            to_string(&BTreeMap::from([("a", "a"), ("b", "b")])).unwrap(),
            indoc! {r#"
                a = a
                b = b
            "#}
        );

        assert_eq!(
            to_string(&BTreeMap::from([
                ("a", vec!["a"]),
                ("b", vec![]),
                ("c", vec!["=", "wow\nzer", "oop\r\ns"])
            ]))
            .unwrap(),
            indoc! {r#"
                a
                  = a
                c
                  = "="
                  = """
                    wow
                    zer
                  = "oop\r\ns"
            "#}
        );
    }

    #[track_caller]
    fn do_test<'a, T>(value: T, expected: &'a str, then: Option<T>)
    where
        T: Serialize + Deserialize<'a> + PartialEq + std::fmt::Debug,
    {
        assert_eq!(to_string(&value).unwrap(), expected, "serialization");
        dbg!(&expected);
        assert_eq!(
            &from_str::<T>(expected).unwrap(),
            &then.unwrap_or(value),
            "deserialization"
        );
    }

    #[test]
    fn test_roundtrip() {
        // maps of strings
        do_test(
            BTreeMap::from([("a", "a"), ("b", "b")]),
            indoc! {"
            a = a
            b = b
            " },
            None,
        );
        // maps of optional strings
        do_test(
            BTreeMap::from([
                ("a", Some("".to_string())),
                ("b", None),
                ("c", Some(" ".to_string())),
            ]),
            indoc! {r##"
            a = ""
            c = " "
            "##},
            Some(BTreeMap::from([
                ("a", Some(String::new())),
                ("c", Some(" ".to_string())),
            ])),
        );

        #[derive(Deserialize, Serialize, PartialEq, Debug)]
        struct Hah(String);
        do_test(
            vec![Hah("a".into()), Hah("b".into())],
            indoc! {r#"
            = a
            = b
            "#},
            None,
        );

        #[derive(Deserialize, Serialize, PartialEq, Debug)]
        #[serde(rename_all = "snake_case")]
        enum Ugh {
            A,
            B(u8),
            C(u8, u8),
            D { a: u8, b: u8 },
        }
        do_test(
            vec![
                Ugh::A,
                Ugh::B(1),
                Ugh::C(1, 2),
                Ugh::D { a: 3, b: 4 },
                Ugh::A,
            ],
            indoc! {r#"
            = a
            =
              b = 1
            =
              c
                = 1
                = 2
            =
              d
                a = 3
                b = 4
            = a
            "#},
            None,
        );

        #[derive(Deserialize, Serialize, PartialEq, Debug, Default)]
        struct Noop;

        #[derive(Deserialize, Serialize, PartialEq, Debug, Default)]
        #[serde(default)]
        struct Blergh {
            a: (),
            b: Noop,
            c: Vec<()>,
            d: char,
        }

        do_test(
            Blergh {
                a: (),
                b: Noop,
                c: vec![(), (), ()],
                d: '\n',
            },
            indoc! {r#"
              c
                = # none
                = # none
                = # none
              d = "\n"
            "#},
            None,
        );

        #[derive(Deserialize, Serialize, PartialEq, Debug, Default)]
        struct Bites {
            #[serde(with = "serde_bytes")]
            long: Vec<u8>,
            #[serde(with = "serde_bytes")]
            short: Vec<u8>,
        }
        let long = (0..255).map(|i| i as u8).collect::<Vec<_>>();
        let short = (0..25).map(|i| i as u8).collect::<Vec<_>>();

        do_test(
            Bites { long, short },
            indoc! {r#"
            long = """
              AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8gISIjJCUmJygpKissLS4vMDEyMzQ1Njc4OTo7
              PD0+P0BBQkNERUZHSElKS0xNTk9QUVJTVFVWV1hZWltcXV5fYGFiY2RlZmdoaWprbG1ub3BxcnN0dXZ3
              eHl6e3x9fn+AgYKDhIWGh4iJiouMjY6PkJGSk5SVlpeYmZqbnJ2en6ChoqOkpaanqKmqq6ytrq+wsbKz
              tLW2t7i5uru8vb6/wMHCw8TFxsfIycrLzM3Oz9DR0tPU1dbX2Nna29zd3t/g4eLj5OXm5+jp6uvs7e7v
              8PHy8/T19vf4+fr7/P3+
            short = AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGA==
            "#},
            None,
        );

        do_test(
            vec![Some("".to_string()), None, Some("a".to_string())],
            indoc! {r#"
                = ""
                = # none
                = a
            "#},
            Some(vec![Some("".to_string()), None, Some("a".to_string())]),
        );
    }
}
