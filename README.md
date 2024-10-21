CONL is a post-minimalist, human-centric configuration language. It is a replacement for JSON/YAML/TOML, etc... that supports a JSON-like data model of values, maps and lists; but is designed to be much easier to work with.

For more information, see the [README](https://github.com/ConradIrwin/conl).

`serde_conl` provides integration with [Serde](https://serde.rs) to serialize and deserialize CONL documents.

## Getting started

Add this crate to your project
```bash
cargo add serde_conl
```

Define a type to represent your configuration. Use `#[serde(default)]` to ensure that any missing fields are set to the default values, and `#[serde(deny_unknown_fields)]` to provide helpful error messages if someone misspells a field name.
```rust
#[derive(Debug, Default, serde::Deserialize, serde::Serialize)]
#[serde(default, deny_unknown_fields)]
struct Config {
    hostname: String,
    port: u32,
    master: bool,
    ssh_args: Vec<String>,
}
```

Parse the file:
```rust
let file = std::fs::File::read("config.conl").unwrap();

let config: Config = serde_conl::from_slice(&file).unwrap();
```

## Notes

Any value that serde supports can be used as a value in a CONL document. Keys
can be any scalar value (strings, numbers, bools, etc...) but cannot be lists or
maps.

That said, CONL is for configuration... so try to use values that make sense to type in a text file, and stay away from fancy programmer-centric type shenanigans. If you need them,
[serde_humanize_rs](https://crates.io/crates/serde-humanize-rs) provides a bunch of types
like durations and file sizes.

Options are typically represented by omitting the field from the map, but as
CONL has no nullability, the empty string is used in map keys or list items. A unit
`()` or `struct X;` is represented by the empty string.

Enums are represented as they are in JSON (but without the syntax noise). For example:
```rust
enum Example {
    A,
    B(u32),
    C { a: u32, b: u32 },
    D(u32, u32)
}
```

```conl
example_a = a
example_b
  b = 1
example_c
  c
    a = 1
    b = 2
example_d
  = 1
  = 2
```
