# OpenSCAD Rust

[![Continuous integration](https://github.com/Michael-F-Bryan/scad-rs/workflows/Continuous%20Integration/badge.svg?branch=main)](https://github.com/Michael-F-Bryan/scad-rs/actions)

([API Docs][api-docs])

A Rust implementation of the [OpenSCAD][scad-website] virtual machine.

## For Developers

When you initially check out the repository, make sure to also fetch the
`openscad` submodule we use for integration/compatibility testing.

```console
$ git clone --recursive https://github.com/Michael-F-Bryan/scad-rs.git
```

I'll typically use [`cargo watch`][cargo-watch] to automatically recompile the
project and run the test suite.

```console
$ cargo watch --clear --ignore "snapshots/*.snap*" \
    -x "check --workspace" \
    -x "test --workspace" \
    -x "doc --workspace --document-private-items" \
    -x "build --workspace --release" \
    -x "integration-tests"
```

## License

This project is licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](./LICENSE-APACHE.md) or
  <http://www.apache.org/licenses/LICENSE-2.0>)
- MIT license ([LICENSE-MIT](./LICENSE-MIT.md) or
   <http://opensource.org/licenses/MIT>)

at your option.

It is recommended to always use [`cargo crev`][crev] to verify the
trustworthiness of each of your dependencies, including this one.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally
submitted for inclusion in the work by you, as defined in the Apache-2.0
license, shall be dual licensed as above, without any additional terms or
conditions.

The intent of this crate is to be free of soundness bugs. The developers will
do their best to avoid them, and welcome help in analysing and fixing them.

[api-docs]: https://michael-f-bryan.github.io/scad-rs
[cargo-watch]: https://crates.io/crates/cargo-watch
[crev]: https://github.com/crev-dev/cargo-crev
[scad-website]: https://openscad.org/
