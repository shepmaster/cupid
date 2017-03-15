# Cupid

Native Rust access to the x86 and x86_64 CPUID instruction.

[![Build Status](https://travis-ci.org/shepmaster/cupid.svg)](https://travis-ci.org/shepmaster/cupid) [![Current Version](http://meritbadge.herokuapp.com/cupid)](https://crates.io/crates/cupid)

[Documentation](https://docs.rs/cupid/)

## Overview

```rust
extern crate cupid;

fn main() {
    let information = cupid::master();
    println!("{:#?}", information);
    if let Some(information) = information {
        if information.sse4_2() {
             println!("SSE 4.2 Available");
        }
    }
}
```

Cupid currently requires a Rust nightly build as it makes use of
[inline assembly][asm]. Chances are good that if you want to use the
functionality provided, you are also doing some low-level trickery, so
hopefully this won't be too inconvenient.

[asm]: http://doc.rust-lang.org/stable/book/inline-assembly.html

## See also

* [libcpuid](http://libcpuid.sourceforge.net/) - A C library providing
  access to the CPUID instruction.
* [cpuid](https://crates.io/crates/cpuid) - Rust bindings to the
  libcpuid library.
* [rust-x86](https://github.com/gz/rust-x86) - Another native crate
  that includes CPUID and more.

## Contributing

1. Fork it ( https://github.com/shepmaster/cupid/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Add a failing test.
4. Add code to pass the test.
5. Commit your changes (`git commit -am 'Add some feature'`)
6. Ensure tests pass.
7. Push to the branch (`git push origin my-new-feature`)
8. Create a new Pull Request
