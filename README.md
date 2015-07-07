# CPUID

Native Rust access to the x86 and x86_64 CPUID instruction.

## Overview

```rust
extern crate cpuid;

fn main() {
    let info = cpuid::feature_information();
    if (info.sse4_2()) { println!("Yay! SSE 4.2 detected!") }
    println!("{:?}", info);
}
```

## See also

* [libcpuid](http://libcpuid.sourceforge.net/) - A C library providing
  access the the CPUID instruction.
* [cpuid](https://crates.io/crates/cpuid) - Rust bindings to the
  libcpuid library.
* [rust-x86](https://github.com/gz/rust-x86) - Another native crate
  that includes CPUID and more.

## Contributing

1. Fork it ( https://github.com/shepmaster/cpuid-rs/fork )
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Add a failing test.
4. Add code to pass the test.
5. Commit your changes (`git commit -am 'Add some feature'`)
6. Ensure tests pass.
7. Push to the branch (`git push origin my-new-feature`)
8. Create a new Pull Request
