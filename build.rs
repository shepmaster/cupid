extern crate gcc;

use std::env;

fn main() {
    if env::var("CARGO_FEATURE_UNSTABLE").is_ok() {
        return
    }

    let target = env::var("TARGET")
        .expect("TARGET environment variable must be set");

    // not supported on non-x86 platforms
    if !target.starts_with("x86_64") &&
       !target.starts_with("i686") &&
       !target.starts_with("i586") {
        return
    }

    let mut cfg = gcc::Config::new();
    if target.contains("msvc") {
        cfg.define("MSVC", None);
    }
    cfg.file("src/arch/shim.c");
    cfg.compile("libcupid.a");

    println!("cargo:rerun-if-changed=src/arch/shim.c");
}
