extern crate gcc;

use std::env;

fn main() {
    if env::var("CARGO_FEATURE_UNSTABLE").is_ok() {
        return
    }

    let target = env::var("TARGET").unwrap();

    // not supported on non-x86 platforms
    if !target.starts_with("x86_64") &&
       !target.starts_with("i686") &&
       !target.starts_with("i586") {
        return
    }

    let mut cfg = gcc::Config::new();
    let msvc = target.contains("msvc");
    if target.starts_with("x86_64") {
        cfg.file(if msvc {"src/arch/x86_64.asm"} else {"src/arch/x86_64.S"});
        cfg.define("X86_64", None);
    } else {
        cfg.file(if msvc {"src/arch/i686.asm"} else {"src/arch/i686.S"});
        cfg.define("X86", None);
    }
    if target.contains("darwin") {
        cfg.define("APPLE", None);
    }
    if target.contains("windows") {
        cfg.define("WINDOWS", None);
    }
    cfg.include("src/arch");
    cfg.compile("libcupid.a");
}
