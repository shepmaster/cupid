extern crate gcc;
extern crate rustc_version;

use rustc_version::{version, Version};
use std::{env, fmt};

fn main() {
    println!("cargo:rerun-if-env-changed=CUPID_PRETEND_CPUID_NOT_AVAILABLE");
    println!("cargo:rerun-if-env-changed=CUPID_FORCE_ENGINE");

    if env::var_os("CUPID_PRETEND_CPUID_NOT_AVAILABLE").is_some() {
        return;
    }

    let target = env::var("TARGET")
        .expect("TARGET environment variable must be set");

    // not supported on non-x86 platforms
    if !target.starts_with("x86_64") &&
       !target.starts_with("i686") &&
       !target.starts_with("i586") {
        return
    }

    println!("cargo:rustc-cfg=cpuid_available");

    let engine = Engine::detect();

    if engine == Engine::C {
        build_c_shim(&target);
    }

    println!("cargo:rustc-cfg={}", engine);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Engine {
    C,
    Std,
}

impl fmt::Display for Engine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Engine::C => "engine_c",
            Engine::Std => "engine_std",
        }.fmt(f)
    }
}

impl Engine {
    fn detect() -> Self {
        match env::var("CUPID_FORCE_ENGINE").as_ref().map(|x| &**x) {
            Ok("c") => return Engine::C,
            Ok("std") => return Engine::Std,
            _ => {}
        };

        let vers = version().expect("Could not determine Rust version");
        let std_simd_vers = Version::parse("1.27.0-beta.0").expect("Invalid base version");

        if vers >= std_simd_vers {
            Engine::Std
        } else {
            Engine::C
        }
    }
}

fn build_c_shim(target: &str) {
    let mut cfg = gcc::Build::new();
    if target.contains("msvc") {
        cfg.define("MSVC", None);
    }
    cfg.file("src/arch/shim.c");
    cfg.compile("libcupid.a");

    println!("cargo:rerun-if-changed=src/arch/shim.c");
}
