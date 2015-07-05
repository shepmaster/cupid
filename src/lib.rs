#![feature(asm)]

use std::{fmt, slice, str};
use std::ops::Deref;

enum RequestType {
    BasicInformation            = 0x00000000,
    VersionInformation          = 0x00000001,
    ExtendedFunctionInformation = 0x80000000,
    BrandString1                = 0x80000002,
    BrandString2                = 0x80000003,
    BrandString3                = 0x80000004,
}

fn cpuid(code: RequestType) -> (u32, u32, u32, u32) {
    let res1;
    let res2;
    let res3;
    let res4;

    unsafe {
        asm!("cpuid"
             : // output operands
             "={eax}"(res1),
             "={ebx}"(res2),
             "={ecx}"(res3),
             "={edx}"(res4)
             : // input operands
             "{eax}"(code as u32)
             : // clobbers
             : // options
        );
    }

    (res1, res2, res3, res4)
}

// This matches the Intel Architecture guide, with bits 31 -> 0.
// The bit positions are inclusive.
fn bits_of(val: u32, start_bit: u8, end_bit: u8) -> u32 {
    let mut silly = 0;

    for _ in start_bit..end_bit+1 {
        silly <<= 1;
        silly |= 1;
    }

    (val >> start_bit) & silly
}

macro_rules! bit {
    ($reg:ident, $idx:expr, $name:ident) => {
        pub fn $name(self) -> bool {
            ((self.$reg >> $idx) & 1) != 0
        }
    }
}

/// Exposes the processor feature flags.
///
/// Each method corresponds to a single capability. Method names match
/// the feature mnemonic listed in the Intel Instruction Set
/// Reference.
#[derive(Copy, Clone, Debug)]
pub struct FeatureInformation {
    ecx: u32,
    edx: u32,
}

impl FeatureInformation {
    bit!(ecx,  0, sse3);
    bit!(ecx,  1, pclmulqdq);
    bit!(ecx,  2, dtes64);
    bit!(ecx,  3, monitor);
    bit!(ecx,  4, ds_cpl);
    bit!(ecx,  5, vmx);
    bit!(ecx,  6, smx);
    bit!(ecx,  7, eist);
    bit!(ecx,  8, tm2);
    bit!(ecx,  9, ssse3);
    bit!(ecx, 10, cnxt_id);
    bit!(ecx, 11, sdbg);
    bit!(ecx, 12, fma);
    bit!(ecx, 13, cmpxchg16b);
    bit!(ecx, 14, xtpr_update_control);
    bit!(ecx, 15, pdcm);
    // 16 - reserved
    bit!(ecx, 17, pcid);
    bit!(ecx, 18, dca);
    bit!(ecx, 19, sse4_1);
    bit!(ecx, 20, sse4_2);
    bit!(ecx, 21, x2apic);
    bit!(ecx, 22, movbe);
    bit!(ecx, 23, popcnt);
    bit!(ecx, 24, tsc_deadline);
    bit!(ecx, 25, aesni);
    bit!(ecx, 26, xsave);
    bit!(ecx, 27, osxsave);
    bit!(ecx, 28, avx);
    bit!(ecx, 29, f16c);
    bit!(ecx, 30, rdrand);
    // 31 - unused

    bit!(edx,  0, fpu);
    bit!(edx,  1, vme);
    bit!(edx,  2, de);
    bit!(edx,  3, pse);
    bit!(edx,  4, tsc);
    bit!(edx,  5, msr);
    bit!(edx,  6, pae);
    bit!(edx,  7, mce);
    bit!(edx,  8, cx8);
    bit!(edx,  9, apic);
    // reserved
    bit!(edx, 11, sep);
    bit!(edx, 12, mtrr);
    bit!(edx, 13, pge);
    bit!(edx, 14, mca);
    bit!(edx, 15, cmov);
    bit!(edx, 16, pat);
    bit!(edx, 17, pse_36);
    bit!(edx, 18, psn);
    bit!(edx, 19, clfsh);
    // reserved
    bit!(edx, 21, ds);
    bit!(edx, 22, acpi);
    bit!(edx, 23, mmx);
    bit!(edx, 24, fxsr);
    bit!(edx, 25, sse);
    bit!(edx, 26, sse2);
    bit!(edx, 27, ss);
    bit!(edx, 28, htt);
    bit!(edx, 29, tm);
    // reserved
    bit!(edx, 31, pbe);
}

macro_rules! dump {
    ($me:expr, $f: expr, $name: ident) => {
        try!(writeln!($f, "{}: {}", stringify!($name), $me.$name()));
    }
}

impl fmt::Display for FeatureInformation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, sse3);
        dump!(self, f, pclmulqdq);
        dump!(self, f, dtes64);
        dump!(self, f, monitor);
        dump!(self, f, ds_cpl);
        dump!(self, f, vmx);
        dump!(self, f, smx);
        dump!(self, f, eist);
        dump!(self, f, tm2);
        dump!(self, f, ssse3);
        dump!(self, f, cnxt_id);
        dump!(self, f, sdbg);
        dump!(self, f, fma);
        dump!(self, f, cmpxchg16b);
        dump!(self, f, xtpr_update_control);
        dump!(self, f, pdcm);
        dump!(self, f, pcid);
        dump!(self, f, dca);
        dump!(self, f, sse4_1);
        dump!(self, f, sse4_2);
        dump!(self, f, x2apic);
        dump!(self, f, movbe);
        dump!(self, f, popcnt);
        dump!(self, f, tsc_deadline);
        dump!(self, f, aesni);
        dump!(self, f, xsave);
        dump!(self, f, osxsave);
        dump!(self, f, avx);
        dump!(self, f, f16c);
        dump!(self, f, rdrand);
        dump!(self, f, fpu);
        dump!(self, f, vme);
        dump!(self, f, de);
        dump!(self, f, pse);
        dump!(self, f, tsc);
        dump!(self, f, msr);
        dump!(self, f, pae);
        dump!(self, f, mce);
        dump!(self, f, cx8);
        dump!(self, f, apic);
        dump!(self, f, sep);
        dump!(self, f, mtrr);
        dump!(self, f, pge);
        dump!(self, f, mca);
        dump!(self, f, cmov);
        dump!(self, f, pat);
        dump!(self, f, pse_36);
        dump!(self, f, psn);
        dump!(self, f, clfsh);
        dump!(self, f, ds);
        dump!(self, f, acpi);
        dump!(self, f, mmx);
        dump!(self, f, fxsr);
        dump!(self, f, sse);
        dump!(self, f, sse2);
        dump!(self, f, ss);
        dump!(self, f, htt);
        dump!(self, f, tm);
        dump!(self, f, pbe);
        Ok(())
    }
}

pub fn feature_information() -> FeatureInformation {
    let (_, _, c, d) = cpuid(RequestType::VersionInformation);
    FeatureInformation { ecx: c, edx: d }
}

fn as_bytes(v: &u32) -> &[u8] {
    let start = v as *const u32 as *const u8;
    // TODO: use u32::BYTES
    unsafe { slice::from_raw_parts(start, 4) }
}

// 3 calls of 4 registers of 4 bytes
const BRAND_STRING_LENGTH: usize = 3 * 4 * 4;

pub struct BrandString {
    bytes: [u8; BRAND_STRING_LENGTH],
}

impl BrandString {
    fn new() -> BrandString {
        BrandString { bytes: [0; BRAND_STRING_LENGTH] }
    }
}

impl Deref for BrandString {
    type Target = str;

    fn deref(&self) -> &str {
        let nul_terminator = self.bytes.iter().position(|&b| b == 0).unwrap_or(0);
        let usable_bytes = &self.bytes[..nul_terminator];
        unsafe { str::from_utf8_unchecked(usable_bytes) }
    }
}

impl fmt::Display for BrandString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self as &str).fmt(f)
    }
}

pub fn brand_string() -> BrandString {
    // Should check supported (EAX Return Value of 0x80000000 ≥ 0x80000004)

    fn append_bytes(a: RequestType, bytes: &mut [u8]) {
        let (a, b, c, d) = cpuid(a);

        let result_bytes =
            as_bytes(&a).iter()
            .chain(as_bytes(&b).iter())
            .chain(as_bytes(&c).iter())
            .chain(as_bytes(&d).iter());

        for (output, input) in bytes.iter_mut().zip(result_bytes) {
            *output = *input
        }
    }

    let mut brand_string = BrandString::new();
    append_bytes(RequestType::BrandString1, &mut brand_string.bytes[0..]);
    append_bytes(RequestType::BrandString2, &mut brand_string.bytes[16..]);
    append_bytes(RequestType::BrandString3, &mut brand_string.bytes[32..]);
    brand_string
}

#[test]
fn basic_genuine_intel() {
    // let (a,b,c,d) = cpuid(RequestType::BasicInformation);

    // assert_eq!(b"Genu", b);
    // assert_eq!(b"ntel", c);
    // assert_eq!(b"ineI", d);
}

#[test]
fn brand_string_contains_intel() {
    assert!(brand_string().contains("Intel(R)"))
}
