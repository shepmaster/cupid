#![feature(asm)]

use std::{fmt, slice, str};
use std::ops::Deref;

enum RequestType {
    BasicInformation                  = 0x00000000,
    VersionInformation                = 0x00000001,
    ThermalPowerManagementInformation = 0x00000006,
    StructuredExtendedInformation     = 0x00000007,
    ExtendedFunctionInformation       = 0x80000000,
    BrandString1                      = 0x80000002,
    BrandString2                      = 0x80000003,
    BrandString3                      = 0x80000004,
    PhysicalAddressSize               = 0x80000008,
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
             "{eax}"(code as u32),
             "{ecx}"(0 as u32)
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
#[derive(Copy, Clone)]
pub struct VersionInformation {
    ecx: u32,
    edx: u32,
}

impl VersionInformation {
    fn new() -> VersionInformation {
        let (_, _, c, d) = cpuid(RequestType::VersionInformation);
        VersionInformation { ecx: c, edx: d }
    }

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
    // 10 - reserved
    bit!(edx, 11, sep);
    bit!(edx, 12, mtrr);
    bit!(edx, 13, pge);
    bit!(edx, 14, mca);
    bit!(edx, 15, cmov);
    bit!(edx, 16, pat);
    bit!(edx, 17, pse_36);
    bit!(edx, 18, psn);
    bit!(edx, 19, clfsh);
    // 20 - reserved
    bit!(edx, 21, ds);
    bit!(edx, 22, acpi);
    bit!(edx, 23, mmx);
    bit!(edx, 24, fxsr);
    bit!(edx, 25, sse);
    bit!(edx, 26, sse2);
    bit!(edx, 27, ss);
    bit!(edx, 28, htt);
    bit!(edx, 29, tm);
    // 30 -reserved
    bit!(edx, 31, pbe);
}

macro_rules! dump {
    ($me:expr, $f: expr, $sname:expr, {$($name:ident),+}) => {
        $f.debug_struct($sname)
            $(.field(stringify!($name), &$me.$name()))+
            .finish()
    }
}

impl fmt::Debug for VersionInformation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, "VersionInformation", {
            sse3,
            pclmulqdq,
            dtes64,
            monitor,
            ds_cpl,
            vmx,
            smx,
            eist,
            tm2,
            ssse3,
            cnxt_id,
            sdbg,
            fma,
            cmpxchg16b,
            xtpr_update_control,
            pdcm,
            pcid,
            dca,
            sse4_1,
            sse4_2,
            x2apic,
            movbe,
            popcnt,
            tsc_deadline,
            aesni,
            xsave,
            osxsave,
            avx,
            f16c,
            rdrand,
            fpu,
            vme,
            de,
            pse,
            tsc,
            msr,
            pae,
            mce,
            cx8,
            apic,
            sep,
            mtrr,
            pge,
            mca,
            cmov,
            pat,
            pse_36,
            psn,
            clfsh,
            ds,
            acpi,
            mmx,
            fxsr,
            sse,
            sse2,
            ss,
            htt,
            tm,
            pbe
        })
    }
}

fn as_bytes(v: &u32) -> &[u8] {
    let start = v as *const u32 as *const u8;
    // TODO: use u32::BYTES
    unsafe { slice::from_raw_parts(start, 4) }
}

// 3 calls of 4 registers of 4 bytes
const BRAND_STRING_LENGTH: usize = 3 * 4 * 4;

/// The brand of the processor.
pub struct BrandString {
    bytes: [u8; BRAND_STRING_LENGTH],
}

impl BrandString {
    fn new() -> BrandString {
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

        let mut brand_string = BrandString { bytes: [0; BRAND_STRING_LENGTH] };
        append_bytes(RequestType::BrandString1, &mut brand_string.bytes[0..]);
        append_bytes(RequestType::BrandString2, &mut brand_string.bytes[16..]);
        append_bytes(RequestType::BrandString3, &mut brand_string.bytes[32..]);
        brand_string
    }
}

impl Clone for BrandString {
    fn clone(&self) -> Self {
        let mut bytes = [0; BRAND_STRING_LENGTH];
        for (d, s) in bytes.iter_mut().zip(self.bytes.iter()) {
            *d = *s;
        }
        BrandString { bytes: bytes }
    }
}

impl Deref for BrandString {
    type Target = str;

    fn deref(&self) -> &str {
        let nul_terminator = self.bytes.iter().position(|&b| b == 0).unwrap_or(0);
        let usable_bytes = &self.bytes[..nul_terminator];
        unsafe { str::from_utf8_unchecked(usable_bytes) }.trim()
    }
}

impl fmt::Display for BrandString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self as &str).fmt(f)
    }
}

impl fmt::Debug for BrandString {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        (self as &str).fmt(f)
    }
}

#[derive(Copy,Clone)]
pub struct ThermalPowerManagementInformation {
    eax: u32,
    ebx: u32,
    ecx: u32,
}

impl ThermalPowerManagementInformation {
    fn new() -> ThermalPowerManagementInformation {
        let (a, b, c, _) = cpuid(RequestType::ThermalPowerManagementInformation);
        ThermalPowerManagementInformation { eax: a, ebx: b, ecx: c }
    }

    bit!(eax,  0, digital_temperature_sensor);
    bit!(eax,  1, intel_turbo_boost);
    bit!(eax,  2, arat);
    // 3 - reserved
    bit!(eax,  4, pln);
    bit!(eax,  5, ecmd);
    bit!(eax,  6, ptm);
    bit!(eax,  7, hwp);
    bit!(eax,  8, hwp_notification);
    bit!(eax,  9, hwp_activity_window);
    bit!(eax, 10, hwp_energy_performance_preference);
    // 12 - reserved
    bit!(eax, 13, hdc);

    pub fn number_of_interrupt_thresholds(self) -> u32 {
        bits_of(self.ebx, 0, 3)
    }

    bit!(ecx, 0, hardware_coordination_feedback);
    // 1 - reserved
    // 2 - reserved
    bit!(ecx, 3, performance_energy_bias);
}

impl fmt::Debug for ThermalPowerManagementInformation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, "ThermalPowerManagementInformation", {
            digital_temperature_sensor,
            intel_turbo_boost,
            arat,
            pln,
            ecmd,
            ptm,
            hwp,
            hwp_notification,
            hwp_activity_window,
            hwp_energy_performance_preference,
            hdc,

            number_of_interrupt_thresholds,

            hardware_coordination_feedback,
            performance_energy_bias
        })
    }
}

#[derive(Copy,Clone)]
pub struct StructuredExtendedInformation {
    ebx: u32,
    ecx: u32,
}

impl StructuredExtendedInformation {
    fn new() -> StructuredExtendedInformation {
        let (_, b, c, _) = cpuid(RequestType::StructuredExtendedInformation);
        StructuredExtendedInformation { ebx: b, ecx: c }
    }

    bit!(ebx,  0, fsgsbase);
    bit!(ebx,  1, ia32_tsc_adjust_msr);
    // 2 - reserved
    bit!(ebx,  3, bmi1);
    bit!(ebx,  4, hle);
    bit!(ebx,  5, avx2);
    // 6 - reserved
    bit!(ebx,  7, smep);
    bit!(ebx,  8, bmi2);
    bit!(ebx,  9, enhanced_rep_movsb_stosb);
    bit!(ebx, 10, invpcid);
    bit!(ebx, 11, rtm);
    bit!(ebx, 12, pqm);
    bit!(ebx, 13, deprecates_fpu_cs_ds);
    // 14 - reserved
    bit!(ebx, 15, pqe);
    // 16 - reserved
    // 17 - reserved
    bit!(ebx, 18, rdseed);
    bit!(ebx, 19, adx);
    bit!(ebx, 20, smap);
    // 21 - reserved
    // 22 - reserved
    // 23 - reserved
    // 24 - reserved
    bit!(ebx, 25, intel_processor_trace);
    // 26 - reserved
    // 27 - reserved
    // 28 - reserved
    // 29 - reserved
    // 30 - reserved
    // 31 - reserved

    bit!(ecx,  0, prefetchwt1);
}

impl fmt::Debug for StructuredExtendedInformation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, "StructuredExtendedInformation", {
            fsgsbase,
            ia32_tsc_adjust_msr,
            bmi1,
            hle,
            avx2,
            smep,
            bmi2,
            enhanced_rep_movsb_stosb,
            invpcid,
            rtm,
            pqm,
            deprecates_fpu_cs_ds,
            pqe,
            rdseed,
            adx,
            smap,
            intel_processor_trace,
            prefetchwt1
        })
    }
}

#[derive(Copy,Clone)]
pub struct PhysicalAddressSize(u32);

impl PhysicalAddressSize {
    fn new() -> PhysicalAddressSize {
        let (a, _, _, _) = cpuid(RequestType::PhysicalAddressSize);
        PhysicalAddressSize(a)
    }

    pub fn physical_address_bits(self) -> u32 {
        bits_of(self.0, 0, 7)
    }

    pub fn linear_address_bits(self) -> u32 {
        bits_of(self.0, 8, 15)
    }
}

impl fmt::Debug for PhysicalAddressSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, "PhysicalAddressSize", {
            physical_address_bits,
            linear_address_bits
        })
    }
}

#[derive(Debug,Clone)]
pub struct Master {
    // TODO: Rename struct
    version_information: Option<VersionInformation>,
    thermal_power_management_information: Option<ThermalPowerManagementInformation>,
    structured_extended_information: Option<StructuredExtendedInformation>,
    brand_string: Option<BrandString>,
    physical_address_size: Option<PhysicalAddressSize>,
}

macro_rules! delegate_flag {
    ($item:ident, $name:ident) => {
        pub fn $name(self) -> bool {
            self.$item.map(|i| i.$name()).unwrap_or(false)
        }
    }
}

impl Master {
    pub fn new() -> Master {
        fn when_supported<F, T>(max: u32, kind: RequestType, then: F) -> Option<T>
            where F: FnOnce() -> T
        {
            if max >= kind as u32 {
                Some(then())
            } else {
                None
            }
        }

        let (max_value, _, _, _) = cpuid(RequestType::BasicInformation);

        let vi = when_supported(max_value, RequestType::VersionInformation, || {
            VersionInformation::new()
        });
        let tpm = when_supported(max_value, RequestType::ThermalPowerManagementInformation, || {
            ThermalPowerManagementInformation::new()
        });
        let sei = when_supported(max_value, RequestType::StructuredExtendedInformation, || {
            StructuredExtendedInformation::new()
        });

        // Extended information

        let (max_value, _, _, _) = cpuid(RequestType::ExtendedFunctionInformation);

        let brand_string = when_supported(max_value, RequestType::BrandString3, || {
            BrandString::new()
        });
        let pas = when_supported(max_value, RequestType::PhysicalAddressSize, || {
            PhysicalAddressSize::new()
        });

        Master {
            version_information: vi,
            thermal_power_management_information: tpm,
            structured_extended_information: sei,
            brand_string: brand_string,
            physical_address_size: pas,
        }
    }

    delegate_flag!(version_information, sse3);
    delegate_flag!(version_information, pclmulqdq);
    delegate_flag!(version_information, dtes64);
    delegate_flag!(version_information, monitor);
    delegate_flag!(version_information, ds_cpl);
    delegate_flag!(version_information, vmx);
    delegate_flag!(version_information, smx);
    delegate_flag!(version_information, eist);
    delegate_flag!(version_information, tm2);
    delegate_flag!(version_information, ssse3);
    delegate_flag!(version_information, cnxt_id);
    delegate_flag!(version_information, sdbg);
    delegate_flag!(version_information, fma);
    delegate_flag!(version_information, cmpxchg16b);
    delegate_flag!(version_information, xtpr_update_control);
    delegate_flag!(version_information, pdcm);
    delegate_flag!(version_information, pcid);
    delegate_flag!(version_information, dca);
    delegate_flag!(version_information, sse4_1);
    delegate_flag!(version_information, sse4_2);
    delegate_flag!(version_information, x2apic);
    delegate_flag!(version_information, movbe);
    delegate_flag!(version_information, popcnt);
    delegate_flag!(version_information, tsc_deadline);
    delegate_flag!(version_information, aesni);
    delegate_flag!(version_information, xsave);
    delegate_flag!(version_information, osxsave);
    delegate_flag!(version_information, avx);
    delegate_flag!(version_information, f16c);
    delegate_flag!(version_information, rdrand);
    delegate_flag!(version_information, fpu);
    delegate_flag!(version_information, vme);
    delegate_flag!(version_information, de);
    delegate_flag!(version_information, pse);
    delegate_flag!(version_information, tsc);
    delegate_flag!(version_information, msr);
    delegate_flag!(version_information, pae);
    delegate_flag!(version_information, mce);
    delegate_flag!(version_information, cx8);
    delegate_flag!(version_information, apic);
    delegate_flag!(version_information, sep);
    delegate_flag!(version_information, mtrr);
    delegate_flag!(version_information, pge);
    delegate_flag!(version_information, mca);
    delegate_flag!(version_information, cmov);
    delegate_flag!(version_information, pat);
    delegate_flag!(version_information, pse_36);
    delegate_flag!(version_information, psn);
    delegate_flag!(version_information, clfsh);
    delegate_flag!(version_information, ds);
    delegate_flag!(version_information, acpi);
    delegate_flag!(version_information, mmx);
    delegate_flag!(version_information, fxsr);
    delegate_flag!(version_information, sse);
    delegate_flag!(version_information, sse2);
    delegate_flag!(version_information, ss);
    delegate_flag!(version_information, htt);
    delegate_flag!(version_information, tm);
    delegate_flag!(version_information, pbe);

    delegate_flag!(thermal_power_management_information, digital_temperature_sensor);
    delegate_flag!(thermal_power_management_information, intel_turbo_boost);
    delegate_flag!(thermal_power_management_information, arat);
    delegate_flag!(thermal_power_management_information, pln);
    delegate_flag!(thermal_power_management_information, ecmd);
    delegate_flag!(thermal_power_management_information, ptm);
    delegate_flag!(thermal_power_management_information, hwp);
    delegate_flag!(thermal_power_management_information, hwp_notification);
    delegate_flag!(thermal_power_management_information, hwp_activity_window);
    delegate_flag!(thermal_power_management_information, hwp_energy_performance_preference);
    delegate_flag!(thermal_power_management_information, hdc);
    delegate_flag!(thermal_power_management_information, hardware_coordination_feedback);
    delegate_flag!(thermal_power_management_information, performance_energy_bias);

    delegate_flag!(structured_extended_information, fsgsbase);
    delegate_flag!(structured_extended_information, ia32_tsc_adjust_msr);
    delegate_flag!(structured_extended_information, bmi1);
    delegate_flag!(structured_extended_information, hle);
    delegate_flag!(structured_extended_information, avx2);
    delegate_flag!(structured_extended_information, smep);
    delegate_flag!(structured_extended_information, bmi2);
    delegate_flag!(structured_extended_information, enhanced_rep_movsb_stosb);
    delegate_flag!(structured_extended_information, invpcid);
    delegate_flag!(structured_extended_information, rtm);
    delegate_flag!(structured_extended_information, pqm);
    delegate_flag!(structured_extended_information, deprecates_fpu_cs_ds);
    delegate_flag!(structured_extended_information, pqe);
    delegate_flag!(structured_extended_information, rdseed);
    delegate_flag!(structured_extended_information, adx);
    delegate_flag!(structured_extended_information, smap);
    delegate_flag!(structured_extended_information, intel_processor_trace);
    delegate_flag!(structured_extended_information, prefetchwt1);
}

pub fn master() -> Master {
    Master::new()
}

#[test]
fn basic_genuine_intel() {
    let (_, b, c, d) = cpuid(RequestType::BasicInformation);

    assert_eq!(b"Genu", as_bytes(&b));
    assert_eq!(b"ntel", as_bytes(&c));
    assert_eq!(b"ineI", as_bytes(&d));
}

#[test]
fn brand_string_contains_intel() {
    assert!(brand_string().contains("Intel(R)"))
}
