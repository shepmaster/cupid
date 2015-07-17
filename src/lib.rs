#![feature(asm)]

//! ```
//! extern crate cupid;
//!
//! fn main() {
//!     let information = cupid::master();
//!     println!("{:#?}", information);
//!     if information.sse4_2() {
//!          println!("SSE 4.2 Available");
//!     }
//! }
//! ```

use std::{fmt, slice, str};
use std::ops::Deref;

enum RequestType {
    BasicInformation                  = 0x00000000,
    VersionInformation                = 0x00000001,
    ThermalPowerManagementInformation = 0x00000006,
    StructuredExtendedInformation     = 0x00000007,
    ExtendedFunctionInformation       = 0x80000000,
    ExtendedProcessorSignature        = 0x80000001,
    BrandString1                      = 0x80000002,
    BrandString2                      = 0x80000003,
    BrandString3                      = 0x80000004,
    // reserved                       = 0x80000005,
    CacheLine                         = 0x80000006,
    TimeStampCounter                  = 0x80000007,
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

fn as_bytes(v: &u32) -> &[u8] {
    let start = v as *const u32 as *const u8;
    // TODO: use u32::BYTES
    unsafe { slice::from_raw_parts(start, 4) }
}

macro_rules! bit {
    ($reg:ident, {$($idx:expr => $name:ident),+}) => {
        $(pub fn $name(self) -> bool {
            ((self.$reg >> $idx) & 1) != 0
        })+
    }
}

macro_rules! dump {
    ($me:expr, $f: expr, $sname:expr, {$($name:ident),+}) => {
        $f.debug_struct($sname)
            $(.field(stringify!($name), &$me.$name()))+
            .finish()
    }
}

macro_rules! delegate_flag {
    ($item:ident, {$($name:ident),+}) => {
        $(pub fn $name(self) -> bool {
            self.$item.map(|i| i.$name()).unwrap_or(false)
        })+
    }
}

#[derive(Copy, Clone)]
pub struct VersionInformation {
    eax: u32,
    ebx: u32,
    ecx: u32,
    edx: u32,
}

impl VersionInformation {
    fn new() -> VersionInformation {
        let (a, b, c, d) = cpuid(RequestType::VersionInformation);
        VersionInformation { eax: a, ebx: b, ecx: c, edx: d }
    }

    pub fn family_id(self) -> u32 {
        let family_id = bits_of(self.eax, 8, 11);
        let extended_family_id = bits_of(self.eax, 20, 27);

        if family_id != 0x0F {
            family_id
        } else {
            extended_family_id + family_id
        }
    }

    pub fn model_id(self) -> u32 {
        let family_id = self.family_id();
        let model_id = bits_of(self.eax, 4, 7);
        let extended_model_id = bits_of(self.eax, 16, 19);

        if family_id == 0x06 || family_id == 0x0F {
            (extended_model_id << 4) + model_id
        } else {
            model_id
        }
    }

    pub fn stepping(self) -> u32 {
        bits_of(self.eax, 0, 3)
    }

    fn processor_signature(self) -> u32 {
        self.eax
    }

    pub fn brand_string(self) -> Option<&'static str> {
        let brand_index = bits_of(self.ebx, 0, 7);
        let processor_signature = self.processor_signature();

        match brand_index {
            0x00 => None,
            0x01 => Some("Intel(R) Celeron(R)"),
            0x02 => Some("Intel(R) Pentium(R) III"),
            0x03 => {
                if processor_signature == 0x06B1 {
                    Some("Intel(R) Celeron(R)")
                } else {
                    Some("Intel(R) Pentium(R) III Xeon(R)")
                }
            },
            0x04 => Some("Intel(R) Pentium(R) III"),
            0x06 => Some("Mobile Intel(R) Pentium(R) III-M"),
            0x07 => Some("Mobile Intel(R) Celeron(R)"),
            0x08 => Some("Intel(R) Pentium(R) 4"),
            0x09 => Some("Intel(R) Pentium(R) 4"),
            0x0A => Some("Intel(R) Celeron(R)"),
            0x0B => {
                if processor_signature == 0x0F13 {
                    Some("Intel(R) Xeon(R) MP")
                } else {
                    Some("Intel(R) Xeon(R)")
                }
            },
            0x0C => Some("Intel(R) Xeon(R) MP"),
            0x0E => {
                if processor_signature == 0x0F13 {
                    Some("Intel(R) Xeon(R)")
                } else {
                    Some("Mobile Intel(R) Pentium(R) 4-M")
                }
            },
            0x0F => Some("Mobile Intel(R) Celeron(R)"),
            0x11 => Some("Mobile Genuine Intel(R)"),
            0x12 => Some("Intel(R) Celeron(R) M"),
            0x13 => Some("Mobile Intel(R) Celeron(R)"),
            0x14 => Some("Intel(R) Celeron(R)"),
            0x15 => Some("Mobile Genuine Intel(R)"),
            0x16 => Some("Intel(R) Pentium(R) M"),
            0x17 => Some("Mobile Intel(R) Celeron(R)"),
            _ => None,
        }
    }

    bit!(ecx, {
         0 => sse3,
         1 => pclmulqdq,
         2 => dtes64,
         3 => monitor,
         4 => ds_cpl,
         5 => vmx,
         6 => smx,
         7 => eist,
         8 => tm2,
         9 => ssse3,
        10 => cnxt_id,
        11 => sdbg,
        12 => fma,
        13 => cmpxchg16b,
        14 => xtpr_update_control,
        15 => pdcm,
        // 16 - reserved
        17 => pcid,
        18 => dca,
        19 => sse4_1,
        20 => sse4_2,
        21 => x2apic,
        22 => movbe,
        23 => popcnt,
        24 => tsc_deadline,
        25 => aesni,
        26 => xsave,
        27 => osxsave,
        28 => avx,
        29 => f16c,
        30 => rdrand
        // 31 - unused
    });

    bit!(edx, {
        0 => fpu,
        1 => vme,
        2 => de,
        3 => pse,
        4 => tsc,
        5 => msr,
        6 => pae,
        7 => mce,
        8 => cx8,
        9 => apic,
        // 10 - reserved
        11 => sep,
        12 => mtrr,
        13 => pge,
        14 => mca,
        15 => cmov,
        16 => pat,
        17 => pse_36,
        18 => psn,
        19 => clfsh,
        // 20 - reserved
        21 => ds,
        22 => acpi,
        23 => mmx,
        24 => fxsr,
        25 => sse,
        26 => sse2,
        27 => ss,
        28 => htt,
        29 => tm,
        // 30 -reserved
        31 => pbe
    });
}

impl fmt::Debug for VersionInformation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, "VersionInformation", {
            family_id,
            model_id,
            stepping,
            brand_string,
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

#[derive(Copy,Clone)]
struct ExtendedProcessorSignature {
    ecx: u32,
    edx: u32,
}

impl ExtendedProcessorSignature {
    fn new() -> ExtendedProcessorSignature {
        let (_, _, c, d) = cpuid(RequestType::ExtendedProcessorSignature);
        ExtendedProcessorSignature { ecx: c, edx: d }
    }

    bit!(ecx, {
        0 => lahf_sahf_in_64_bit,
        // 1-4 reserved
        5 => lzcnt,
        // 6-7 reserved
        8 => prefetchw
        // 9-31 reserved
    });

    bit!(edx, {
        // 0-10 reserved
        11 => syscall_sysret_in_64_bit,
        // 12-19 reserved
        20 => execute_disable,
        // 21-25 reserved
        26 => gigabyte_pages,
        27 => rdtscp_and_ia32_tsc_aux,
        // 28 reserved
        29 => intel_64_bit_architecture
        // 30-31 reserved
    });
}

impl fmt::Debug for ExtendedProcessorSignature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, "ThermalPowerManagementInformation", {
            lahf_sahf_in_64_bit,
            lzcnt,
            prefetchw,
            syscall_sysret_in_64_bit,
            execute_disable,
            gigabyte_pages,
            rdtscp_and_ia32_tsc_aux,
            intel_64_bit_architecture
        })
    }
}

// 3 calls of 4 registers of 4 bytes
const BRAND_STRING_LENGTH: usize = 3 * 4 * 4;

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

    bit!(eax, {
        0 => digital_temperature_sensor,
        1 => intel_turbo_boost,
        2 => arat,
        // 3 - reserved
        4 => pln,
        5 => ecmd,
        6 => ptm,
        7 => hwp,
        8 => hwp_notification,
        9 => hwp_activity_window,
        10 => hwp_energy_performance_preference,
        // 12 - reserved
        13 => hdc
    });

    pub fn number_of_interrupt_thresholds(self) -> u32 {
        bits_of(self.ebx, 0, 3)
    }

    bit!(ecx, {
        0 => hardware_coordination_feedback,
        // 1-2 - reserved
        3 => performance_energy_bias
    });
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

    bit!(ebx, {
        0 => fsgsbase,
        1 => ia32_tsc_adjust_msr,
        // 2 - reserved
        3 => bmi1,
        4 => hle,
        5 => avx2,
        // 6 - reserved
        7 => smep,
        8 => bmi2,
        9 => enhanced_rep_movsb_stosb,
        10 => invpcid,
        11 => rtm,
        12 => pqm,
        13 => deprecates_fpu_cs_ds,
        // 14 - reserved
        15 => pqe,
        // 16-17 - reserved
        18 => rdseed,
        19 => adx,
        20 => smap,
        // 21-24 - reserved
        25 => intel_processor_trace
        // 26-31 - reserved
    });

    bit!(ecx, {
        0 => prefetchwt1
    });
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

#[derive(Debug, Copy, Clone)]
pub enum CacheLineAssociativity {
    Disabled,
    DirectMapped,
    TwoWay,
    FourWay,
    EightWay,
    SixteenWay,
    Full,
}

#[derive(Copy, Clone)]
pub struct CacheLine(u32);

impl CacheLine {
    fn new() -> CacheLine {
        let (_, _, c, _) = cpuid(RequestType::CacheLine);
        CacheLine(c)
    }

    pub fn cache_line_size(self) -> u32 {
        bits_of(self.0, 0, 7)
    }

    pub fn l2_associativity(self) -> Option<CacheLineAssociativity> {
        match bits_of(self.0, 12, 15) {
            0x00 => Some(CacheLineAssociativity::Disabled),
            0x01 => Some(CacheLineAssociativity::DirectMapped),
            0x02 => Some(CacheLineAssociativity::TwoWay),
            0x04 => Some(CacheLineAssociativity::FourWay),
            0x06 => Some(CacheLineAssociativity::EightWay),
            0x08 => Some(CacheLineAssociativity::SixteenWay),
            0x0F => Some(CacheLineAssociativity::Full),
            _ => None,
        }
    }

    pub fn cache_size(self) -> u32 {
        bits_of(self.0, 16, 31)
    }
}

impl fmt::Debug for CacheLine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, "CacheLine", {
            cache_line_size,
            l2_associativity,
            cache_size
        })
    }
}

#[derive(Copy, Clone)]
pub struct TimeStampCounter {
    edx: u32,
}

impl TimeStampCounter {
    fn new() -> TimeStampCounter {
        let (_, _, _, d) = cpuid(RequestType::TimeStampCounter);
        TimeStampCounter { edx: d }
    }

    bit!(edx, {
        // 0-7 - reserved
        8 => invariant_tsc
        // 9-31 - reserved
    });
}

impl fmt::Debug for TimeStampCounter {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        dump!(self, f, "TimeStampCounter", {
            invariant_tsc
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

/// Information about the currently running processor
///
/// Feature flags match the feature mnemonic listed in the Intel
/// Instruction Set Reference. This struct provides a facade for flags
/// so the consumer doesn't need to worry about which particular CPUID
/// leaf provides the information.
///
/// For data beyond simple feature flags, you will need to retrieve
/// the nested struct and call the appropriate methods on it.
#[derive(Debug,Clone)]
pub struct Master {
    // TODO: Rename struct
    version_information: Option<VersionInformation>,
    thermal_power_management_information: Option<ThermalPowerManagementInformation>,
    structured_extended_information: Option<StructuredExtendedInformation>,
    extended_processor_signature: Option<ExtendedProcessorSignature>,
    brand_string: Option<BrandString>,
    cache_line: Option<CacheLine>,
    time_stamp_counter: Option<TimeStampCounter>,
    physical_address_size: Option<PhysicalAddressSize>,
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

        let eps = when_supported(max_value, RequestType::ExtendedProcessorSignature, || {
            ExtendedProcessorSignature::new()
        });
        let brand_string = when_supported(max_value, RequestType::BrandString3, || {
            BrandString::new()
        });
        let cache_line = when_supported(max_value, RequestType::CacheLine, || {
            CacheLine::new()
        });
        let tsc = when_supported(max_value, RequestType::TimeStampCounter, || {
            TimeStampCounter::new()
        });
        let pas = when_supported(max_value, RequestType::PhysicalAddressSize, || {
            PhysicalAddressSize::new()
        });

        Master {
            version_information: vi,
            thermal_power_management_information: tpm,
            structured_extended_information: sei,
            extended_processor_signature: eps,
            brand_string: brand_string,
            cache_line: cache_line,
            time_stamp_counter: tsc,
            physical_address_size: pas,
        }
    }

    pub fn brand_string(&self) -> Option<&str> {
        self.brand_string.as_ref().map(|bs| bs as &str).or({
            self.version_information.and_then(|vi| vi.brand_string())
        })
    }

    delegate_flag!(version_information, {
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
    });

    delegate_flag!(thermal_power_management_information, {
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
        hardware_coordination_feedback,
        performance_energy_bias
    });

    delegate_flag!(structured_extended_information, {
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
    });

    delegate_flag!(extended_processor_signature, {
        lahf_sahf_in_64_bit,
        lzcnt,
        prefetchw,
        syscall_sysret_in_64_bit,
        execute_disable,
        gigabyte_pages,
        rdtscp_and_ia32_tsc_aux,
        intel_64_bit_architecture
    });

    delegate_flag!(time_stamp_counter, {
        invariant_tsc
    });
}

/// The main entrypoint to the CPU information
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
    assert!(master().brand_string().unwrap().contains("Intel(R)"))
}
