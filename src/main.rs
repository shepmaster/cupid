extern crate cpuid;

fn main() {
    println!("{}", cpuid::brand_string());
    println!("{}", cpuid::feature_information());
}
