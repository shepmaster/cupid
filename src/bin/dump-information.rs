extern crate cpuid;

fn main() {
    println!("{}", cpuid::brand_string().trim());
    println!("{}", cpuid::feature_information());
}
