extern crate cpuid;

fn main() {
    println!("{}", cpuid::feature_information())
}
