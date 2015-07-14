extern crate cupid;

fn main() {
    println!("{}", cupid::brand_string().trim());
    println!("{:?}", cupid::feature_information());
    let pas = cupid::physical_address_size();
    println!("{}, {}", pas.physical_address_bits(), pas.linear_address_bits());
    println!("{:?}", cupid::structured_extended_information());
    println!("{:?}", cupid::thermal_power_management_information());
    println!("{:?}", cupid::master());
    println!("{:?}", cupid::master().sse3());
    println!("{:?}", cupid::master().avx2());
}
