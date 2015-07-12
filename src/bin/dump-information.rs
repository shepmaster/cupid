extern crate cupid;

fn main() {
    println!("{}", cupid::brand_string().trim());
    println!("{:?}", cupid::feature_information());
}
