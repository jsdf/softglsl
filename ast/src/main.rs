fn main() {
    // Read the contents of the file
    let contents = std::fs::read_to_string("minimal.glsl").unwrap();
    // Parse the contents of the file
    let ast = glast::parser::parse_from_str(contents.as_str()).unwrap();

    // serialize the AST to JSON
    let json = serde_json::to_string_pretty(&ast).unwrap();

    println!("Hello, world!");
}
