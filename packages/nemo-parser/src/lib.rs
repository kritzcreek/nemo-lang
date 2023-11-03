
use tree_sitter::Parser;
use tree_sitter_nemo;

pub fn parse() {
    let mut parser = Parser::new();
    parser.set_language(tree_sitter_nemo::language()).unwrap();
    let source_code = "hello";
    let tree = parser.parse(source_code, None).unwrap();
    let root_node = tree.root_node();
    println!("{root_node:?}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        parse();
        assert!(false)
    }
}
