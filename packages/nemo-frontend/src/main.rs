use nemo_frontend::parser;

fn main() {
    parser::highlight("fn main() = { 1 + 29 }");
}
