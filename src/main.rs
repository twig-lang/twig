mod ast;
mod token;

lalrpop_util::lalrpop_mod!(grammar);

use std::fs::read_to_string;

fn main() -> std::io::Result<()> {
    let file = read_to_string("thing.tl")?;
    let lexer = token::Lexer::new(&file);
    let parser = grammar::GrammarParser::new();
    let ast = parser.parse(lexer).unwrap();

    println!("{ast:?}");

    Ok(())
}
