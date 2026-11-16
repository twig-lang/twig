mod ast;
mod combinator;
mod lexer;
mod lexer_wrapper;
mod parse;
mod token;

lalrpop_util::lalrpop_mod!(grammar);

use std::fs::read_to_string;

fn main() -> std::io::Result<()> {
    let file = read_to_string("thing.tl")?;

    for t in lexer::Lexer::new(&file) {
        println!("{:?}", t);
    }

    Ok(())
}
