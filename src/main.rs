mod ast;
mod combinator;
mod lexer;
mod lexer_wrapper;
mod parse;
mod token;

lalrpop_util::lalrpop_mod!(grammar);

use std::fs::read_to_string;

use logos::Logos;

fn main() -> std::io::Result<()> {
    let file = read_to_string("thing.tl")?;

    let mut lexer = token::Token2::lexer(&file);

    while let Some(t) = lexer.next() {
        match t {
            Ok(t) => {
                println!("{t:?} {:?}", lexer.slice())
            }

            Err(e) => {
                println!("error: {e:?} {:?}", lexer.slice())
            }
        }
    }

    Ok(())
}
