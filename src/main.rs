mod ast;
mod token;

use ariadne::{self};

lalrpop_util::lalrpop_mod!(grammar);

use std::fs::read_to_string;

fn main() -> std::io::Result<()> {
    let mut colors = ariadne::ColorGenerator::new();
    let file = read_to_string("thing.tl")?;
    let lexer = token::Lexer::new(&file);
    let parser = grammar::GrammarParser::new();
    let ast = match parser.parse(lexer) {
        Ok(x) => x,
        Err(e) => {
            use lalrpop_util::ParseError;

            let a = colors.next();

            match e {
                ParseError::InvalidToken { location } => todo!(),
                ParseError::UnrecognizedEof { location, expected } => todo!(),
                ParseError::UnrecognizedToken { token, expected } => ariadne::Report::build(
                    ariadne::ReportKind::Error,
                    ("thing.tl", token.0..token.2),
                )
                .with_message("unexpected token")
                .with_label(
                    ariadne::Label::new(("thing.tl", token.0..token.2))
                        .with_color(a)
                        .with_message(format!("expected {}, got {}", expected.join(", "), token.1)),
                )
                .finish()
                .print(("thing.tl", ariadne::Source::from(file)))?,
                ParseError::ExtraToken { token } => todo!(),
                ParseError::User { error } => todo!(),
            }

            panic!("errors found!");
        }
    };

    println!("{}", ast.len());

    Ok(())
}
