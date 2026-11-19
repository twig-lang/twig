use crate::{lexer::*, token::*};
use std::iter::Peekable;

pub struct Unexpected {
    pub expected: Tag,
    pub got: Tag,
    pub span: (usize, usize),
}

pub struct LexerWrapper<'a>(Peekable<Lexer<'a>>);

impl<'a> LexerWrapper<'a> {
    pub fn new(text: &'a str) -> Self {
        Self(Lexer::new(text).peekable())
    }

    pub fn check(&mut self, tag: Tag) -> bool {
        let t = self.0.next();

        let got = match t {
            Some(Ok(t)) => t.tag,
            None => Tag::Eof,
            _ => Tag::Error,
        };

        got == tag
    }

    pub fn eat(&mut self, tag: Tag) -> bool {
        if self.check(tag) {
            self.0.next();
            true
        } else {
            false
        }
    }

    pub fn expect(&mut self, tag: Tag) -> Result<Token<'a>, Unexpected> {
        if self.check(tag) {
            Ok(self.0.next().unwrap().unwrap())
        } else {
            let t = self.0.next();

            let (got, span) = match t {
                Some(Ok(t)) => (t.tag, t.span),
                None => (Tag::Eof, (0, 0)),
                _ => (Tag::Error, (0, 0)),
            };

            Err(Unexpected {
                expected: tag,
                got,
                span,
            })
        }
    }
}

impl<'a> Iterator for LexerWrapper<'a> {
    type Item = Result<Token<'a>, crate::lexer::Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
