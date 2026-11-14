use crate::token::*;
use phf::phf_map;
use std::{iter::Peekable, num::ParseFloatError, num::ParseIntError, str::Chars};

const PUNCTUATION: &str = "?!+-&/%$^<>*=|@~";

static KEYWORDS: phf::Map<&'static str, Tag> = phf_map! {
    "module"  => Tag::Module,
    "sig"     => Tag::Sig,
    "import"  => Tag::Import,
    "type"    => Tag::Type,
    "var"     => Tag::Var,
    "const"   => Tag::Const,
    "match"   => Tag::Match,
    "with"    => Tag::With,
    "set"     => Tag::Set,
    "label"   => Tag::Label,
    "break"   => Tag::Break,
    "and"     => Tag::And,
    "then"    => Tag::Then,
    "as"      => Tag::As,
    "pass"    => Tag::Pass,
    "fn"      => Tag::Fn,
    "decl"    => Tag::Decl,
    "extends" => Tag::Extends,
    "self"    => Tag::Slf,
    "using"   => Tag::Using,
    "defer"   => Tag::Defer,
};

static OPERATORS: phf::Map<&'static str, Tag> = phf_map! {
    "="  => Tag::Equal,
    "@"  => Tag::At,
};

#[derive(Debug)]
pub enum Error {
    UnclosedComment { start: usize },
    UnclosedString { start: usize },
    UnknownCharacter { at: usize, what: char },
    InvalidInt(ParseIntError),
    InvalidFloat(ParseFloatError),
    InvalidBase,
    InvalidEscape { at: usize },
    InvalidChar { start: usize },
}

pub struct Lexer<'a> {
    text: &'a str,
    head: Peekable<Chars<'a>>,
    position: usize,
}

fn maybe_single_char(c: char) -> Option<Tag> {
    Some(match c {
        '#' => Tag::Hash,
        '(' => Tag::LParen,
        ')' => Tag::RParen,
        '[' => Tag::LBrac,
        ']' => Tag::RBrac,
        '{' => Tag::LCurl,
        '}' => Tag::RCurl,
        ',' => Tag::Comma,
        ';' => Tag::Semicolon,
        ':' => Tag::Colon,
        '.' => Tag::Dot,
        _ => return None,
    })
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            text,
            head: text.chars().peekable(),
            position: 0,
        }
    }

    fn peek_(&mut self) -> Option<char> {
        self.head.peek().copied()
    }

    fn step_(&mut self) -> Option<char> {
        self.head.next().inspect(|_| self.position += 1)
    }

    fn mktoken(&self, tag: Tag, from: usize) -> Token<'a> {
        Token {
            tag,
            span: (from, self.position),
            slice: &self.text[from..self.position],
            extra: None,
        }
    }

    fn mktoken_ex(&self, tag: Tag, from: usize, extra: Extra) -> Token<'a> {
        Token {
            tag,
            span: (from, self.position),
            slice: &self.text[from..self.position],
            extra: Some(extra),
        }
    }

    fn step_while(&mut self, f: impl Fn(char) -> bool) {
        while let Some(c) = self.head.peek() {
            if !f(*c) {
                break;
            }

            self.step_();
        }
    }

    fn skip_ws(&mut self) {
        self.step_while(char::is_whitespace);
    }

    fn comment(&mut self) -> Result<Token<'a>, Error> {
        let here = self.position;
        let mut closed = false;
        self.step_();

        while let Some(c) = self.peek_() {
            if c == '"' {
                closed = true;
                break;
            } else {
                self.step_();
            }
        }

        if !closed {
            Err(Error::UnclosedComment { start: here })
        } else {
            self.step_();
            Ok(self.mktoken(Tag::Comment, here))
        }
    }

    fn word(&mut self) -> Token<'a> {
        let here = self.position;
        let mut tag = Tag::Identifier;

        self.step_while(|c| c.is_alphanumeric() || "_-".contains(c));

        if self.peek_().is_some_and(|c| "?!#".contains(c)) {
            self.step_();
        }

        let slice = &self.text[here..self.position];
        if let Some(t) = KEYWORDS.get(slice) {
            tag = *t;
        }

        self.mktoken(tag, here)
    }

    fn operator(&mut self) -> Token<'a> {
        let here = self.position;
        let mut tag = Tag::Operator;

        self.step_while(|c| PUNCTUATION.contains(c));

        let slice = &self.text[here..self.position];
        if let Some(t) = OPERATORS.get(slice) {
            tag = *t;
        }

        self.mktoken(tag, here)
    }

    // i'm so sorry
    fn number(&mut self) -> Result<Token<'a>, Error> {
        let start = self.position;

        self.step_while(|c| c.is_ascii_digit());

        if self.peek_().is_some_and(|c| c.eq_ignore_ascii_case(&'r')) {
            let base = usize::from_str_radix(&self.text[start..self.position], 10)
                .map_err(|_| Error::InvalidBase)?;

            self.step_();
            let here = self.position;

            self.step_while(|c| {
                "0123456789abcdefghijklmnopqrstuvwxyz"[0..base].contains(c.to_ascii_lowercase())
            });

            let int = u128::from_str_radix(&self.text[here..self.position], base as u32)
                .map_err(Error::InvalidInt)?;

            return Ok(self.mktoken_ex(Tag::Int, start, Extra::Int(int)));
        }

        if self.peek_() == Some('.') {
            self.step_();

            self.step_while(|c| c.is_ascii_digit());

            let f = self.text[start..self.position]
                .parse()
                .map_err(Error::InvalidFloat)?;

            return Ok(self.mktoken_ex(Tag::Float, start, Extra::Float(f)));
        }

        let n = self.text[start..self.position]
            .parse()
            .map_err(Error::InvalidInt)?;

        Ok(self.mktoken_ex(Tag::Int, start, Extra::Int(n)))
    }

    fn step_char_(&mut self, here: usize) -> Result<char, Error> {
        let c = self.step_().ok_or(Error::UnclosedString { start: here })?;

        Ok(if c == '\\' {
            match self.step_().ok_or(Error::UnclosedString { start: here })? {
                '\\' => '\\',
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\'' => '\'',
                '"' => '"',
                '`' => '`',
                _ => return Err(Error::InvalidEscape { at: self.position }),
            }
        } else {
            c
        })
    }

    fn str(&mut self) -> Result<Token<'a>, Error> {
        let here = self.position;
        let mut closed = false;
        let mut st = String::new();
        self.step_();

        while let Some(c) = self.peek_() {
            if c == '\'' {
                closed = true;
                break;
            } else {
                st.push(self.step_char_(here)?)
            }
        }

        if !closed {
            Err(Error::UnclosedString { start: here })
        } else {
            self.step_();
            Ok(self.mktoken_ex(Tag::String, here, Extra::Str(st)))
        }
    }

    fn chr(&mut self) -> Result<Token<'a>, Error> {
        let here = self.position;
        self.step_();

        let chr = self.step_char_(here)?;

        if self.peek_() == Some('`') {
            return Err(Error::InvalidChar { start: here });
        }

        self.step_();

        Ok(self.mktoken_ex(Tag::Char, here, Extra::Char(chr)))
    }

    fn lex(&mut self) -> Result<Token<'a>, Error> {
        match self.peek_() {
            Some(c) => {
                if c.is_whitespace() {
                    self.skip_ws();
                    self.lex()
                } else if c == '"' {
                    self.comment()
                } else if let Some(tag) = maybe_single_char(c) {
                    let here = self.position;
                    self.step_();
                    Ok(self.mktoken(tag, here))
                } else if c.is_alphabetic() || c == '_' {
                    Ok(self.word())
                } else if PUNCTUATION.contains(c) {
                    Ok(self.operator())
                } else if c.is_ascii_digit() {
                    self.number()
                } else if c == '\'' {
                    self.str()
                } else if c == '`' {
                    self.chr()
                } else {
                    let e = Err(Error::UnknownCharacter {
                        at: self.position,
                        what: c,
                    });
                    self.step_();
                    e
                }
            }
            None => Ok(self.mktoken(Tag::Eof, self.position)),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex() {
            Ok(t) => {
                if t.tag == Tag::Eof {
                    None
                } else {
                    Some(Ok(t))
                }
            }
            e => Some(e),
        }
    }
}
