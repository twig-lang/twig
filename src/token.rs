use logos::{Logos, SpannedIter};

#[derive(Debug, PartialEq, Clone, Default)]
pub enum Error {
    #[default]
    Invalid,

    InvalidFloat(std::num::ParseFloatError),

    InvalidInt(std::num::ParseIntError),
}

impl From<std::num::ParseFloatError> for Error {
    fn from(value: std::num::ParseFloatError) -> Self {
        Self::InvalidFloat(value)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(value: std::num::ParseIntError) -> Self {
        Self::InvalidInt(value)
    }
}

#[derive(Debug, Clone, Logos)]
pub enum IntegerHint {
    #[token("u8")]
    U8,
    #[token("u16")]
    U16,
    #[token("u32")]
    U32,
    #[token("u64")]
    U64,
    #[token("i8")]
    I8,
    #[token("i16")]
    I16,
    #[token("i32")]
    I32,
    #[token("i64")]
    I64,
}

#[derive(Debug, Clone, Logos)]
pub enum FloatHint {
    #[token("f32")]
    F32,

    #[token("f64")]
    F64,
}

fn float_cb(
    l: &mut logos::Lexer<Token>,
) -> Result<(f64, Option<FloatHint>), std::num::ParseFloatError> {
    let value = l.slice().parse()?;

    let mut inner_lexer = l.clone().morph::<FloatHint>();

    let (got, n) = match inner_lexer.next() {
        Some(Ok(t)) => (Some(t), inner_lexer.slice().len()),
        _ => (None, 0),
    };

    l.bump(n);

    Ok((value, got))
}

fn int_cb(
    l: &mut logos::Lexer<Token>,
) -> Result<(u64, Option<IntegerHint>), std::num::ParseIntError> {
    let value = l.slice().parse()?;

    let mut inner_lexer = l.clone().morph::<IntegerHint>();

    let (got, n) = match inner_lexer.next() {
        Some(Ok(t)) => (Some(t), inner_lexer.slice().len()),
        _ => (None, 0),
    };

    l.bump(n);

    Ok((value, got))
}

#[derive(Debug, Clone, Logos)]
#[logos(error = Error)]
#[logos(skip r"[ \r\n\t]+")]
pub enum Token {
    #[regex(r#""[^"]*""#)]
    Comment,

    #[regex(r"[_\p{letter}][\w_-]*[!?#]?", |l| l.slice().to_string())]
    Identifier(String),

    #[regex(r"[+\-@^=<>|&$%*/][+\-@^=<>|&$%*/.~!]*", priority = 1)]
    BinaryOperator,

    #[regex(r"[#!~][+\-@^=<>|&$%*/.~!]*", priority = 1)]
    UnaryOperator,

    #[regex(r"\d+", int_cb)]
    Integer((u64, Option<IntegerHint>)),

    #[regex(r"\d+\.\d+", float_cb)]
    Float((f64, Option<FloatHint>)),

    #[regex(r"`.`")]
    Character,

    #[regex(r"'([^'])*'")]
    String,

    #[token(r"(")]
    LParen,
    #[token(r")")]
    RParen,
    #[token(r"[")]
    LBrac,
    #[token(r"]")]
    RBrac,
    #[token(r"{")]
    LCurl,
    #[token(r"}")]
    RCurl,
    #[token(r"=")]
    Equal,
    #[token(r"#")]
    Hash,
    #[token(r",")]
    Comma,
    #[token(r"@")]
    At,
    #[token(r"&")]
    Ref,
    #[token(r";")]
    Semicolon,
    #[token(r":")]
    Colon,
    #[token(r".")]
    Dot,
    #[token(r"module")]
    Module,
    #[token(r"sig")]
    Sig,
    #[token(r"import")]
    Import,
    #[token(r"type")]
    Type,
    #[token(r"var")]
    Var,
    #[token(r"const")]
    Const,
    #[token(r"match")]
    Match,
    #[token(r"with")]
    With,
    #[token(r"set")]
    Set,
    #[token(r"label")]
    Label,
    #[token(r"break")]
    Break,
    #[token(r"as")]
    As,
    #[token(r"pass")]
    Pass,
    #[token(r"fn")]
    Fn,
    #[token(r"extends")]
    Extends,
    #[token(r"self")]
    Slf,
    #[token(r"using")]
    Using,
    #[token(r"defer")]
    Defer,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Comment => "*comment*",
            Self::Identifier(text) => &text,
            Self::BinaryOperator => "operator",
            Self::UnaryOperator => "operator",
            Self::Integer((n, _)) => return f.write_fmt(format_args!("{n}")),
            Self::Float((n, _)) => return f.write_fmt(format_args!("{n}")),
            Self::Character => "char",
            Self::String => "string",
            Self::LParen => "(",
            Self::RParen => ")",
            Self::LBrac => "[",
            Self::RBrac => "]",
            Self::LCurl => "{",
            Self::RCurl => "}",
            Self::Equal => "=",
            Self::Hash => "#",
            Self::Comma => ",",
            Self::At => "@",
            Self::Ref => "&",
            Self::Semicolon => ";",
            Self::Colon => ":",
            Self::Dot => ".",
            Self::Module => "module",
            Self::Sig => "sig",
            Self::Import => "import",
            Self::Type => "type",
            Self::Var => "var",
            Self::Const => "const",
            Self::Match => "match",
            Self::With => "with",
            Self::Set => "set",
            Self::Label => "label",
            Self::Break => "break",
            Self::As => "as",
            Self::Pass => "pass",
            Self::Fn => "fn",
            Self::Extends => "extends",
            Self::Slf => "self",
            Self::Using => "using",
            Self::Defer => "defer",
        })
    }
}

pub type Spanned<T, L, E> = Result<(L, T, L), E>;

pub struct Lexer<'i> {
    stream: SpannedIter<'i, Token>,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'i> Iterator for Lexer<'i> {
    type Item = Spanned<Token, usize, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.stream.next().map(|(t, s)| Ok((s.start, t?, s.end)))
    }
}
