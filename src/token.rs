use logos::Logos;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum Error {
    #[default]
    Invalid,
}

#[derive(Logos)]
#[logos(error = Error)]
pub enum Token2 {
    #[regex(r"[_\a][\w_-]*[!?#]?")]
    Identifier,

    #[regex(r"[@^=<>|&$%*/][@^=<>|&$%*/.]*", priority = 1)]
    BinaryOperator,

    #[regex(r"[#!~][@^=<>|&$%*/]*", priority = 1)]
    UnaryOperator,

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
    #[token(r"decl")]
    Decl,
    #[token(r"extends")]
    Extends,
    #[token(r"self")]
    Slf,
    #[token(r"using")]
    Using,
    #[token(r"defer")]
    Defer,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tag {
    Error,      // <error>
    Eof,        // <end of file>
    Identifier, // <identifier>
    Operator,   // <operator>
    Comment,    // "comment..."
    String,     // 'string...'
    Char,       // `c`
    Int,        // <integer>
    Float,      // <float>
    LParen,     // (
    RParen,     // )
    LBrac,      // [
    RBrac,      // ]
    LCurl,      // {
    RCurl,      // }
    Equal,      // =
    Hash,       // #
    Comma,      // ,
    At,         // @
    Semicolon,  // ;
    Colon,      // :
    Dot,        // .
    Module,     // module
    Sig,        // sig
    Import,     // import
    Type,       // type
    Var,        // var
    Const,      // const
    Match,      // match
    With,       // with
    Set,        // set
    Label,      // label
    Break,      // break
    And,        // and
    Then,       // then
    As,         // as
    Pass,       // pass
    Fn,         // fn
    Decl,       // decl
    Extends,    // extends
    Slf,        // self
    Using,      // using
    Defer,      // defer
}

#[derive(Debug)]
pub enum Extra {
    Str(String),
    Char(char),
    Int(u128),
    Float(f64),
}

#[derive(Debug)]
pub struct Token<'a> {
    pub tag: Tag,
    pub span: (usize, usize),
    pub slice: &'a str,
    pub extra: Option<Extra>,
}
