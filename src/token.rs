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
