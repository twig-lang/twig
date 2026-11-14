/* NOTE-s for later:
 * - an Argument is what is passed into a function, a Parameter is the thing at
 *   the definition
 * - some stuff is "shared" between types, exprs, and modules, we use a prefix
 *   for telling which is which, T for type and M for module
 */

pub enum Path {
    // name
    Atom(String),

    // path . name
    Child(Box<Self>, String),
}

/* NOTE: we don't need to worry about modules with arguments, since those are
   module expressions, which we will just avoid when dealing with this :)
     #Array(:Item type i32).t    # ERROR,
           ^~~~~~~~~~~~~~~~     <- call not allowed
*/

// [:label] value
pub struct Argument {
    pub label: Option<String>,
    pub value: Box<Expression>,
}

pub enum Pattern {
    // <id>
    Name(String),
}

// pattern = expression
pub struct Case(pub Pattern, pub Box<Expression>);

pub enum Expression {
    // pass
    Pass,

    // literals
    Int(u64),
    Float(f64),
    String(String),
    Char(char),

    // break [name] [with expr]
    Break {
        label: Option<String>,
        value: Option<Box<Self>>,
    },

    // # path
    FunctionRef(Path),

    Variable(Path),

    // expr as type
    Cast(Box<Self>, Box<Type>),

    // expr ( arg... )
    Call {
        callee: Box<Self>,
        arguments: Vec<Argument>,
    },

    // match value {case},
    Match {
        value: Box<Self>,
        cases: Vec<Case>,
    },
}

// name ty
pub struct TMember {
    pub name: String,
    pub ty: Box<Type>,
}

pub enum TPattern {
    Name(String),
}

pub enum Type {
    // pass
    Pass,

    // <id>
    Named(String),

    // fn (type...) -> type
    Function {
        arguments: Vec<Type>,
        ret: Box<Type>,
    },

    // @ [const] type
    Pointer {
        mutable: bool,
        item: Box<Type>,
    },

    // [ length ] type
    Array {
        length: usize,
        item: Box<Type>,
    },

    // { {member}, }
    Struct(Vec<TMember>),

    // match {pattern},
    Match(Vec<TPattern>),
}
