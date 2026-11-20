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

// [: label] value
pub struct Argument {
    pub is_label: Option<String>,
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

    // break [name] [ ( expr ) ]
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

    // expr [ idx ]
    Index {
        value: Box<Self>,
        index: Box<Self>,
    },

    // match value {case},
    Match {
        value: Box<Self>,
        cases: Vec<Case>,
    },

    // defer <expr>
    Defer(Box<Self>),

    // expr ; expr (in a parenexpr)
    Sequence {
        before: Box<Self>,
        after: Box<Self>,
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
    Named(Path),

    // fn (type...) -> type
    Function {
        arguments: Vec<Type>,
        ret: Box<Type>,
    },

    // @ [var] type
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

// <mexpr>
// type <ty>
pub enum MArgumentValue {
    M(MExpression),
    T(Type),
}

// [:label] value
pub struct MArgument {
    pub label: Option<String>,
    pub value: Box<MArgumentValue>,
}

pub enum MExpression {
    Variable(Path),
    Mod(Vec<Toplevel>),
    Call(Box<Self>, Vec<MArgument>),
    Import(Path),
}

pub enum MType {
    Variable(Path),
    Call(Box<Self>, Vec<MArgument>),

    // mtype + mtype
    Join(Box<Self>, Box<Self>),

    Sig(Vec<Declaration>),
}

pub enum MParameterType {
    Module(Option<MType>),
    Type,
}

pub struct MParameter {
    pub is_label: bool,
    pub name: String,
    pub ty: MParameterType,
}

pub struct Parameter {
    pub is_label: bool,
    pub name: String,
    pub ty: Type,
}

pub enum FunctionName {
    // name(param...) : ret
    Function {
        name: String,
        parameters: Vec<Parameter>,
    },

    // (recv) name(param...)
    Method {
        recv: Box<Type>,
        name: String,
        parameters: Vec<Parameter>,
    },

    // (recv) binop (param)
    Binary {
        recv: Box<Type>,
        name: String,
        rhs: Parameter,
    },

    // (recv) unop
    Unary {
        recv: Box<Type>,
        name: String,
    },
}

pub enum Declaration {
    AbstractType(String),
    Type { name: String, ty: Box<Type> },
    Value { name: String, ty: Box<Type> },
    Function { name: FunctionName, ty: Box<Type> },
}

pub enum Toplevel {
    Decl(Declaration),

    Attribute {
        head: String,
        arguments: Vec<String>,
        child: Box<Self>,
    },

    Value {
        name: String,
        ty: Option<Box<Type>>,
        val: Box<Expression>,
    },

    Function {
        name: FunctionName,
        val: Box<Expression>,
        ret: Box<Type>,
    },

    // using <path>, a la OCaml open
    Using(Path),

    // extends <path>, a la OCaml includes
    Extends(Path),

    // module name [(params...)] = mexpr
    ModuleDefinition {
        name: String,
        parameters: Vec<MParameter>,
        value: Box<MExpression>,
    },

    // sig name [(params...)] = mty
    SigDefinition {
        name: String,
        parameters: Vec<MParameter>,
        value: Box<MType>,
    },

    // module [(params...)] [: mtype]
    ModuleParameters {
        parameters: Vec<MParameter>,
        ty: Option<MType>,
    },
}
