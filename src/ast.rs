pub enum Expression {
    Pass,

    Break {
        label: String,
        value: Box<Expression>,
    },
}

pub struct Member {
    pub name: String,
    pub ty: Box<Type>,
}

pub enum Type {
    Pass,

    Named(String),

    Function {
        arguments: Vec<Type>,
        ret: Box<Type>,
    },

    Pointer {
        mutable: bool,
        item: Box<Type>,
    },

    Array {
        length: usize,
        item: Box<Type>,
    },

    Struct(Vec<Member>),
}
