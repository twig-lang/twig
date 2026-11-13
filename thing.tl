"this is a comment!"

#[extern] decl i32.+(l i32, r i32): i32;
#[extern(puts)] decl puts(msg @const u8);

fn add(l, r): i32 =
    l + r;

fn five: i32 = add(2, 3);

fn main =
    puts('Hello, world!');
