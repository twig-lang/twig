"this is a comment!"

#[extern] decl i32.+(r i32): i32;

fn add(l, r): i32 =
    l + r;

fn i32.twice: i32 = self + self;

fn five: i32 = 2 + 3;

fn maybe(:left? bool, :right? bool) =
    match left?
        True = 1,
        False = (match right?
            True = 2,
            False = 0);

fn five-: i32 =
    maybe(:left? False, :right? True) +
    maybe(:left? True, :right? True);

module Array = import Std.Array;
module IntArray = Array(:Item type i32);

using IntArray;
