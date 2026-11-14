"this is a comment!"

#[extern] i32.+(r i32): i32;

add(l, r): i32 =
    l + r;

i32.twice: i32 = self + self;

five: i32 = 1 twice + 3;

type bool = match True, False;

maybe(:left? bool, :right? bool) =
    match left?
        True = 1,
        False = (match right?
            True = 2,
            False = 0);

five-: i32 =
    maybe(:left? False, :right? True) +
    maybe(:left? True, :right? True);

module Array = import Std.Array;
module IntArray = Array(:Item type i32);

using IntArray;

large-fnptr = #IntArray.t.twice;

"evil cast syntax"
i32.as: pass = pass;
