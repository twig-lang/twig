"this is a comment!"

#[extern] (i32) + i32: i32;

fn add(l, r): i32 = l + r;

fn (i32) twice: i32 = self + self;
const five: i32 = 1 twice + 3;

type bool = match True, False;

fn maybe(:left? bool, :right? bool) =
    match left?
        True = 1,
        False = 0;

const one: i32 = maybe(:left? False, :right? True) + maybe(:left? True, :right? True);

module Array = import Std.Array;
module IntArray = Array(:Item type i32);

using IntArray;

const large-fnptr = #IntArray.t.twice;

"evil cast syntax"
fn (i32) as: pass = pass;

fn (IntArray.t) [x: i32]: i32 = pass;

const example = IntArray.filled(:length 16, :value 1)[1];

sig BitNot = (
  type t;
  fn (t) not: t);

sig BitOr = (
  type t;
  fn (t) | t: t);

module Nor(M: BitOr + BitNot) = (
    type t = M.t;
    (t) |~ (other: t): t = (self | other) not;
);
