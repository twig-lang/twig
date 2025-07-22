module Thing = begin
  type t = i32;

  function new(): t = 0;

  function my_method(self: t): t = self;
end;

function thing_example
begin
  let thing = Thing
    { 1. A function `new` is looked up in the `Thing` module. }
    ::new()
    { 2. A function `my_method` is looked up in the `Thing`, then `i32` modules. }
    .my_method()
  ;
end

{ A module which exposes an opaque type
  and functions to construct and destroy it. }
module type HotPotato = begin
  type t;

  function new(): t;

  function drop(self: t);
end;

module Potato: HotPotato = begin
  type t = ();

  function new(): t = ();
  function drop(_: t) = ();
end;

{ A module functor, which takes other modules or types as inputs. }
module Game!(P: HotPotato) = begin
  function play()
  begin
    let potato = P::new();
    potato.drop();
  end
end;

{ Can also bind to modules the outputs of functors. }
module PotatoGame = Game!(Potato);

{ And there are module type functors. }
module type GameType!(P: HotPotato) = begin
  function play();
end;

{ Types can be used as functor inputs. }
module Identity!(type T) = begin
  function f(x: T): T = x;
end;

function id_f32(x: f32): f32 =
  Identity!(f32)::f(x);

{ Module types can be "added" together, which can be useful
  for applying more than one constraint at a time }

module type DoesThing = begin
  type t;

  function thing(&self: t);
end;

module type DoesAnotherThing = begin
  type t;

  function another_thing(&self: t);
end;

module ThingDoer!(M: DoesThing + DoesAnotherThing) = begin
  { assume there is something interesting here. }
end;

{ Source code in files also corresponds to modules. To turn a
  file into a functor, a (single) declaration may be added,
  such as:
     { file is Array.t }
     module!(type T);

  which can then be used somewhere else like:
     let array = Array!(u64)::new();

  Note that a declaration such as
     module;

  is allowed. }
