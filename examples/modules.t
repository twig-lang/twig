module Thing = {
  type t = i32;

  function new: t = 0;

  function method(self: t): t = self;
};

function thing_example = {
  let thing = Thing.new method;
};

module type HotPotato = {
  type t;

  function new: t;
  function drop(x: t);
};

module Potato: HotPotato = {
  type t = ();

  function new: t = ();
  function drop(_: t) = ();
};

module Game!(P: HotPotato) = {
  function play = {
    let potato = P.new;
    potato drop;
  };
};

module PotatoGame = Game!(Potato);

module type GameType!(P: HotPotato) = {
  function play;
};

module Identity!(type T) = {
  function f(x: T): T = x;
};

function id_f32(x: f32): f32 =
  Identity!(f32).f(x);

module type DoesThing = {
  type t;
  function thing(&x: t);
};

module type DoesAnotherThing = {
  type t;
  function another_thing(&x: t);
};

module ThingDoer!(M: DoesThing + DoesAnotherThing) = {};
