module Pointer!(type T) = begin
  type t = *T;
  type mutable = *mut T;

  function ref(&from: T): t =
    { A pointer is constructed with a prefix `*`, and dereferenced
      with a prefix `@`. There are no lifetime guarantees. }
    *from;

  { functions and subscripts can be unsafe. }

  subscript unsafe & deref[self: t]: T
    { A pointer dereference creates a projection of the pointer's value,
      so a `&` must be added to obtain a reference. This is an unsafe operation. }
    yield @&self;

  function refmut(&mut from: T): mutable =
    *mut from;

  subscript unsafe &mut deref_mut[self: mutable]: T
    yield @&mut self;

  function unsafe setptr(self: mutable, by: T)
    set self.deref_mut[] = by;
end;

function pointer_example
begin
  module Ptr = Pointer!(i32);

  let five = 5;
  let fmut = mut 5;

  let ptr = Ptr::ref(&five);
  let pmt = Ptr::refmut(&mut fmut);

  unsafe begin
    let _ = ptr.deref[];

    pmt.setptr(five);
  end
end
