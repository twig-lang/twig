module Pointer!(type T) = {
  type t = *const T;
  type mutable = *mut T;

  function ref(&from: T): t =
    *from;

  subscript unsafe & deref[self: t]: T
    yield @&self;

  function refmut(&mut from: T): mutable =
    *mut from;

  subscript unsafe &mut deref_mut[self: mutable]: T
    yield @&mut self;

  function unsafe setptr(self: mutable, by: T)
    set self.deref_mut[] = by;
};

function pointer_example = {
  module Ptr = Pointer!(i32);

  let five = 5;
  let fmut = mut 5;

  let ptr = Ptr::ref(&five);
  let pmt = Ptr::refmut(&mut fmut);

  unsafe {
    let _ = ptr.deref[];

    pmt.setptr(five);
  }
};
