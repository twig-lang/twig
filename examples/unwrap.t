type MaybeInt = variant
  None,
  Some(i64);

{ only project a value if the variant is Some }
subscript if unwrap[opt: MaybeInt]: i64
  { variant names are not namespaced in the module
    they're defined in, so using MaybeInt.Some here would be
    an error. }

  match opt begin
    case Some(x): yield x;
    else: ;
  end

  { not yielding is an option here! }

function printf(x: i64)
  { assume this function prints a number. }
  return;

function example
begin
  let some = Some(1);
  let none = None;

  { prints 1 }
  let if x = unwrap(some) in
    println(x);

  { does not print anything }
  let if x = unwrap(none) in
    println(x);
end
