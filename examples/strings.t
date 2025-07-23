{ A list of a few string escapes. }

extern "C" function puts(text: *u8): i32;

{ `str` is an opaque alias of a byte slice of UTF-8 encoded text. }
function escapes: str =
  { adjacent string literals are concatenated. }
  "newline: \n"
  "tab: \t"
  "quotes: \""
  "quote: \'"
  "cr: \r"

  "also_newline: \x0a"
  "and_quotes: \u{0022}"
  "and_quote: \u{0_0_2_7}"

  "with_newlines: Hello,
world!" { indentation would be included in the string. }

  r#"raw: "text strings" may include quotations or es\ca\pes."#
  r##"also_raw: also #" and "# can be used in raw strings."##

  "null: \0"
;

{ b"..." constructs a byte string, which has type []u8.
  It doesn't need to be encoded in UTF-8.  }
function bytes: []u8 =
  { also note that strings don't have an implicit \0 character as
    in C. }

  { byte strings can also be raw, as in: }
  rb#""Hello, world!","#

  b" said the program\n\0";

function main
begin
  let text = escapes();
  let text_ptr = text.as_bytes[].data;

  { Operations across language boundaries are always unsafe. }
  unsafe puts(text_ptr);

  let hello = bytes();
  let hello_ptr = hello.data;

  unsafe puts(hello_ptr);
end
