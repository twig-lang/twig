// i'm not porting everything as-is, time for combinators or smth

use crate::lexer_wrapper::LexerWrapper;
use crate::token::{Extra, Tag};

pub trait Combinator: Sized {
    type Item;

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item>;

    fn map<U, F: Fn(Self::Item) -> U>(self, f: F) -> Map<Self::Item, U, F, Self> {
        Map(self, f)
    }

    fn star(self) -> Star<Self> {
        Star(self)
    }

    fn separated_by<C: Combinator>(self, sep: C) -> Separated<C, Self> {
        Separated(sep, self)
    }

    fn then<C: Combinator>(self, other: C) -> (Self, C) {
        (self, other)
    }
}

impl<C: Combinator> Combinator for &C {
    type Item = C::Item;

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item> {
        (*self).parse(src)
    }
}

impl<A: Combinator, B: Combinator> Combinator for (A, B) {
    type Item = (A::Item, B::Item);

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item> {
        let a = self.0.parse(src)?;
        let b = self.1.parse(src)?;
        Some((a, b))
    }
}

impl<A: Combinator, B: Combinator, C: Combinator> Combinator for (A, B, C) {
    type Item = (A::Item, B::Item, C::Item);

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item> {
        let a = self.0.parse(src)?;
        let b = self.1.parse(src)?;
        let c = self.2.parse(src)?;
        Some((a, b, c))
    }
}

// not TODO combinators for more tuples

pub struct Map<T, U, F: Fn(T) -> U, C: Combinator<Item = T>>(C, F);

impl<T, U, F: Fn(T) -> U, C: Combinator<Item = T>> Combinator for Map<T, U, F, C> {
    type Item = U;

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item> {
        self.0.parse(src).map(&self.1)
    }
}

pub struct Star<T: Combinator>(pub T);

impl<T: Combinator> Combinator for Star<T> {
    type Item = Vec<T::Item>;

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item> {
        let mut r = Vec::new();

        while let Some(i) = self.0.parse(src) {
            r.push(i);
        }

        Some(r)
    }
}

pub struct Separated<S: Combinator, T: Combinator>(pub S, pub T);

impl<S: Combinator, T: Combinator> Combinator for Separated<S, T> {
    type Item = Vec<T::Item>;

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item> {
        // T {S T}
        let r = (&self.1)
            .then((&self.0, &self.1).map(|(_, x)| x).star())
            .parse(src);

        match r {
            None => None,
            Some((l, mut v)) => {
                v.insert(0, l);
                Some(v)
            }
        }
    }
}

pub struct T(pub Tag);

impl Combinator for T {
    type Item = ();

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item> {
        src.expect(self.0).ok().map(|_| ())
    }
}

impl Combinator for Tag {
    type Item = (Tag, String, (usize, usize), Option<Extra>);

    fn parse<'a>(&self, src: &mut LexerWrapper<'a>) -> Option<Self::Item> {
        let t = src.expect(*self).ok()?;
        Some((t.tag, t.slice.to_string(), t.span, t.extra))
    }
}
