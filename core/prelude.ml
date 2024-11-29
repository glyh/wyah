open Core_frontend.Parser

let prelude =
  {| 
  I x = x;
  K x y = x;
  K1 x y = y;
  S f g x = f x (g x);
  compose f g x = f (g x);
  twice f = compose f f;

  negate x = 0 - x;

  False = Pack{1, 0};
  True = Pack{2, 0};
  and x y = if x y False;
  or x y = if x True y;
  not x = if x False True;
  xor x y = if x (not y) y;

  MkPair = Pack{1, 2};
  fst p = casePair p K;
  snd p = casePair p K1;

  Nil = Pack{1, 0};
  Cons = Pack{2, 2};

  head xs = caseList xs abort head';
  head' x xs = x;

  tail xs = caseList xs abort tail';
  tail' x xs = xs;

  length xs = caseList xs 0 length';
  length' x xs = 1 + length xs;
  |}
  |> parse_string
