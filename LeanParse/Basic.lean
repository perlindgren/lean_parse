import Std.Internal.Parsec

open Std.Internal.Parsec
open Std.Internal.Parsec.String

inductive Terminal where
  | int : Int    → Terminal
  | id  : String → Terminal
deriving Repr

open Terminal
@[inline]
def parseIntLitTerm : Parser Terminal := do
  return int (← digits)

-- test IntLitTerm
#eval (parseIntLitTerm.run "78")   -- 78
#eval (parseIntLitTerm.run "-3")   -- error expected digit
#eval (parseIntLitTerm.run "0xq")  -- int 0, "xq" will be caught later

@[inline]
def parseHexLitTerm : Parser Terminal := do
  let _ ← pstring "0x"
  return int ((← many1 hexDigit).reverse.foldr toHex 0)
  where toHex c acc : Int :=
    acc * 16 +
    if ('0' ≤ c ∧ c ≤ '9') then
      c.toNat - '0'.toNat
    else if ('a' ≤ c ∧ c ≤ 'f') then
      10 + c.toNat - 'a'.toNat
    else
      10 + c.toNat - 'A'.toNat

-- test HexLitTerm
#eval (parseHexLitTerm.run "0x7")
#eval (parseHexLitTerm.run "0xc")
#eval (parseHexLitTerm.run "0xF")
#eval (parseHexLitTerm.run "0x10")
#eval (parseHexLitTerm.run "x10") -- error. expected 0x
#eval (parseHexLitTerm.run "0xq") -- error. hex digit expected

@[inline]
def parseIntTerm : Parser Terminal := do
  if (← peek!) == '-' then
    skip
    let r ← parseHexLitTerm <|> parseIntLitTerm
    match r with
    | int v => return int (-v)
    | _ => panic! "-- unreachable --"
  else
    return ← attempt parseHexLitTerm <|> parseIntLitTerm

#eval (parseIntTerm.run "0")
#eval (parseIntTerm.run "0x7")
#eval (parseIntTerm.run "-7")
#eval (parseIntTerm.run "-0x10")
#eval (parseIntTerm.run "0xq")   -- int 0, "xq" will be caught later


@[inline]
def parseIdTerm : Parser Terminal := do
  let h ← asciiLetter <|> pchar '_'
  let t ← many (asciiLetter <|> digit <|> pchar '_')
  return id (String.mk ([h] ++ t.toList))

#eval (parseIdTerm.run "abc")   -- id "abc"
#eval (parseIdTerm.run "7abc")  -- expected _, not great
#eval (parseIdTerm.run "=")     -- expected _, not great

@[inline]
def parseTerm : Parser Terminal := do
  return ← (attempt parseIntTerm <|> parseIdTerm)

#eval (parseTerm.run "7")         -- int 7
#eval (parseTerm.run "0x17")      -- int 23
#eval (parseIntTerm.run "-7")     -- int -7
#eval (parseIntTerm.run "-0x10")  -- int -16
#eval (parseTerm.run "0xq")       -- int 0, "xq" will be caught later
#eval (parseTerm.run "7abc")      -- int 7, "abc" will be caught later
#eval (parseTerm.run "abc")       -- id "abc"
#eval (parseTerm.run "abc7")      -- id "abc7"
#eval (parseTerm.run "ab_c7")     -- id "ab_c7"
#eval (parseTerm.run "_abc7")     -- id "_abc7"
#eval (parseIdTerm.run "=")       -- expected _, not great
