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
#eval (parseIntLitTerm.run "78") -- 78
#eval (parseIntLitTerm.run "-3") -- expected digit

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
#eval (parseHexLitTerm.run "x10") -- expected 0x

@[inline]
def parseIdTerm : Parser Terminal := do
  let s ← many1 (asciiLetter <|> digit <|> pchar '_')
  return id (String.mk (s.toList))

#eval (parseIdTerm.run "abc")   -- id "abc"
#eval (parseIdTerm.run "7abc")  -- id "7abc"
-- Note, the order of combinators ensures "7abc" not to happen
#eval (parseIdTerm.run "=")     -- expected _

@[inline]
def parseTerm : Parser Terminal := do
  return ← (attempt parseHexLitTerm <|> parseIntLitTerm <|> parseIdTerm)

#eval (parseTerm.run "7")     -- int 7
#eval (parseTerm.run "0x17")  -- int 23
#eval (parseTerm.run "0xq")   -- int 0, "xq" will be caught later
#eval (parseTerm.run "abc")   -- id "abc"
#eval (parseTerm.run "abc7")  -- id "abc7"
#eval (parseTerm.run "ab_c7") -- id "ab_c7"
#eval (parseTerm.run "_abc7") -- id "_abc7"
