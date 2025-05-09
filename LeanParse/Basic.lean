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
  return int ((← many hexDigit).reverse.foldr toHex 0)
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
