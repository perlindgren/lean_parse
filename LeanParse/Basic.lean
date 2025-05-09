import Init.System.IO
import Std.Internal.Parsec

open Std.Internal.Parsec
open Std.Internal.Parsec.ByteArray

def parseZero : Parser Unit := skipByteChar '0'

@[inline]
def parsePos : Parser Nat := do
  let ident ← digits
  if ident == 0 then
    fail "id was 0"
  else
    return ident

@[inline]
def parseId : Parser Nat := parsePos

def parseIdList : Parser (Array Nat) := do
  many idWs
where
  @[inline]
  idWs : Parser Nat := do
    let ident ← attempt parseId
    skipByteChar ' '
    return ident
