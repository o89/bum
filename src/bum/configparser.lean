import bum.types
import bum.parser

def whitespace := sat Char.isWhitespace

def strSat := str ∘ sat

def reserved := ",[]{} \n\t\""
def quote := Char.ofNat 34
def tok :=
(do ch quote; let str ← strSat (not ∘ BEq.beq quote);
    ch quote; pure str) <|>
strSat (not ∘ reserved.contains)

def gappy {α : Type} (p : Parser α) : Parser α :=
do let _ ← many whitespace; let x ← p; let _ ← many whitespace; pure x

def listParser {α : Type} (p : Parser α) : Parser (List α) := do
  let _ ← ch '['; let _ ← many whitespace;
  let tokens ← sepBy (ch ',') (gappy p);
  let _ ← ch ']'; pure tokens

def pairParser {α β : Type} (p : Parser α) (q : Parser β) : Parser (α × β) := do
  gappy (ch '{'); let name ← p;
  gappy (ch ','); let value ← q;
  gappy (ch '}');
  pure (name, value)

def valAux (val : Parser Val) : Parser Val :=
(Val.list <$> listParser val) <|>
(Val.pair <$> pairParser val val) <|>
(Val.string <$> tok)

def val := Parser.fix valAux

def trimWs {α : Type} (p : Parser α) : Parser α := do
  let _ ← many whitespace; let res ← p;
  let _ ← many whitespace; pure res

def confParser : Parser (List Val) :=
many (trimWs (do let res ← val; ch '.'; pure res))