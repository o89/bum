import bump.types bump.parser

def whitespace := sat Char.isWhitespace

def strSat := str ∘ sat

def reserved := ",[]{} \n\t\""
def quote := Char.ofNat 34
def tok :=
(do ch quote; str ← strSat (not ∘ HasBeq.beq quote);
    ch quote; pure str) <|>
strSat (not ∘ reserved.contains)

def gappy {α : Type} (p : Parser α) : Parser α :=
do many whitespace; x ← p; many whitespace; pure x

def listParser {α : Type} (p : Parser α) : Parser (List α) := do
  ch '['; many whitespace;
  tokens ← sepBy (ch ',') (gappy p);
  ch ']'; pure tokens

def pairParser {α β : Type} (p : Parser α) (q : Parser β) : Parser (α × β) := do
  gappy (ch '{'); name ← p;
  gappy (ch ','); value ← q;
  gappy (ch '}');
  pure (name, value)

def valAux (val : Parser Val) : Parser Val :=
(Val.list <$> listParser val) <|>
(Val.pair <$> pairParser val val) <|>
(Val.string <$> tok)

def val := Parser.fix valAux

def confParser : Parser (List Val) :=
many (many whitespace *> val <* ch '.' <* many whitespace)
