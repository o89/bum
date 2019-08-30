inductive ParseResult (α : Type)
| done (pos : Nat) (result : α) : ParseResult
| fail (pos : Nat) (msg : List String) : ParseResult

def Parser (α : Type) :=
∀ (input : String) (start : Nat), ParseResult α

def Parser.bind (α β : Type) (p : Parser α) (f : α → Parser β) : Parser β :=
λ input pos ⇒ match p input pos with
| ParseResult.done pos a ⇒ f a input pos
| ParseResult.fail _ pos msg ⇒ ParseResult.fail β pos msg

def Parser.pure (α : Type) (a : α) : Parser α :=
λ input pos ⇒ ParseResult.done pos a

def Parser.fail (α : Type) (msg : String) : Parser α :=
λ _ pos ⇒ ParseResult.fail α pos [ msg ]

instance : Monad Parser :=
{ pure := Parser.pure, bind := Parser.bind }

instance : MonadFail Parser :=
{ fail := Parser.fail }

def Parser.failure (α : Type) : Parser α :=
λ _ pos ⇒ ParseResult.fail α pos []

def Parser.orelse (α : Type) (p q : Parser α) : Parser α :=
λ input pos ⇒ match p input pos with
| ParseResult.fail _ pos₁ msg₁ ⇒
  if pos₁ ≠ pos then ParseResult.fail _ pos₁ msg₁
  else match q input pos with
  | ParseResult.fail _ pos₂ msg₂ ⇒
    if pos₁ < pos₂ then ParseResult.fail _ pos₁ msg₁
    else if pos₂ < pos₁ then ParseResult.fail _ pos₂ msg₂
    else ParseResult.fail _ pos₁ (msg₁ ++ msg₂)
  | ok ⇒ ok
| ok ⇒ ok

instance : Alternative Parser :=
{ failure := Parser.failure, orelse := Parser.orelse }

def decorateErrors {α : Type} (msgs : List String) (p : Parser α) : Parser α :=
λ input pos ⇒ match p input pos with
| ParseResult.fail _ _ _ ⇒ ParseResult.fail _ pos msgs
| ok ⇒ ok

def decorateError {α : Type} (msg : String) : Parser α → Parser α :=
decorateErrors [ msg ]

def foldrCore {α β : Type} (f : α → β → β) (p : Parser α) (b : β) : Nat → Parser β
| 0 ⇒ failure
| reps + 1 ⇒
  (do x ← p; xs ← foldrCore reps; pure (f x xs)) <|> pure b

def foldr {α β : Type} (f : α → β → β) (p : Parser α) (b : β) : Parser β :=
λ input pos ⇒ foldrCore f p b (input.length - pos + 1) input pos

def eps : Parser Unit := pure ()

def many {α : Type} (p : Parser α) : Parser (List α) :=
foldr List.cons p []

def many1 {α : Type} (p : Parser α) : Parser (List α) :=
List.cons <$> p <*> many p

def str (p : Parser Char) : Parser String :=
String.mk <$> many p

def sepBy1 {α : Type} (sep : Parser Unit) (p : Parser α) : Parser (List α) :=
List.cons <$> p <*> many (do sep; p)

def sepBy {α : Type} (sep : Parser Unit) (p : Parser α) : Parser (List α) :=
sepBy1 sep p <|> pure []

def Parser.fixCore {α : Type} (F : Parser α → Parser α) : Nat → Parser α
| 0 ⇒ failure
| maxDepth + 1 ⇒ F (Parser.fixCore maxDepth)

def Parser.fix {α : Type} (F : Parser α → Parser α) : Parser α :=
λ input pos ⇒ Parser.fixCore F (input.length - pos + 1) input pos

def sat (p : Char → Bool) : Parser Char :=
λ input pos ⇒
  if pos < input.length then
    let c := input.get pos;
    if p c then ParseResult.done (pos + 1) c
    else ParseResult.fail _ pos []
  else ParseResult.fail _ pos []

def ch (c : Char) : Parser Unit :=
decorateError (String.singleton c) $ sat (λ x ⇒ c = x) *> eps

def remaining : Parser Nat :=
λ input pos ⇒ ParseResult.done pos (input.length - pos)

def eof : Parser Unit :=
decorateError "<end-of-file>" $
do rem ← remaining; guard (rem = 0)

def Parser.run {α : Type} (p : Parser α) (input : String) : Except String α :=
match (p <* eof) input 0 with
| ParseResult.done pos res ⇒ Except.ok res
| ParseResult.fail _ pos msg ⇒
  Except.error ("expected “" ++ String.intercalate "or " msg ++
                "” at " ++ toString pos ++
                " in:\n" ++ input.extract (pos - 10) (pos + 10) ++ "...")
