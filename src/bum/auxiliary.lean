@[extern 2 "lean_io_run_cmd"]
constant IO.runCmd (s : @& String) : IO UInt32 := arbitrary _

@[extern 2 "lean_io_chdir"]
constant IO.chdir (s : @& String) : IO UInt32 := arbitrary _

@[extern 2 "lean_io_remove"]
constant IO.remove (s : @& String) : IO UInt32 := arbitrary _

@[extern 3 "lean_io_set_env"]
constant IO.setEnv (name val : @& String) : IO UInt32 := arbitrary _

def List.joinPath : List String → String :=
String.intercalate (String.singleton System.FilePath.pathSeparator)

def List.space : List String → String :=
String.intercalate " "

def sequence {α : Type} {m : Type → Type} [Monad m] : List (m α) → m (List α)
| (hd :: tl) ⇒ do
  hd' ← hd; tl' ← sequence tl;
  pure (hd' :: tl')
| [] ⇒ pure []

-- ???
instance Monad.HasAndthen {α : Type}
  {m : Type → Type} [Monad m] : HasAndthen (m α) :=
⟨λ a b ⇒ do a; b⟩

def forM' {α β : Type} [Inhabited β] {m : Type → Type} [Monad m]
  (f : α → m β) : List α → m β :=
List.foldr (HasAndthen.andthen ∘ f) (pure $ arbitrary β)

def IO.cond (b : Bool) (action : IO Unit) : IO Unit :=
if b then action else pure ()

def List.uniqAux {α β : Type} [HasBeq β] (f : α → β) : List α → List α → List α
| buff, [] ⇒ buff
| buff, hd :: tl ⇒
  if (f <$> buff).elem (f hd) then
    List.uniqAux buff tl
  else List.uniqAux (hd :: buff) tl

def List.uniq {α β : Type} [HasBeq β] (f : α → β) : List α → List α :=
List.reverse ∘ List.uniqAux f []
