@[extern 2 "lean_io_chdir"]
constant IO.chdir (s : @& String) : IO UInt32

@[extern 2 "lean_io_remove"]
constant IO.remove (s : @& String) : IO UInt32

@[extern 2 "lean_get_last_write_time"]
constant IO.getLastWriteTime (path : @& String) : IO UInt64

@[extern 3 "lean_io_set_env"]
constant IO.setEnv (name val : @& String) : IO UInt32

abbrev List.joinPath : List String → String :=
System.mkFilePath

def List.space : List String → String :=
String.intercalate " "

def sequence {α : Type} {m : Type → Type} [Monad m] : List (m α) → m (List α)
| (hd :: tl) => do
  let hd' ← hd; let tl' ← sequence tl;
  pure (hd' :: tl')
| [] => pure []

-- ???
instance Monad.HAndThen {α β : Type} {m : Type → Type}
  [Monad m] : HAndThen (m α) (m β) (m β) :=
⟨λ a b => a >>= λ _ => b⟩

def uniqAux {α β : Type} [BEq β] (f : α → β) : List α → List α → List α
| buff, [] => buff
| buff, hd :: tl =>
  if List.elem (f hd) (List.map f buff) then
    uniqAux f buff tl
  else uniqAux f (hd :: buff) tl

def List.uniq {α β : Type} [BEq β] (f : α → β) : List α → List α :=
List.reverse ∘ uniqAux f []
