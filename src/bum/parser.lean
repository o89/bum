import Init.System.IO
import Lean.Parser
import bum.types
import bum.auxiliary
open Lean Lean.Parser

abbrev CanFail := Except String

def Option.err {ε α : Type} (x : ε) : Option α → Except ε α
| some v => Except.ok v
| none   => Except.error x

def CanFail.guard (cond : Bool) (err : String) :=
if ¬cond then Except.error err else Except.ok ()

protected def getNode (name : Name) : Syntax → Option Syntax
| node@(Syntax.node kind _) =>
  if name == kind then some node else none
| _ => none

protected def getArg (s : Syntax) (name : Name) : Option Syntax :=
s.getArgs.findSome? (getNode name)

protected def isDeclValSimple (id : Name) : Syntax → Option Syntax
| Syntax.node `Lean.Parser.Command.declaration args => do
  definition ← args.findSome? (getNode `Lean.Parser.Command.def);

  id' ← getArg definition `Lean.Parser.Command.declId;
  guard (id'.getIdAt 0 == id);

  decl ← getArg definition `Lean.Parser.Command.declValSimple;
  pure decl
| _ => none

protected def getDeclValSimple (id : Name) (s : Syntax) :=
s.getArgs.findSome? (isDeclValSimple id)

protected def getStrLit (s : Syntax) : Option String :=
getArg s `strLit >>= Syntax.isStrLit?

protected def getString (id : Name) (s : Syntax) : CanFail String :=
match getDeclValSimple id s with
| some decl =>
  (getArg decl `Lean.Parser.Term.str >>= getStrLit).err
    ("“" ++ toString id ++ "” is not a string")
| none => Except.error ("“" ++ toString id ++ "” not found")

protected def getApplicationName := getString `app
protected def getDepsDir := getString `depsDir

protected def checkAtomValue (s : String) : Syntax → Bool
| Syntax.atom _ s' => s = s'
| _ => false

protected def isNotComma : Syntax → Bool
| Syntax.atom _ "," => false
| _ => true

protected def isValidFileList (s : Syntax) :=
(Syntax.isStrLit? <$> getArg s `strLit).isSome ||
checkAtomValue "," s

protected def getExt (filename : String) : String × String :=
let byDot := filename.split (λ x => x == '.');
if byDot.length > 1 then
  (String.join byDot.init, byDot.getLast!)
else (filename, "")

protected def getSource (filename : String) : CanFail Source :=
match getExt filename with
| (path, "lean") => Except.ok (Source.lean [".", "src", path].joinPath)
| (path, "cpp")  => Except.ok (Source.cpp [".", "src", path].joinPath)
| _              =>
  Except.error ("“" ++ filename ++ "” is not Lean or C++ source")

protected def getStringList {α : Type} (id : Name)
  (f : String → CanFail α) (s : Syntax) : CanFail (List α) :=
let getStrLitExcept :=
λ s => (getStrLit s).err "expected string";
match getDeclValSimple id s with
| some decl => do
  val ← (getArg decl `Lean.Parser.Term.listLit).err
    ("“" ++ toString id ++ "” must be a list");

  CanFail.guard (val.getNumArgs = 3) "invalid list";
  CanFail.guard (checkAtomValue "[" $ val.getArg 0) "invalid list";
  CanFail.guard (checkAtomValue "]" $ val.getArg 2) "invalid list";

  let lst := (val.getArg 1).getArgs.toList;
  CanFail.guard (lst.all isValidFileList)
    ("“" ++ toString id ++ "” must be a list of string");
  (lst.filter isNotComma).mapM (getStrLitExcept >=> f)
| none => pure []

protected def getFiles := getStringList `files getSource
protected def getCppLibs := getStringList `cppLibs pure
protected def getCppFlags := getStringList `cppFlags pure

protected def getBuildType (s : Syntax) : CanFail BuildType :=
match getDeclValSimple `build s with
| some decl => do
  val ← (getArg decl `Lean.Parser.Term.id).err
    "build type must be an atom";
  match val.getIdAt 0 with
  | `exec => pure BuildType.executable
  | `lib => pure BuildType.library
  | x => throw ("“" ++ toString x ++ "” is unknown build type")
| _ => pure BuildType.executable

protected def getRepo : Syntax → CanFail Repo
| Syntax.node `Lean.Parser.Term.anonymousCtor args => do
  CanFail.guard (args.size = 3) "invalid repository tuple";
  CanFail.guard (checkAtomValue "⟨" $ args.get! 0) "invalid repository tuple";
  CanFail.guard (checkAtomValue "⟩" $ args.get! 2) "invalid repository tuple";

  let val := args.get! 1;

  let repoVar := (val.getArg 0).getIdAt 0;
  CanFail.guard (checkAtomValue "," $ val.getArg 1) "invalid repository tuple";
  url ← (getStrLit $ val.getArg 2).err "URI must be a string";

  match repoVar with
  | `git    => pure (Repo.git url)
  | `github => pure (Repo.github url)
  | repo => throw ("“" ++ toString repo ++ "” is unknown repository type")
| _ => throw "repository description must be a tuple"

protected def getDep : Syntax → CanFail Dep
| Syntax.node `Lean.Parser.Term.anonymousCtor args => do
  CanFail.guard (args.size = 3) "invalid package tuple";
  CanFail.guard (checkAtomValue "⟨" $ args.get! 0) "invalid package tuple";
  CanFail.guard (checkAtomValue "⟩" $ args.get! 2) "invalid package tuple";

  let val := args.get! 1;

  let pkgName := (val.getArg 0).getIdAt 0;
  CanFail.guard (pkgName != Name.anonymous)
    "invalid package name";
  CanFail.guard (checkAtomValue "," $ val.getArg 1)
    "invalid package tuple";
  repo ← getRepo (val.getArg 2);
  pure ⟨toString pkgName, repo⟩
| _ => throw "package description must be a tuple"

protected def getDeps (s : Syntax) : CanFail (List Dep) := do
match getDeclValSimple `deps s with
| some decl => do
  val ← (getArg decl `Lean.Parser.Term.listLit).err
    "dependency list must be a list";

  CanFail.guard (val.getNumArgs = 3)
    "invalid dependency list";
  CanFail.guard (checkAtomValue "[" $ val.getArg 0)
    "invalid dependency list";
  CanFail.guard (checkAtomValue "]" $ val.getArg 2)
    "invalid dependency list";

  let lst := (val.getArg 1).getArgs.toList.filter isNotComma;
  lst.mapM getDep
| none => pure []

def readConf (filename : String) : IO Project := do
  env ← mkEmptyEnvironment;
  s ← parseFile env filename;
  match s with
  | node@(Syntax.node `null args) => do
    name ← IO.ofExcept (getApplicationName node);
    depsDir ← IO.ofExcept (getDepsDir node);

    buildType ← IO.ofExcept (getBuildType node);
    deps ← IO.ofExcept (getDeps node);

    files ← IO.ofExcept (getFiles node);
    cppLibs ← IO.ofExcept (getCppLibs node);
    cppFlags ← IO.ofExcept (getCppFlags node);

    pure ⟨buildType, name, files, deps, depsDir, cppLibs, cppFlags⟩
  | _ => throw (IO.Error.userError "something went wrong")
