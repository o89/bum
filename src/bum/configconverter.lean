import bum.auxiliary
import bum.configparser

def getExt (filename : String) : String × String :=
let byDot := filename.splitOn ".";
if byDot.length > 1 then
  (String.join byDot.init, byDot.getLast!)
else (filename, "")

def confGetString (s : String) (inh : Option String) :
  List Val → Except String String
| hd :: tl =>
  match hd with
  | Val.pair (Val.string name, val) =>
    if name = s then val.getString
    else confGetString s inh tl
  | _ => confGetString s inh tl
| [] =>
  match inh with
  | none   => Except.error ("variable “" ++ s ++ "” not found")
  | some v => Except.ok v

def confGetBuildType : List Val → Except String BuildType
| hd :: tl =>
  match hd with
  | Val.pair (Val.string "build", Val.string val) =>
    if val.isPrefixOf "executable" then
      Except.ok BuildType.executable
    else if val.isPrefixOf "library" then
      Except.ok BuildType.library
    else Except.error "invalid “build” value"
  | Val.pair (Val.string "build", _) =>
    Except.error "invalid “build” value"
  | _ => confGetBuildType tl
| _ => Except.ok BuildType.executable

def parseRepo : Val → Except String Dep
| Val.pair (Val.string name, Val.pair (Val.string repo, Val.string url)) =>
  match repo with
  | "git" => Except.ok ⟨name, Repo.git url⟩
  | "github" => Except.ok ⟨name, Repo.github url⟩
  | _ => Except.error "unknown repository source"
| _ => Except.error "invalid dependency"

-- ???
instance : Functor List :=
{ map := List.map }

def parseRepos (xs : List Val) : Except String (List Dep) :=
sequence (parseRepo <$> xs)

def getStringList (id : String) : List Val → Except String (List String)
| hd :: tl =>
  match hd with
  | Val.pair (Val.string id', Val.list libs) =>
    if id = id' then sequence (Val.getString <$> libs)
    else getStringList id tl
  | _ => getStringList id tl
| _ => Except.ok []

def confGetDeps : List Val → Except String (List Dep)
| hd :: tl =>
  match hd with
  | Val.pair (Val.string "deps", Val.list deps) => parseRepos deps
  | _ => confGetDeps tl
| _ => Except.ok []

def getSource (dir : String) : Val → Except String Source
| Val.string filename =>
  match getExt filename with
  | (path, "lean") => Except.ok (Source.lean [dir, path].joinPath)
  | (path, "cpp")  => Except.ok (Source.cpp [dir, path].joinPath)
  | _              => Except.error $ "“" ++ filename ++ "” is not Lean or C++ source"
| _ => Except.error "filename must be a string"

def confGetFiles (dir : String) : List Val → Except String (List Source)
| hd :: tl =>
  match hd with
  | Val.pair (Val.string "files", Val.list lst) =>
    match sequence (getSource dir <$> lst) with
    | Except.ok val    => Except.ok val
    | Except.error err => Except.error err
  | _ => confGetFiles dir tl
| _ => Except.error "sources list not found"

def confToProject (xs : List Val) : Except String Project := do
  let srcDir ← confGetString "src-dir" (some "src") xs;
  let depsDir ← confGetString "deps-dir" (some "deps") xs;

  let name ← confGetString "app" none xs;
  let buildType ← confGetBuildType xs;

  let deps ← confGetDeps xs;
  let files ← confGetFiles srcDir xs;
  let cppLibs ← getStringList "cpp-libs" xs;
  let cppFlags ← getStringList "cpp-flags" xs;
  pure ⟨buildType, name, files, deps, depsDir, srcDir, cppLibs, cppFlags⟩

def readConf (filename : String) : IO Project := do
  let contents ← IO.FS.readFile filename;
  let conf ← IO.ofExcept (Parser.run confParser contents);
  IO.ofExcept (confToProject conf)