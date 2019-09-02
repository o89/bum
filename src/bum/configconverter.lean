import bum.auxiliary bum.configparser

def getExt (filename : String) : String × String :=
let byDot := filename.split ".";
if byDot.length > 1 then
  (String.join byDot.init, byDot.getLast)
else (filename, "")

def confGetString (s : String) (default : Option String) :
  List Val → Except String String
| hd :: tl ⇒
  match hd with
  | Val.pair (Val.string name, val) ⇒
    if name = s then val.getString
    else confGetString tl
  | _ ⇒ confGetString tl
| [] ⇒
  match default with
  | none   ⇒ Except.error ("variable “" ++ s ++ "” not found")
  | some v ⇒ Except.ok v

def confGetBuildType : List Val → Except String BuildType
| hd :: tl ⇒
  match hd with
  | Val.pair (Val.string "build", Val.string val) ⇒
    if val.isPrefixOf "executable" then
      Except.ok BuildType.executable
    else if val.isPrefixOf "library" then
      Except.ok BuildType.library
    else Except.error "invalid “build” value"
  | Val.pair (Val.string "build", _) ⇒
    Except.error "invalid “build” value"
  | _ ⇒ confGetBuildType tl
| _ ⇒ Except.ok BuildType.executable

def parseRepo : Val → Except String Dep
| Val.pair (Val.string name, Val.pair (Val.string "git", Val.string url)) ⇒
  Except.ok ⟨name, Repo.git url⟩
| _ ⇒ Except.error "unknown repository source"

def parseRepos (xs : List Val) : Except String (List Dep) :=
sequence (parseRepo <$> xs)

def confGetCppLibs : List Val → Except String (List String)
| hd :: tl ⇒
  match hd with
  | Val.pair (Val.string "cpp_libs", Val.list libs) ⇒
    sequence (Val.getString <$> libs)
  | _ ⇒ confGetCppLibs tl
| _ ⇒ Except.ok []

def confGetDeps : List Val → Except String (List Dep)
| hd :: tl ⇒
  match hd with
  | Val.pair (Val.string "deps", Val.list deps) ⇒ parseRepos deps
  | _ ⇒ confGetDeps tl
| _ ⇒ Except.ok []

def getSource : Val → Except String Source
| Val.string filename ⇒
  match getExt filename with
  | (path, "lean") ⇒ Except.ok (Source.lean [".", "src", path].joinPath)
  | (path, "cpp")  ⇒ Except.ok (Source.cpp [".", "src", path].joinPath)
  | _              ⇒ Except.error $ "“" ++ filename ++ "” is not Lean or C++ source"
| _ ⇒ Except.error "filename must be a string"

def confGetFiles : List Val → Except String (List Source)
| hd :: tl ⇒
  match hd with
  | Val.pair (Val.string "files", Val.list lst) ⇒
    match sequence (getSource <$> lst) with
    | Except.ok val    ⇒ Except.ok val
    | Except.error err ⇒ Except.error err
  | _ ⇒ confGetFiles tl
| _ ⇒ Except.error "sources list not found"

def confToProject (xs : List Val) : Except String Project := do
  name ← confGetString "app" none xs;
  buildType ← confGetBuildType xs;
  deps ← confGetDeps xs;
  files ← confGetFiles xs;
  depsDir ← confGetString "deps_dir" (some "deps") xs;
  cppLibs ← confGetCppLibs xs;
  pure ⟨buildType, name, files, deps, depsDir, cppLibs⟩

def readConf (filename : String) := do
  contents ← IO.readTextFile filename;
  conf ← IO.ofExcept (Parser.run confParser contents);
  IO.ofExcept (confToProject conf)
