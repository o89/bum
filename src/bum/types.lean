import bum.auxiliary
open IO.Process (SpawnArgs)
open System (FilePath)

inductive Application
| zero | n2o | nitro

def Application.ofString : String → Except String Application
| "zero"  => Except.ok Application.zero
| "n2o"   => Except.ok Application.n2o
| "nitro" => Except.ok Application.nitro
| s       => Except.error ("unknown template “" ++ s ++"”")

inductive Scale
| current | total
instance : Inhabited Scale := ⟨Scale.current⟩

inductive Command
| app     : Application → Command
| compile : Bool → Command
| clean   : Scale → Command
| olean   : Scale → Bool → Command
| help | nope | deps | start
| leanPath | gitignore

protected def Command.reserved :=
[ "compile", "start", "dep", "clean", "olean", "app", "lean-path", "gitignore" ]

protected def Command.groupAux :
  String × List String → List (String × List String) →
  List String → List (String × List String)
| ("", []), [], [] => []
| (name, options), buf, hd :: tl =>
  if Command.reserved.elem hd then
    Command.groupAux (hd, []) (buf ++ [ (name, options) ]) tl
  else Command.groupAux (name, options ++ [ hd ]) buf tl
| elem, buf, [] => buf ++ [ elem ]

protected def Command.group := Command.groupAux ("", []) []

protected def Command.ofString : String × List String → Except String Command
| ("compile", [])        => Command.compile false
| ("compile", ["force"]) => Command.compile true
| ("lean-path", [])      => Command.leanPath
| ("gitignore", [])      => Command.gitignore
| ("start", [])          => Command.start
| ("dep", [])           => Command.deps
| ("clean", [])          => Command.clean Scale.current
| ("clean", ["recur"])   => Command.clean Scale.total
| ("olean", keys)        =>
  if ¬ keys.all (List.contains ["recur", "force"]) then
    throw s!"olean expects only “recur” and “force” flags"
  else
    let scale := if keys.contains "recur" then Scale.total else Scale.current
    let force := keys.contains "force"
    Command.olean scale force
| ("app", [ template ])  => Command.app <$> Application.ofString template
| ("", _)                => Command.nope
| (cmd, xs)              => throw s!"unknown or malformed command “{cmd} {xs.space}”"

protected def Command.ofList : List String → Except String (List Command) :=
List.mapM Command.ofString ∘ Command.group

def Command.parse : List String → Except String (List Command)
| [] => pure []
| lst@(hd :: _) =>
  if Command.reserved.elem hd then Command.ofList lst
  else throw s!"“{hd}” is not a registered command"

def Command.helpString :=
"BUM Lean 4 build tool

    invoke = bum           | bum list
           | bum lean-path | bum gitignore

      list = [] | command [options] list

   command = app (zero|n2o|nitro)  | dep
           | olean [recur] [force] | compile [force]
           | clean [recur]         | start"

inductive Repo
| github : String → Repo
| git    : String → Repo
| none

-- TODO: remove hard-coded GitHub
def Application.toRepo : Application → Repo
| Application.zero  => Repo.github "o89/sample-zero"
| Application.n2o   => Repo.github "o89/sample-n2o"
| Application.nitro => Repo.github "o89/sample-nitro"

def Repo.cmd (target : String) : Repo → SpawnArgs
| Repo.none        => { cmd := "echo", args := #["-n"] }
| Repo.git url     => { cmd := "git",  args := #["clone", url, target] }
| Repo.github repo => { cmd := "git",  args := #["clone", s!"https://github.com/{repo}", target ] }

structure Dep :=
(name : String) (source : Repo)

def sep := toString FilePath.pathSeparator
def Dep.cmd (depsDir : FilePath) (x : Dep) : SpawnArgs :=
Repo.cmd (toString depsDir ++ sep ++ x.name) x.source

instance : ToString Dep :=
⟨λ s => match s with
| ⟨name, Repo.none⟩       => name
| ⟨name, Repo.github url⟩ => name ++ " github \"" ++ url ++ "\""
| ⟨name, Repo.git url⟩    => name ++ " git \"" ++ url ++ "\""⟩

inductive BuildType
| executable | library

inductive Source
| lean : String → Source
| cpp  : String → Source

def Source.cpp? : Source → Bool
| Source.lean _ => false
| Source.cpp _  => true

def file (name ext : String) : FilePath :=
FilePath.mk (name ++ ext)

def Source.path : Source → FilePath
| Source.lean path => file path ".lean"
| Source.cpp path  => file path ".cpp"

def Source.subst (ext : String) : Source → FilePath
| Source.lean path => file path ext
| Source.cpp  path => file path ext

def Source.asOlean := Source.subst ".olean"
def Source.asCpp   := Source.subst ".cpp"
def Source.obj     := Source.subst ".o"

def Source.garbage : Source → List FilePath
| Source.lean path =>
  [ file path ".olean", file path ".cpp", file path ".o" ]
| Source.cpp path  =>
  [ file path ".o" ]

structure Project :=
(build    : BuildType)
(name     : String)
(files    : List Source)
(deps     : List Dep)
(depsDir  : FilePath)
(srcDir   : FilePath)
(cppLibs  : List String)
(cppFlags : List String)

def Project.getBinary (conf : Project) : FilePath :=
match conf.build with
| BuildType.executable => FilePath.mk conf.name
| BuildType.library => file ("lib" ++ conf.name) ".a"

inductive Val
| string : String → Val
| list   : List Val → Val
| pair   : Val × Val → Val

def Val.getString : Val → Except String String
| Val.string v => Except.ok v
| _            => Except.error "expected string"

structure Tools :=
(leanHome lean ar cpp : String)
