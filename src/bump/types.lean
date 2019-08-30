inductive Application
| zero | n2o | nitro

def Application.ofString : String → Except String Application
| "zero"  ⇒ Except.ok Application.zero
| "n2o"   ⇒ Except.ok Application.n2o
| "nitro" ⇒ Except.ok Application.nitro
| s       ⇒ Except.error ("unknown template “" ++ s ++"”")

inductive Command
| app : Application → Command | deps | compile
| start | clean
| help

partial def Command.ofList : List String → Except String (List Command)
| hd :: tl ⇒
  match hd with
  | "compile" ⇒ List.cons Command.compile <$> Command.ofList tl
  | "start"   ⇒ List.cons Command.start   <$> Command.ofList tl
  | "clean"   ⇒ List.cons Command.clean   <$> Command.ofList tl
  | "deps"    ⇒ List.cons Command.deps    <$> Command.ofList tl
  | "app"     ⇒
    match tl with
    | template :: tl' ⇒
      List.cons <$> (Command.app <$> Application.ofString template) <*>
        Command.ofList tl'
    | [] ⇒ Except.error "“app” expects template name"
  | _ ⇒ Except.error ("unknown command “" ++ hd ++"”")
| [] ⇒ Except.ok []

def Command.helpString :=
"BUMP Lean 4 build tool

    invoke = bump | bump list
      list = []   | command [options] list
   command = app [zero|n2o|nitro] | deps | compile
           | start | clean"

inductive Repo
| none
| git : String → Repo

-- TODO: remove hard-coded GitHub
def Application.toRepo : Application → Repo
| Application.zero  ⇒ Repo.git "https://github.com/o89/sample-zero"
| Application.n2o   ⇒ Repo.git "https://github.com/o89/sample-n2o"
| Application.nitro ⇒ Repo.git "https://github.com/o89/sample-nitro"

def Repo.cmd (target : String) : Repo → String
| Repo.none    ⇒ ""
| Repo.git url ⇒ "git clone " ++ url ++ " " ++ target

structure Dep :=
(name : String) (source : Repo)

def Dep.cmd (depsDir : String) (x : Dep) : String :=
Repo.cmd (depsDir ++ "/" ++ x.name) x.source

instance : HasToString Dep :=
⟨λ s ⇒ match s with
| ⟨name, Repo.none⟩    ⇒ name
| ⟨name, Repo.git url⟩ ⇒ name ++ " \"" ++ url ++ "\""⟩

inductive BuildType
| executable | library

inductive Source
| lean : String → Source
| cpp  : String → Source

def Source.path : Source → String
| Source.lean path ⇒ path ++ ".lean"
| Source.cpp path  ⇒ path ++ ".cpp"

def Source.asCpp : Source → String
| Source.lean path ⇒ path ++ ".cpp"
| Source.cpp path  ⇒ path ++ ".cpp"

def Source.obj : Source → String
| Source.lean path ⇒ path ++ ".o"
| Source.cpp path  ⇒ path ++ ".o"

def Source.garbage : Source → List String
| Source.lean path ⇒
  [ path ++ ".olean", path ++ ".cpp", path ++ ".o" ]
| Source.cpp path  ⇒
  [ path ++ ".o" ]

structure Project :=
(build   : BuildType)
(name    : String)
(files   : List Source)
(deps    : List Dep)
(depsDir : String)
(cppLibs : List String)

def Project.getBinary (conf : Project) : String :=
match conf.build with
| BuildType.executable ⇒ conf.name
| BuildType.library ⇒ "lib" ++ conf.name ++ ".a"

inductive Val
| string : String → Val
| list   : List Val → Val
| pair   : Val × Val → Val

def Val.getString : Val → Except String String
| Val.string v ⇒ Except.ok v
| _            ⇒ Except.error "expected string"

structure Tools :=
(leanHome lean leanc ar cpp : String)
