inductive Application
| zero | n2o | nitro

def Application.ofString : String → Except String Application
| "zero"  ⇒ Except.ok Application.zero
| "n2o"   ⇒ Except.ok Application.n2o
| "nitro" ⇒ Except.ok Application.nitro
| s       ⇒ Except.error ("unknown template “" ++ s ++"”")

inductive Scale
| this | all

instance : Inhabited Scale := ⟨Scale.this⟩

def Scale.ofString : String → Except String Scale
| "this" ⇒ Except.ok Scale.this
| "all"  ⇒ Except.ok Scale.all
| s      ⇒ Except.error ("unknown scale “" ++ s ++"”")

inductive Command
| app : Application → Command | deps | compile
| start | clean : Scale → Command
| olean : Scale → Command
| help

partial def Command.ofList : List String → Except String (List Command)
| hd :: tl ⇒
  match hd with
  | "compile" ⇒ List.cons Command.compile <$> Command.ofList tl
  | "start"   ⇒ List.cons Command.start   <$> Command.ofList tl
  | "deps"    ⇒ List.cons Command.deps    <$> Command.ofList tl
  | "clean"   ⇒
    match tl with
    | hd :: tl' ⇒
      match Scale.ofString hd with
      | Except.ok scale ⇒
        List.cons (Command.clean scale) <$> Command.ofList tl'
      | _ ⇒ List.cons (Command.clean $ default Scale) <$> Command.ofList tl
    | [] ⇒ pure [ Command.clean (default Scale) ]
  | "olean"   ⇒
    match tl with
    | hd :: tl' ⇒
      match Scale.ofString hd with
      | Except.ok scale ⇒
        List.cons (Command.olean scale) <$> Command.ofList tl'
      | _ ⇒ List.cons (Command.olean $ default Scale) <$> Command.ofList tl
    | [] ⇒ pure [ Command.olean (default Scale) ]
  | "app"     ⇒
    match tl with
    | template :: tl' ⇒
      List.cons <$> (Command.app <$> Application.ofString template) <*>
        Command.ofList tl'
    | [] ⇒ Except.error "“app” expects template name"
  | _ ⇒ Except.error ("unknown command “" ++ hd ++"”")
| [] ⇒ Except.ok []

def Command.helpString :=
"BUM Lean 4 build tool

    invoke = bum  | bum list
      list = []   | command [options] list
   command = app [zero|n2o|nitro] | deps | compile
           | start | clean | clean [this|all]
           | olean | olean [this|all]"

inductive Repo
| none
| git : String → Repo
| github : String → Repo

-- TODO: remove hard-coded GitHub
def Application.toRepo : Application → Repo
| Application.zero  ⇒ Repo.github "o89/sample-zero"
| Application.n2o   ⇒ Repo.github "o89/sample-n2o"
| Application.nitro ⇒ Repo.github "o89/sample-nitro"

def Repo.cmd (target : String) : Repo → String
| Repo.none    ⇒ ""
| Repo.git url ⇒ "git clone " ++ url ++ " " ++ target
| Repo.github url ⇒ "git clone https://github.com/" ++ url ++ " " ++ target

structure Dep :=
(name : String) (source : Repo)

def Dep.cmd (depsDir : String) (x : Dep) : String :=
Repo.cmd (depsDir ++ "/" ++ x.name) x.source

instance : HasToString Dep :=
⟨λ s ⇒ match s with
| ⟨name, Repo.none⟩    ⇒ name
| ⟨name, Repo.github url⟩ ⇒ name ++ " github \"" ++ url ++ "\""
| ⟨name, Repo.git url⟩ ⇒ name ++ " git \"" ++ url ++ "\""⟩

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
