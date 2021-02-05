import Init.System.FilePath
import bum.configconverter
import Std.Data

open IO.Process

def Lean.deps := [ "-ldl", "-lgmp", "-Wl,--end-group", "-lLean",
                   "-lStd", "-lInit", "-lleancpp", "-Wl,--start-group" ]

def Lean.cppOptions := [ "-no-pie", "-pthread", "-Wno-unused-command-line-argument" ]

def config := "bum.config"

structure Action extends SpawnArgs :=
(old? : IO Bool := pure true)
(skip : IO Unit := pure ())

def exec (proc : SpawnArgs) (s : Option String := none) : IO Unit := do
  let info := s.getD ""
  println! "==> {proc.cmd} {proc.args.toList.space} {info}"

  let child ← spawn proc
  let exitCode ← child.wait

  if exitCode ≠ 0 then
    s! "process “{proc.cmd}” exited with code {exitCode}"
    |> IO.userError |> throw

  pure ()

def uptodate (filename : String) : IO Unit :=
println! "“{filename}” is already up to date, skip."

def Source.skip : Source → IO Unit :=
uptodate ∘ Source.path

def IO.enoent (path : String) : IO Bool :=
not <$> IO.fileExists path

def IO.getLastWriteTime! (path : String) : IO UInt64 := do
  if (← IO.fileExists path) then
    IO.getLastWriteTime path
  else pure 0

def rebuild? (v : Source) : IO Bool := do
  let mtime₁ ← IO.getLastWriteTime! v.path
  let mtime₂ ← IO.getLastWriteTime!
    (match v with
    | Source.lean val => v.asOlean
    | Source.cpp val  => v.obj)
  pure (mtime₁ > mtime₂)

def sourceOlean (tools : Tools) : Source → List Action
| src@(Source.lean path) =>
  [ { cmd := tools.lean, args := #["-o", src.asOlean, src.path],
      skip := src.skip, old? := rebuild? src } ]
| _ => []

def getInclude (tools : Tools) : Array String :=
#["-I" ++ [ tools.leanHome, "include" ].joinPath]

def sourceCommands (tools : Tools) (dir : String) : Source → List Action
| src@(Source.lean path) =>
  [ -- generate olean
    { cmd  := tools.lean, args := #["-o", src.asOlean, src.path],
      skip := src.skip, old? := rebuild? src },
    -- compile into .cpp
    { cmd  := tools.lean, old? := rebuild? src,
      args := #["-c", ["..", src.asCpp].joinPath, ["..", src.path].joinPath]
      cwd  := dir },
    -- emit .o
    { cmd  := tools.cpp, old? := rebuild? src,
      args := getInclude tools ++ #["-c", src.asCpp, "-o", src.obj] } ]
| src@(Source.cpp path) =>
  [{ cmd := tools.cpp, old? := rebuild? src, skip := src.skip,
     args := getInclude tools ++ #["-c", src.path, "-o", src.obj] } ]

def sourceLink (output : String) (tools : Tools)
  (files : List Source) (flags : List String) : List Action :=
[ { cmd := tools.ar, old? := IO.enoent output, skip := uptodate output,
    args := #["rvs", output] ++ Array.map Source.obj files.toArray ++
              flags.toArray } ]

def sourceCompile (output : String) (tools : Tools)
  (files : List Source) (libs flags : List String) : List Action :=
[ { cmd := tools.cpp, old? := IO.enoent output, skip := uptodate output,
    args := Lean.cppOptions.toArray ++ #["-o", output] ++
            (List.map Source.obj files).reverse.toArray ++
            libs.reverse.toArray ++ flags.toArray ++
            #["-L" ++ tools.leanHome ++ "/lib/lean"] } ]

def compileCommands (conf : Project) (tools : Tools)
  (libs flags : List String) : List Action :=
List.join (List.map (sourceCommands tools conf.srcDir) conf.files) ++
match conf.build with
| BuildType.executable =>
  sourceCompile conf.getBinary tools conf.files libs flags
| BuildType.library =>
  sourceLink conf.getBinary tools conf.files flags

def oleanCommands (conf : Project) (tools : Tools) :=
List.join (List.map (sourceOlean tools) conf.files)

def procents {α : Type} (xs : List α) : List (Nat × α) :=
List.map (λ (p : Nat × α) => (p.1 * 100 / xs.length, p.2)) xs.enum

def performAction (force : IO.Ref Bool) : Nat × Action → IO Unit :=
λ (n, act) => do
  let info := some s!"({n} %)"
  let old? ← act.old?
  let force? ← force.get
  let run? := force? ∨ old?
  force.set run?

  if run? then exec act.toSpawnArgs info else act.skip

def compileProject (force : IO.Ref Bool) (conf : Project)
  (tools : Tools) (libs : List String) : IO Unit := do
  IO.println ("Compiling " ++ conf.name)
  compileCommands conf tools libs conf.cppFlags
  |> procents |> List.forM (performAction force)

def silentRemove (filename : String) : IO Unit :=
IO.remove filename >>= λ _ => pure ()

structure Pkg :=
(name path : String)

def String.addToPath (dest delta : String) :=
match dest with
| "" => delta
| _  => dest ++ ":" ++ delta

def addToLeanPath (u : String) : IO Unit := do
  let path ← IO.getEnv "LEAN_PATH";
  match path with
  | none   => IO.setEnv "LEAN_PATH" u
  | some v => IO.setEnv "LEAN_PATH" (v.addToPath u);
  pure ()

abbrev Path := String
abbrev Deps := List (Path × Project)

partial def resolveDepsAux (depsDir : String) (download : Bool) :
  String → Dep → IO Deps
| parent, dep => do
  let confPath := [ depsDir, dep.name, config ].joinPath;

  let isThere ← IO.fileExists confPath;
  if (¬isThere ∧ download) then {
    IO.println ("==> downloading " ++ dep.name ++ " (of " ++ parent ++ ")");
    exec (Dep.cmd depsDir dep)
  }

  let conf ← readConf confPath;
  match conf.build with
  | BuildType.executable =>
    s!"{conf.name} is not a library and cannot be project dependency"
    |> IO.Error.userError |> throw
  | _ => pure ()

  let projects ← sequence (List.map (resolveDepsAux depsDir download dep.name) conf.deps);
  pure (List.join projects ++ [ (dep.name, conf) ])

def resolveDeps (conf : Project) (download : Bool := false) : IO Deps :=
List.uniq (Project.name ∘ Prod.snd) <$> List.join <$>
  List.mapM (resolveDepsAux conf.depsDir download conf.name) conf.deps

def getLeanPathFromDeps (depsDir : String) (xs : Deps) : IO (List String) :=
let getSourcesDir : Path × Project → String :=
λ (path, conf) => [ ".", depsDir, path, conf.srcDir ].joinPath
List.mapM (IO.realPath ∘ getSourcesDir) xs

def getDepBinaryPath (depsDir : String) (conf : Path × Project) : String :=
[ ".", depsDir, conf.fst, conf.snd.getBinary ].joinPath

def getCppLibraries (conf : Path × Project) : List String :=
List.map (String.append "-l") conf.snd.cppLibs

def evalDep {α : Type} (depsDir : String) (rel : Path)
  (action : IO α) : IO α := do
  let cwd ← IO.realPath "."
  let path := [ depsDir, rel ].joinPath

  let exitv ← IO.chdir path
  unless (exitv = 0) do {
    throw (IO.Error.userError s!"cannot chdir to {path}")
  }

  let val ← action
  let _ ← IO.chdir cwd
  pure val

def buildAux (tools : Tools) (depsDir : String) (force : IO.Ref Bool)
  (doneRef : IO.Ref (Std.HashSet String)) : Deps → IO Unit
| [] => pure ()
| hd :: tl => do
  let done ← doneRef.get
  if ¬ done.contains hd.snd.name then {
    evalDep depsDir hd.fst (compileProject force hd.snd tools []);

    doneRef.set (done.insert hd.snd.name);
    buildAux tools depsDir force doneRef tl
  } else buildAux tools depsDir force doneRef tl

def setLeanPath (conf : Project) : IO Deps := do
  let deps ← resolveDeps conf;
  let leanPath ← getLeanPathFromDeps conf.depsDir deps;
  List.forM addToLeanPath leanPath;
  pure deps

def build (tools : Tools) (conf : Project) (force? := false) : IO Unit := do
  let deps ← setLeanPath conf;

  let force ← IO.mkRef force?;
  let done  ← IO.mkRef Std.mkHashSet;

  buildAux tools conf.depsDir force done deps;
  let libs :=
    Lean.deps ++
    List.join (List.map getCppLibraries deps) ++
    List.map (getDepBinaryPath conf.depsDir) deps;

  compileProject force conf tools libs

def olean (force : IO.Ref Bool) (tools : Tools) (conf : Project) : IO Unit := do
  IO.println ("Generate .olean for " ++ conf.name);
  oleanCommands conf tools
  |> procents |> List.forM (performAction force)

def oleanRecur (tools : Tools) (conf : Project) (force? := false) : IO Unit := do
  let deps ← setLeanPath conf
  let force ← IO.mkRef force?

  List.forM
    (λ (path, project) =>
      olean force tools project
      |> evalDep conf.depsDir path)
    deps
  olean force tools conf

def clean (conf : Project) : IO Unit := do
  let conf ← readConf config;
  let buildFiles :=
  conf.getBinary :: List.join (List.map Source.garbage conf.files);
  List.forM silentRemove buildFiles  

def cleanRecur (conf : Project) : IO Unit := do
  let deps ← resolveDeps conf;
  List.forM
    (λ (cur : Path × Project) =>
      evalDep conf.depsDir cur.fst (clean cur.snd))
    deps;
  clean conf
