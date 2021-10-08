import Init.System.FilePath
import bum.configconverter
import Std.Data

open System (FilePath)
open IO.Process

def workdir := FilePath.mk "."

def Lean.deps := [
  "-ldl", "-lgmp", "-lm", "-lstdc++",
  "-Wl,--end-group", "-lleanrt", "-lStd", "-lInit", "-Wl,--start-group",
  "-Wl,--end-group", "-lLean", "-lleancpp", "-Wl,--start-group"
]

def Lean.cppOptions := ["-D LEAN_MULTI_THREAD", "-D LEAN_AUTO_THREAD_FINALIZATION", "-fPIC", "-fvisibility=hidden", "-pthread"]

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

def uptodate (filename : FilePath) : IO Unit :=
println! "“{filename}” is already up to date, skip."

def Source.skip : Source → IO Unit :=
uptodate ∘ Source.path

def IO.enoent (path : FilePath) : IO Bool :=
not <$> System.FilePath.pathExists path

def IO.getLastWriteTime! (path : FilePath) : IO UInt64 := do
  if (← System.FilePath.pathExists path) then
    IO.getLastWriteTime (toString path)
  else pure 0

def Source.newer? (v : Source) (path : FilePath) : IO Bool := do
  let mtime₁ ← IO.getLastWriteTime! v.path
  let mtime₂ ← IO.getLastWriteTime! path
  pure (mtime₁ > mtime₂)

def sourceOlean (tools : Tools) : Source → List Action
| src@(Source.lean path) =>
  [ { cmd := tools.lean, args := #["-o", toString src.asOlean, toString src.path],
      skip := src.skip, old? := src.newer? src.asOlean } ]
| _ => []

def getInclude (tools : Tools) : Array String :=
#["-I" ++ [ tools.leanHome, "include" ].joinPath]

def sourceCommands (tools : Tools) (dir : FilePath) : Source → List Action
| src@(Source.lean path) =>
  [ -- generate olean
    { cmd  := tools.lean, args := #["-o", toString src.asOlean, toString src.path],
      skip := src.skip, old? := src.newer? src.asOlean },
    -- compile into .cpp
    { cmd  := tools.lean, old? := src.newer? src.asCpp,
      args := #["-c", ["..", toString src.asCpp].joinPath,
                ["..", toString src.path].joinPath]
      cwd  := dir },
    -- emit .o
    { cmd  := tools.cpp, old? := src.newer? src.obj,
      args := getInclude tools ++ #["-c", toString src.asCpp, "-o", toString src.obj] } ]
| src@(Source.cpp path) =>
  [{ cmd := tools.cpp, old? := src.newer? src.obj, skip := src.skip,
     args := getInclude tools ++ #["-c", toString src.path, "-o", toString src.obj] } ]

def sourceLink (output : FilePath) (tools : Tools)
  (files : List Source) (flags : List String) : List Action :=
[ { cmd := tools.ar, old? := IO.enoent output, skip := uptodate output,
    args := #["rvs", toString output] ++
            Array.map (toString ∘ Source.obj) files.toArray ++
            flags.toArray } ]

def sourceCompile (output : FilePath) (tools : Tools)
  (files : List Source) (libs flags : List String) : List Action :=
[ { cmd := tools.cpp, old? := IO.enoent output, skip := uptodate output,
    args := Lean.cppOptions.toArray.map toString ++ #["-o", toString output] ++
            (List.map (toString ∘ Source.obj) files).reverse.toArray ++
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
  |> procents |> λ xs => List.forM xs (performAction force)

def silentRemove (filename : FilePath) : IO Unit :=
IO.remove (toString filename) >>= λ _ => pure ()

structure Pkg :=
(name path : String)

def String.addToPath (dest delta : String) :=
match dest with
| "" => delta
| _  => dest ++ ":" ++ delta

def addToLeanPath (path : FilePath) : IO Unit := do
  let u := toString path
  let path ← IO.getEnv "LEAN_PATH"
  match path with
  | none   => IO.setEnv "LEAN_PATH" u
  | some v => IO.setEnv "LEAN_PATH" (v.addToPath u);
  pure ()

abbrev Link := System.FilePath × Project
abbrev Deps := List Link

partial def resolveDepsAux (depsDir : FilePath) (download : Bool) :
  String → Dep → IO Deps
| parent, dep => do
  let confPath := depsDir / dep.name / config;

  let isThere ← System.FilePath.pathExists confPath;
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
  pure (List.join projects ++ [(FilePath.mk dep.name, conf)])

def resolveDeps (conf : Project) (download : Bool := false) : IO Deps :=
List.uniq (Project.name ∘ Prod.snd) <$> List.join <$>
  List.mapM (resolveDepsAux conf.depsDir download conf.name) conf.deps

def getLeanPathFromDeps (depsDir : FilePath) (xs : Deps) : IO (List FilePath) :=
let getSourcesDir : Link → FilePath :=
λ (path, conf) => workdir / depsDir / path / conf.srcDir;
List.mapM (IO.FS.realPath ∘ getSourcesDir) xs

def getDepBinaryPath (depsDir : FilePath) (conf : FilePath × Project) : FilePath :=
workdir / depsDir / conf.fst / conf.snd.getBinary

def getCppLibraries (conf : FilePath × Project) : List String :=
List.map (String.append "-l") conf.snd.cppLibs

def evalDep {α : Type} (depsDir : FilePath) (rel : FilePath)
  (action : IO α) : IO α := do
  let cwd ← IO.FS.realPath "."
  let path := depsDir / rel

  IO.chdir path
  let val ← action
  IO.chdir cwd
  pure val

def buildAux (tools : Tools) (depsDir : FilePath) (force : IO.Ref Bool)
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
  List.forM leanPath addToLeanPath;
  pure deps

def build (tools : Tools) (conf : Project) (force? := false) : IO Unit := do
  let deps ← setLeanPath conf;

  let force ← IO.mkRef force?;
  let done  ← IO.mkRef Std.mkHashSet;

  buildAux tools conf.depsDir force done deps;
  let libs :=
    Lean.deps ++
    List.join (List.map getCppLibraries deps) ++
    List.map (toString ∘ getDepBinaryPath conf.depsDir) deps;

  compileProject force conf tools libs

def olean (force : IO.Ref Bool) (tools : Tools) (conf : Project) : IO Unit := do
  IO.println ("Generate .olean for " ++ conf.name);
  oleanCommands conf tools
  |> procents |> λ xs => List.forM xs (performAction force)

def oleanRecur (tools : Tools) (conf : Project) (force? := false) : IO Unit := do
  let deps ← setLeanPath conf
  let force ← IO.mkRef force?

  List.forM deps λ (path, project) =>
    olean force tools project
    |> evalDep conf.depsDir path;
  olean force tools conf

def clean (conf : Project) : IO Unit := do
  let conf ← readConf config;
  let buildFiles :=
  conf.getBinary :: List.join (List.map Source.garbage conf.files);
  List.forM buildFiles silentRemove

def cleanRecur (conf : Project) : IO Unit := do
  let deps ← resolveDeps conf;
  List.forM deps λ cur =>
    evalDep conf.depsDir cur.fst (clean cur.snd);
  clean conf
