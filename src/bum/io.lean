import Init.System.IO
import Init.System.FilePath
import bum.parser

def Lean.deps := [ "-lpthread", "-lgmp", "-ldl" ]
-- bin/leanc:
-- NOTE: leanstatic and leanstdlib are cyclically dependent
def Lean.libraries (leanHome : String) := 
List.joinPath <$>
  [ [ leanHome, "bin", "libleanstatic.a" ],
    [ leanHome, "bin", "libleanstdlib.a" ],
    [ leanHome, "bin", "libleanstatic.a" ],
    [ leanHome, "bin", "libleanstdlib.a" ] ]
def Lean.cppOptions := [ "-fPIC", "-Wno-unused-command-line-argument" ]

def config := "bum.config"

def runCmdPretty (additionalInfo s : String) : IO Unit := do
  IO.println ("==> " ++ s ++ " " ++ additionalInfo);
  exitv ← IO.runCmd s;
  let errorStr := "process exited with code " ++ toString exitv;
  IO.cond (exitv ≠ 0) $ throw (IO.Error.userError errorStr)

def sourceOlean (tools : Tools) : Source → Option (List String)
| src@(Source.lean path) =>
  some [ [ tools.lean, "--make", src.path ].space ]
| _ => none

def sourceCommands (tools : Tools) : Source → List String
| src@(Source.lean path) =>
  List.space <$>
    [ [ tools.lean, "--make", src.path ],
      [ tools.lean, "-c", src.asCpp, src.path ],
      [ tools.leanc, "-c", src.asCpp, "-o", src.obj ] ]
| src@(Source.cpp path) =>
  List.space <$>
    [ [ tools.leanc, "-c", src.path, "-o", src.obj ] ]

def sourceLink
  (output : String) (tools : Tools)
  (files : List Source) (flags : List String) :=
List.space $ [ tools.ar, "rvs", output ] ++ Source.obj <$> files ++ flags

def sourceCompile (output : String) (tools : Tools)
  (files : List Source) (libs flags : List String) :=
List.space $
  pure tools.cpp ++ Lean.cppOptions ++
  [ "-o",  output ] ++
  (Source.obj <$> files).reverse ++
  libs.reverse ++ flags

def compileCommands
  (conf : Project) (tools : Tools)
  (libs flags : List String) :=
match conf.build with
| BuildType.executable =>
  List.join (sourceCommands tools <$> conf.files) ++
  [ sourceCompile conf.getBinary tools conf.files libs flags ]
| BuildType.library =>
  List.join (sourceCommands tools <$> conf.files) ++
  [ sourceLink conf.getBinary tools conf.files flags ]

def oleanCommands (conf : Project) (tools : Tools) :=
List.join (List.filterMap (sourceOlean tools) conf.files)

def procents {α : Type} (xs : List α) : List (Nat × α) :=
(λ (p : Nat × α) => (p.1 * 100 / xs.length, p.2)) <$> xs.enum

def compileProject (conf : Project) (tools : Tools) (libs : List String) : IO Unit :=
let runPretty :=
λ (p : Nat × String) => runCmdPretty ("(" ++ toString p.1 ++ " %)") p.2;
let actions := runPretty <$> procents (compileCommands conf tools libs conf.cppFlags);
IO.println ("Compiling " ++ conf.name) >> forM' id actions

def silentRemove (filename : String) : IO Unit :=
IO.remove filename >>= λ _ => pure ()

structure Pkg :=
(name path : String)

def String.addToPath (dest delta : String) :=
match dest with
| "" => delta
| _  => dest ++ ":" ++ delta

def addToLeanPath (pkg : Pkg) : IO Unit := do
  let pkgStr := pkg.name ++ "=" ++ pkg.path;
  path ← IO.getEnv "LEAN_PATH";
  _ ← match path with
  | none   => IO.setEnv "LEAN_PATH" pkgStr
  | some v => IO.setEnv "LEAN_PATH" (v.addToPath pkgStr);
  pure ()

abbrev Path := String
abbrev Deps := List (Path × Project)

partial def resolveDepsAux (depsDir : String) (download : Bool) :
  String → Dep → IO Deps
| parent, dep => do
  let confPath := [ depsDir, dep.name, config ].joinPath;

  isThere ← IO.fileExists confPath;
  IO.cond (¬isThere ∧ download) (do
    IO.println ("==> downloading " ++ dep.name ++ " (of " ++ parent ++ ")");
    exitv ← IO.runCmd (Dep.cmd depsDir dep);
    let err :=
      "downloading of “" ++ dep.name ++
      "” failed with code " ++ toString exitv;
    IO.cond (exitv ≠ 0) $ throw (IO.Error.userError err));

  conf ← readConf confPath;
  projects ← sequence (resolveDepsAux dep.name <$> conf.deps);
  pure (List.join projects ++ [ (dep.name, conf) ])

def resolveDeps (conf : Project) (download : Bool := false) : IO Deps :=
List.uniq (Project.name ∘ Prod.snd) <$> List.join <$>
  List.mapM (resolveDepsAux conf.depsDir download conf.name) conf.deps

def getLeanPathFromDeps (depsDir : String) (xs : Deps) : IO (List Pkg) :=
let getSourcesDir : Path → String :=
λ path => [ ".", depsDir, path, "src" ].joinPath;
let getPkg : Path × Project → IO Pkg :=
λ ⟨path, conf⟩ => (Pkg.mk conf.name) <$> IO.realPath (getSourcesDir path);
List.mapM getPkg xs

def getDepBinaryPath (depsDir : String) (conf : Path × Project) : String :=
[ ".", depsDir, conf.fst, conf.snd.getBinary ].joinPath

def getCppLibraries (conf : Path × Project) : List String :=
String.append "-l" <$> conf.snd.cppLibs

def evalDep {α : Type} (depsDir : String) (rel : Path)
  (action : IO α) : IO α := do
  cwd ← IO.realPath ".";
  let path := [ depsDir, rel ].joinPath;
  exitv ← IO.chdir path;
  let errString := "cannot go to " ++ path;
  IO.cond (exitv ≠ 0) $ throw (IO.Error.userError errString);
  val ← action; _ ← IO.chdir cwd;
  pure val

def buildAux (tools : Tools) (depsDir : String)
  (doneRef : IO.Ref (List String)) :
  Bool → Deps → IO Unit
| _, [] => pure ()
| needsRebuild', hd :: tl => do
  done ← doneRef.get;
  if done.notElem hd.snd.name then do
    needsRebuild ← evalDep depsDir hd.fst (do
      needsRebuild ← or needsRebuild' <$> not <$> IO.fileExists hd.snd.getBinary;
      IO.cond needsRebuild (compileProject hd.snd tools []);
      pure needsRebuild);

    doneRef.set (hd.snd.name :: done);
    buildAux needsRebuild tl
  else buildAux needsRebuild' tl

def setLeanPath (conf : Project) : IO Deps := do
  deps ← resolveDeps conf;
  leanPath ← getLeanPathFromDeps conf.depsDir deps;
  List.forM addToLeanPath leanPath;
  pure deps

def build (tools : Tools) (conf : Project) : IO Unit := do
  deps ← setLeanPath conf;
  ref ← IO.mkRef [];
  buildAux tools conf.depsDir ref false deps;
  let libs :=
    Lean.deps ++
    List.join (getCppLibraries <$> deps) ++
    Lean.libraries tools.leanHome ++
    getDepBinaryPath conf.depsDir <$> deps;
  compileProject conf tools libs

def olean (tools : Tools) (conf : Project) : IO Unit := do
  IO.println ("Generate .olean for " ++ conf.name);
  List.forM (runCmdPretty "") (oleanCommands conf tools)

def recOlean (tools : Tools) (conf : Project) : IO Unit := do
  deps ← setLeanPath conf;
  List.forM
    (λ (cur : Path × Project) =>
      evalDep conf.depsDir cur.fst (olean tools cur.snd))
    deps;
  olean tools conf

def clean (conf : Project) : IO Unit := do
  conf ← readConf config;
  let buildFiles :=
  conf.getBinary :: List.join (Source.garbage <$> conf.files);
  forM' silentRemove buildFiles  

def cleanRec (conf : Project) : IO Unit := do
  deps ← resolveDeps conf;
  List.forM
    (λ (cur : Path × Project) =>
      evalDep conf.depsDir cur.fst (clean cur.snd))
    deps;
  clean conf
