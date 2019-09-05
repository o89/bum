import init.system.io init.system.filepath bum.io

def getTools (conf : Project) : IO Tools := do
  leanHomeOpt ← IO.getEnv "LEAN_HOME";
  match leanHomeOpt with
  | some leanHome ⇒ do
    IO.setEnv "LEAN_PATH" [ leanHome, "library" ].joinPath;
    IO.realPath [ ".", "src" ].joinPath >>= addToLeanPath;
    IO.runCmd ("mkdir -p " ++ conf.depsDir);
    pure ⟨leanHome,
          [ leanHome, "bin", "lean" ].joinPath,
          [ leanHome, "bin", "leanc" ].joinPath,
          "ar", "c++"⟩
  | none ⇒ throw "LEAN_HOME not found"

def eval : Command → IO Unit
| Command.start ⇒ do
  conf ← readConf config;
  match conf.build with
  | BuildType.executable ⇒ do
    let name := conf.getBinary;
    IO.println ("Starting: " ++ name);
    IO.runCmd [".", name].joinPath;
    pure ()
  | BuildType.library ⇒
    IO.println "Cannot start a library"
| Command.clean scale ⇒ do
  conf ← readConf config;
  (match scale with
  | Scale.this ⇒ clean
  | Scale.all  ⇒ cleanRec) conf
| Command.compile ⇒ do
  conf ← readConf config;
  tools ← getTools conf;
  build tools conf
| Command.olean scale ⇒ do
  conf ← readConf config;
  tools ← getTools conf;
  match scale with
  | Scale.this ⇒ do
    deps ← resolveDeps conf;
    getLeanPathFromDeps conf.depsDir deps >>= addToLeanPath;
    olean tools conf
  | Scale.all ⇒ recOlean tools conf
| Command.deps ⇒ do
  conf ← readConf config;
  deps ← resolveDeps conf true;
  let toPrint := (String.append "==> dependency: " ∘ Project.name) <$> deps;
  if toPrint ≠ [] then
    IO.println (String.intercalate "\n" toPrint)
  else pure ()
| Command.help ⇒ IO.println Command.helpString
| Command.app app ⇒ do IO.runCmd (Repo.cmd "." app.toRepo); pure ()

def evalList : List Command → IO Unit
| [] ⇒ eval Command.help
| xs ⇒ forM' eval xs >> IO.println "OK"

def main (args : List String) :=
match Command.ofList args with
| Except.ok v      ⇒ evalList v
| Except.error err ⇒ IO.println err
