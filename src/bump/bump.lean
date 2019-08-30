import init.system.io init.system.filepath bump.io

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
| Command.clean ⇒ do
  conf ← readConf config;
  let buildFiles :=
  conf.getBinary :: List.join (Source.garbage <$> conf.files);
  forM' silentRemove buildFiles
| Command.compile ⇒ do
  conf ← readConf config;
  leanHomeOpt ← IO.getEnv "LEAN_HOME";
  match leanHomeOpt with
  | some leanHome ⇒ do
    IO.setEnv "LEAN_PATH" [ leanHome, "library" ].joinPath;
    IO.realPath [ ".", "src" ].joinPath >>= addToLeanPath;
    let tools : Tools :=
    ⟨leanHome,
     [ leanHome, "bin", "lean" ].joinPath,
     [ leanHome, "bin", "leanc" ].joinPath,
     "ar", "c++"⟩;
    IO.runCmd ("mkdir -p " ++ conf.depsDir);
    build tools conf
  | none ⇒ throw "LEAN_HOME not found"
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
