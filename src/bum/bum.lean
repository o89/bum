import bum.io

def getTools (conf : Project) : IO Tools := do
  let leanHomeOpt ← IO.getEnv "LEAN_HOME";

  match leanHomeOpt with
  | some leanHome => do
    let _ ← IO.setEnv "LEAN_PATH" "";
    addToLeanPath [ leanHome, "lib", "lean" ].joinPath;
    let src ← IO.realPath [ ".", "src" ].joinPath;
    addToLeanPath src;
    let _ ← IO.Process.run { cmd := "mkdir", args := #["-p", conf.depsDir]};
    pure ⟨leanHome, [ leanHome, "bin", "lean" ].joinPath, "ar", "c++"⟩
  | _ => throw (IO.Error.userError "Environment variable LEAN_HOME not found")

def eval : Command → IO Unit
| Command.start => do
  let conf ← readConf config;
  match conf.build with
  | BuildType.executable => do
    let name := conf.getBinary;
    runCmdPretty { cmd := [".", name].joinPath };
    pure ()
  | BuildType.library =>
    IO.println "Cannot start a library"
| Command.clean scale => do
  let conf ← readConf config;
  (match scale with
  | Scale.this => clean
  | Scale.all  => cleanRec) conf
| Command.compile => do
  let conf ← readConf config;
  let tools ← getTools conf;
  build tools conf
| Command.olean scale => do
  let conf ← readConf config;
  let tools ← getTools conf;
  match scale with
  | Scale.this => do
    let _ ← setLeanPath conf;
    olean tools conf
  | Scale.all => recOlean tools conf
| Command.deps => do
  let conf ← readConf config;
  let deps ← resolveDeps conf true;
  let toPrint :=
  List.map (String.append "==> dependency: " ∘
            Project.name ∘ Prod.snd) deps;
  if toPrint ≠ [] then
    IO.println (String.intercalate "\n" toPrint)
  else pure ()
| Command.help => IO.println Command.helpString
| Command.app app => do
  runCmdPretty (Repo.cmd "." app.toRepo)
  let _ ← IO.Process.run { cmd := "rm", args := #["-rf", ".git"] }
  pure ()
| Command.nope => pure ()

def evalList : List Command → IO Unit
| [] => eval Command.help
| xs => List.forM eval xs >> IO.println "OK"

def main (args : List String) :=
match Command.parse args with
| Except.ok v      => evalList v
| Except.error err => IO.println err
