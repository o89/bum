import Init.System.IO
import Init.System.FilePath
import bum.io

def getTools (conf : Project) : IO Tools := do
  leanHomeOpt ← IO.getEnv "LEAN_HOME";

  match leanHomeOpt with
  | some leanHome => do
    _ ← IO.setEnv "LEAN_PATH" "";
    addToLeanPath [ leanHome, "lib", "lean" ].joinPath;
    src ← IO.realPath [ ".", "src" ].joinPath;
    addToLeanPath src;
    _ ← IO.runCmd ("mkdir -p " ++ conf.depsDir);
    pure ⟨leanHome, [ leanHome, "bin", "lean" ].joinPath, "ar", "c++"⟩
  | _ => throw (IO.Error.userError "Environment variable LEAN_HOME not found")

def eval : Command → IO Unit
| Command.start => do
  conf ← readConf config;
  match conf.build with
  | BuildType.executable => do
    let name := conf.getBinary;
    IO.println ("Starting: " ++ name);
    _ ← IO.runCmd [".", name].joinPath;
    pure ()
  | BuildType.library =>
    IO.println "Cannot start a library"
| Command.clean scale => do
  conf ← readConf config;
  (match scale with
  | Scale.this => clean
  | Scale.all  => cleanRec) conf
| Command.compile => do
  conf ← readConf config;
  tools ← getTools conf;
  build tools conf
| Command.olean scale => do
  conf ← readConf config;
  tools ← getTools conf;
  match scale with
  | Scale.this => do
    _ ← setLeanPath conf;
    olean tools conf
  | Scale.all => recOlean tools conf
| Command.deps => do
  conf ← readConf config;
  deps ← resolveDeps conf true;
  let toPrint :=
  List.map (String.append "==> dependency: " ∘
            Project.name ∘ Prod.snd) deps;
  if toPrint ≠ [] then
    IO.println (String.intercalate "\n" toPrint)
  else pure ()
| Command.help => IO.println Command.helpString
| Command.app app => IO.runCmd (Repo.cmd "." app.toRepo) >>= λ _ => pure ()
| Command.nope => pure ()

def evalList : List Command → IO Unit
| [] => eval Command.help
| xs => List.forM eval xs >> IO.println "OK"

def main (args : List String) :=
match Command.parse args with
| Except.ok v      => evalList v
| Except.error err => IO.println err
