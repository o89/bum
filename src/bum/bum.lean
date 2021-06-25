import bum.io

def getTools (conf : Project) : IO Tools := do
  let leanHomeOpt ← IO.getEnv "LEAN_HOME"

  match leanHomeOpt with
  | some leanHome => do
    discard (IO.setEnv "LEAN_PATH" "")
    addToLeanPath [ leanHome, "lib", "lean" ].joinPath
    let src ← IO.FS.realPath conf.srcDir
    addToLeanPath src
    discard (IO.Process.run { cmd := "mkdir", args := #["-p", toString conf.depsDir]})
    pure ⟨leanHome, [ leanHome, "bin", "lean" ].joinPath, "ar", "c++"⟩
  | none => throw (IO.Error.userError "Environment variable LEAN_HOME not found")

def eval : Command → IO Unit
| Command.start => do
  let conf ← readConf config
  match conf.build with
  | BuildType.executable => do
    let name := conf.getBinary
    exec { cmd := toString (workdir / name) }
  | BuildType.library =>
    IO.println "Cannot start a library"
| Command.clean scale => do
  let conf ← readConf config
  (match scale with
  | Scale.current => clean
  | Scale.total   => cleanRecur) conf
| Command.compile force? => do
  let conf ← readConf config
  let tools ← getTools conf
  build tools conf force?
| Command.olean scale force? => do
  let conf ← readConf config
  let tools ← getTools conf
  match scale with
  | Scale.current => do
    discard (setLeanPath conf)
    let force ← IO.mkRef force?
    olean force tools conf
  | Scale.total => oleanRecur tools conf force?
| Command.deps => do
  let conf ← readConf config
  let deps ← resolveDeps conf true
  let toPrint :=
  List.map (String.append "==> dependency: " ∘
            Project.name ∘ Prod.snd) deps
  if toPrint ≠ [] then
    IO.println (String.intercalate "\n" toPrint)
  else pure ()
| Command.help => IO.println Command.helpString
| Command.app app => do
  exec (Repo.cmd "." app.toRepo)
  IO.Process.run { cmd := "rm", args := #["-rf", ".git"] }
  |> discard
| Command.gitignore =>
  "“gitignore” can only be called individually: bum gitignore"
  |> IO.userError |> throw
| Command.leanPath =>
  "“lean-path” can only be called individually: bum lean-path"
  |> IO.userError |> throw
| Command.nope => pure ()

def evalList : List Command → IO Unit
| [] => eval Command.help
| [Command.nope, Command.leanPath] => do
  let conf ← readConf config
  let src ← IO.FS.realPath conf.srcDir
  println! "export LEAN_PATH=$LEAN_PATH:{src}"
| [Command.nope, Command.gitignore] => do
  let conf ← readConf config
  List.filter Source.cpp? conf.files
  |> List.map (λ file => s!"!{file.asCpp}")
  |> (["*.olean", "*.o", "*.a", "*.cpp", toString conf.getBinary] ++ ·)
  |> String.intercalate "\n"
  |> IO.println
| xs => List.forM xs eval >> IO.println "OK"

def main (args : List String) : IO Unit :=
match Command.parse args with
| Except.ok v      => evalList v
| Except.error err => IO.println err
