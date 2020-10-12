# BUM

## Installation

1. Download [latest Lean 4 build](https://github.com/leanprover/lean4/commits/master) from Build Artifacts.

2. Set `LEAN_HOME` environment variable with the path to Lean 4 installation:

```
$ echo 'export LEAN_HOME=/path/to/lean4 >> ~/.bashrc'
```

3. Run Make:

```
$ make
```

4. And copy `bum` binary to your preferred place:

```
$ sudo cp bum /usr/local/bin
# cp bum ~/.local/bin
```
