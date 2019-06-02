# YSC2229 Examples

Various examples of using YSC2229 libraries for writing your own
programs. The libraries are delivered as a standalone package
available for building and can be compiled from the privater
repository

https://github.com/ilyasergey/ysc2229-part-one

The repository also contains all instructions.

Please, consult the lecture notes for the theoretical material.:

https://ilyasergey.net/YSC2229

## Project Structure 

This project consist of the following folders:

* `lib` contains the libraries that build upon the modules from
  `ysc2229-part-one`, and serve as libraries for the runnable of this
  project. It also contains the `dune` descriptor for building the
  libraries (explained below). It is assumed that most of the
  "interesting" and reusable components of the project will be defined
  as libraries.

* `runners` contains multiple examples of executables that rely both
  on local libraries (from `lib`), as well as the descriptor for
  compiling the runnable binaries. The corresponding files contain the
  comments with explanations of their ket components and intended
  functionality.

## Building the project

### Dependencies

As this project depends on `ysc2229-part-one`, make sure the latter
one is installed via `opam`, as described in the `README.md` of the
[corresponding project](https://github.com/ilyasergey/ysc2229-part-one).

The dependencies also contain a third-party packege `core`, which can
be installed as follows:

```
opam install core
```


The project can be then built by typing

```
make
```

or

```
dune build @install
```

If you have used the `make` option, you should be able to run the
build binary, linked from `./bin` folder of the project:

```
./bin/compare_sorts
```

To remove compilation artefacts, run

```
make clean
```

## Running REPL for the project

One can indeed want to experiment with the project as it evolves using
the familiar Read-Eval-Print-Loop (REPL). This can be accomplished
using a very useful tool for OCaml called `utop` (check the
[Documentation](https://github.com/ocaml-community/utop)).

`utop` provides as superior REPL that can be run for OCaml-based
projects (built via `dune`).

### Installing utop

The easiest and recommended way of installing utop is via opam:

```
$ opam install utop
```

To use utop in Emacs/Aquamacs, follow the instructions [here](https://github.com/ocaml-community/utop#main-setup).

### Running utop from the project root

In the root of the project, type

```
 dune utop .
```

You can now type your code here, loading file dynamically:

```
 #use "runners/compare_sorts.ml";;
```

Notice, that you can use auto-completion via TAB.

You can also invoke code from `opam`-installed and local libraries
(e.g., `SortUtil`):

```
─( 11:05:08 )─< command 0 >──────────────────────────────────────{ counter: 0 }─
utop # SortUtil.useless_message;;
- : string = "Hello"
```

To exit the REPL, type

```
#quit;;
```

### Running utop from Emacs

Assume you're hacking one of the libraries or the runners in the
project, say, `compare_sorts`. You may want to test your changes by
starting a `utop` buffer right in Emacs. Just press `Ctrl-C Ctrl-S`
(or `Alt-X` and then type `utop` and press Enter).

When prompted for the first time which command to run, enter:

```
dune utop ../ -- -emacs
```

or

```
opam config exec -- dune utop ../ -- -emacs
```

This will be the default option for the next times and will start the
`utop` REPL with all the project dependencies declared as `library`
(see the notes on configurations below). Notice that the path
argument `../` above is such that it points to the root of the current
project. This should always be the case in order to handle the
dependencies properly.

You can then evaluate the buffers from `runners` using usual shortcuts
(`Ctrl-B`), using the libraries as in the `utop` examples above.

## Playing with examples:

* `runners/compare_sorts.ml` - a simple file that generates two arrays
  and compares two sorting algorithms on them. To evaluate, after
  building the project run
  ```
  bin/compare_sorts
  ```

* `runners/read_input.ml` - an example that reads user input from a
  command-line argument. For instance, if you run
  ```
  bin/read_input data.txt
  ```
  it will create the file `data.txt` in the root of the project, with
  a message `Hello!` in it. 

For more interesting things to engineer in OCaml, check Google, or the
book [Real World OCaml](https://realworldocaml.org/).

## Essential configuration files

In order to have the smooth experience when working with the project,
a number of infrastructure files are provided, telling the building
tools what is located where and how parts of the project should be
treated. Sometime they need to be modifier in order to add, e.g., new
executables, but most of the infrastructure is already fixed and will
not need any enhancements as the project grows. Here we explain some
key elements.

### Building via dune

In the project, each folder with OCaml (`*.ml`) files contains a
configuration file named `dune`. Depending on the purpose of the files
in the folders, different descriptions are given.

For instance, all files in `lib` are treated as libraries, so the
descriptor defines them as such:

```
(library
 (name ysc2229_example_lib)
 (public_name ysc2229-example-lib)
  (wrapped false)
  (libraries ysc2229-part-one)
  (synopsis "Exercise libraries of YSC2229"))
``

The public-name is given so the libraries could be compiled and loaded
as an independent package via `opam`. The `libraries` part specifies
dependencies on the external libraries (e.g., `ysc2229-part-one`),
which should be independently installed via `opam`.

In `runners`, the descriptor contains explanations what should be
compiled as an independent executable binary::

```
(executables
 (names compare_sorts)
  (public_names compare_sorts)
  (modules compare_sorts RunnerUtil)
  (libraries ysc2229-part-one ysc2229-example-lib))
```

It also specifies dependencies from the external and internal
libraries `(libraries ysc2229-part-one ysc2229-example-lib)`, as well
as on the internal module `RunnerUtil` defined in the same folder.

### Makefile

The toplevel `Makefile` contains various shell tasks, simplifying
building of the project. For instance, the following tasks invokes the
`dune` build script, and then links the compiled executables under the
folder `bin` as follows:

```
all:
	dune build --profile release @install 
	@test -L bin || ln -s _build/install/default/bin .
```

### opam descriptor

The top-level file `ysc2229-example-lib.opam` allows to ship the
library part of the project (under `lib`) as an independent package to
be used by third parties (similarly to how it's done for
`ysc2229-part-one`). To do so, after building the project, type in
terminal from the project root.

```
opam install .
```

It will install the project's modules from `lib` as a new package
`ysc2229-example-lib`. You can uninstall it as follows:

```
opam uninstall ysc2229-example-lib
```







