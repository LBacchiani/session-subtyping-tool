# Session-subtyping-tool

## Executable and Source Code Version Requirements:

You will need Graphviz (dot) installed on your machine with the dot command inserted in your PATH variable to generate graphs:

- [Here](https://www2.graphviz.org/Packages/stable/windows/10/cmake/Release/x64/graphviz-install-2.44.1-win64.exe) you can find Graphviz for Windows
- To install Graphviz for OSx you need a package manager. You can use [Homebrew](https://brew.sh) and install Graphiz with `brew install graphviz`
- The `dot` command have to be in your PATH.
- Your Graphviz installation should support different: `png, jpeg, pdf, svg`. You can check this executing `dot -v`. If you find out that your installation does not support the aforementioned formats, execute `dot -c`

## Source Code Usage:

### Requirements:
Besides the aforementioned requirements, to use the source code version you will need:
- [Python (3.X version)](https://www.python.org/downloads/)
- [Ghc](https://www.haskell.org/platform/)
- Depending on your setup, you might need to install extra Haskell packages (GHC's complaints will help you figure these out and you can install them with `cabal`).
- If you are working on a Linux distribution, you will need to edit `general_config.json` inserting the command used to open images from command line.

### Compiling Haskell sources:
Here, we assume that you have a terminal open and that you are in one of the folder containing Haskell source code in session-type-utilities subdirectories (do the same for all algorithms and type viewer)

#### Compile Algorithms:
  * Run: `ghc Checker`
 
#### Compile Type Viewer:
* Run: `ghc Viewer`

Then, move the generated executable inside proper folders:
- Move asynchronous subtyping algorithm executable to asynchronous-subtyping/your OS 
- Move fair asynchronous subtyping algorithm executable to fair-asynchronous-subtyping/your OS 
- Move synchronous subtyping algorithm executable to sync_subtyping/your OS 
- Move type viewer executable to viewer/your OS 

#### Run the Application:
Here, we assume that you have a terminal open and that you are in the root of the project.
* Run: `python SessionSubtypingTool.py`

## Distribution:
In the Release section, you can find the executable version for:
- OSx
- Windows

## Original Algorithm Repositories:
- [Asynchronous Subtyping](https://github.com/julien-lange/asynchronous-subtyping)
- [Fair Asynchronous Subtyping](https://github.com/julien-lange/fair-asynchronous-subtyping)
- [Synchronous Subtyping](https://bitbucket.org/julien-lange/modelcheckingsessiontypesubtyping/src/master/)
