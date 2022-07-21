# Session-subtyping-tool
The GUI-based tool is also avalable as executables. [Here](https://github.com/LBacchiani/session-subtyping-tool/releases/) you can find session types examples and executables for:
- OSx (download macOS.dmg and drag the tool into the Applications folder and double click on the dragged application).
- Windows (download win.zip, unzip it and double click on Launcher.lnk in the main unzipped directory).

## Executable and Source Code Version Requirements:

You will need Graphviz (dot) installed on your machine with the dot command path inserted in your PATH variable to generate graphs:

- [Here](https://www2.graphviz.org/Packages/stable/windows/10/cmake/Release/x64/graphviz-install-2.44.1-win64.exe) you can find Graphviz for Windows
- To install Graphviz for OSx you need a package manager. You can use [Homebrew](https://brew.sh) and install Graphiz with `brew install graphviz`
- The `dot` command has to be in your PATH.
- Your Graphviz installation should support different formats: `png, jpeg, pdf, svg`. You can check this executing `dot -v`. If you find out that your installation does not support the aforementioned formats, execute `dot -c`

Make sure that the downloaded version of Graphviz is actually under execution: `dot -v` must print the correct version when executed (put the Graphviz path at the beginning of the system PATH variable)

## Source Code Usage:

### Requirements:
Besides the aforementioned requirements, to use the source code version you will need:
- [Python (3.X version)](https://www.python.org/downloads/)
- [Ghc](https://www.haskell.org/platform/)
- Depending on your setup, you might need to install extra Python packages (Python's complaints will help you figure these out and you can install them).
- Depending on your setup, you might need to install extra Haskell packages (GHC's complaints will help you figure these out and you can install them with `cabal`).

### Compiling Haskell sources:
Here, we assume that you have a terminal open and that you are in one of the folders containing Haskell source code in session-type-utilities subdirectories (do the same for all algorithms, type viewer and type parser)

#### Compile Algorithms:
* Run: `ghc Checker`
 
#### Compile Type Viewer:
* Run: `ghc Viewer`

#### Compile Type Parser:
* Run: `ghc Main`

Then, move the generated executable inside proper folders:
- Move asynchronous subtyping algorithm executable to asynchronous-subtyping/your OS 
- Move fair asynchronous subtyping algorithm executable to fair-asynchronous-subtyping/your OS 
- Move synchronous subtyping algorithm executable to sync_subtyping/your OS 
- Move type viewer executable to viewer/your OS 
- Move type parser executable to parser/your OS


### Run the Application:
Here, we assume that you have a terminal open and that you are in the root of the project.
* Run: `python SessionSubtypingTool.py`

## Original Algorithm Repositories:
- [Asynchronous Subtyping](https://github.com/julien-lange/asynchronous-subtyping)
- [Fair Asynchronous Subtyping](https://github.com/julien-lange/fair-asynchronous-subtyping)
- [Synchronous Subtyping](https://bitbucket.org/julien-lange/modelcheckingsessiontypesubtyping/src/master/)
