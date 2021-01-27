# session-subtyping-tool

## Requirements:

You will need Graphviz (dot) installed on your machine with the dot command inserted in your PATH variable to generate graphs:

- [Here](https://www2.graphviz.org/Packages/stable/windows/10/cmake/Release/x64/graphviz-install-2.44.1-win64.exe) you can find Graphviz for Windows. Aftar having    Graphviz installed, you will have to execute "dot -c" as administrator to make the tool supporting different image formats.
- To install Graphviz for OSx you need a package manager. You can use [Homebrew](https://brew.sh) and install Graphiz with "brew install graphviz"
- Compile Haskell sources (you can find them in session type utilities).
- Put the Haskell generated executables in the correct folders: for algorithm executables the correct folder is name algoritmh/your OS, while the correct folder for type_viewer is viewer/your OS
- If you are using a Linux distribution in general_config.json set the command used to open images from command line
