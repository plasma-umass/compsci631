COMPSCI631 Support Code
=======================

Setup
-----

This code is unlikely to work with Windows. It has been tested under Mac OS X
and Linux.

- Install [OPAM](https://opam.ocaml.org/doc/Install.html). I recommend
  following the directions under *Using your distribution's package
  system* instead of using the generic installation script.
  
- Check the version of OCaml installed by running:

  ```
  ocaml -version
  ```
  
  You should have version 4.06.1 installed (4.07 will not work). If the wrong version is
  installed, run the following command:
  
  ```
  opam switch 4.06.1
  ```

  Follow any on-screen directions that the command provides. In particular, you will probably
  need to run:
  
  ```
  eval `opam config env`
  ```
  
  followed by:
  
  ```
  opam update
  ```
  

- To install the support code for this class, run:

  ```
  opam pin add compsci631 https://github.com/plasma-umass/compsci631.git
  opam install compsci631
  ```

  This will install the required support packages for COMPSCI631 assignments.

Editor Setup
============

I strongly recommend using [Visual Studio Code](https://code.visualstudio.com)
with the [OCaml and Reason IDE](https://marketplace.visualstudio.com/items?itemName=freebroccolo.reasonml) extension.

To get the most of the extension, you'll need to install [Merlin](https://github.com/ocaml/merlin). Once installed,
in the same directory as your OCaml code, create a file called `.merlin`
with the line:

    PKG compsci631

This line configures auto-completion to use the types in the course library.