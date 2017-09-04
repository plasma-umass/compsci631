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
  
  You should have version 4.04.2 installed (4.05 will not work). If the wrong version is
  installed, run the following command:
  
  ```
  opam switch 4.04.2
  ```

  Follow any on-screen directions that the command provides.
  

- Finally, to install the support code for this class, run:

  ```
  opam pin add compsci631 https://github.com/plasma-umass/compsci631.git
  opam install compsci631
  ```

  This will install the required support packages for COMPSCI631 assignments.
