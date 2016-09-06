COMPSCI631 Support Code
=======================

Setup
-----

This code is unlikely to work with Windows. It has been tested under Mac OS X
and Linux.

- Install [OPAM](https://opam.ocaml.org/doc/Install.html). I recommend
  following the directions under *Using your distribution's package
  system* instead of using the generic installation script.
  
- Insa
  
- Check the version of OCaml installed by running:

  ```
  ocaml -version
  ```
  
  You should have version 4.02.*x* installed (4.03 will not work). If the wrong version is
  installed, run the following command:
  
  ```
  opam switch 4.02.3
  ```
  
- Install the required packages:


  ```
  opam install ocamlfind pa_ounit re mparser ppx_deriving
  opam pin add compsci631 https://github.com/plasma-umass/compsci631.git 
  ```
