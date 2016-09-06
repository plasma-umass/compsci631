COMPSCI631 Support Code
=======================

Setup
-----

This code is unlikely to work with Windows. It has been tested under Mac OS X
and Linux.

- Install [OPAM](https://opam.ocaml.org/doc/Install.html). I recommend following the directions under *Using your distribution's package system* instead of using the generic installation script.

- From the command line, run:

  ```
  opam repository add plasma-opam https://github.com/plasma-umass/opam-repository.git
  opam install compsci631
  ```
  
  If you get the error *compsci631 is not available because it requires OCaml >= 4.03.0.*, you'll need to run the following command to install it before you install the package:
  
  ```
  opam switch 4.03.0
  ```

