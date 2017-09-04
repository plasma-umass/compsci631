val test :
  Ppx_test.Location.t -> Ppx_test.Longident.t  -> (unit -> bool) -> unit
val test_unit :
  Ppx_test.Location.t -> Ppx_test.Longident.t -> (unit -> unit) -> unit
val test_fail :
  Ppx_test.Location.t -> Ppx_test.Longident.t -> (unit -> unit) -> unit