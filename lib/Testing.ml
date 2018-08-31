let run_tests () =
  match Sys.getenv_opt "DISABLE_PPX_TEST" with
    | None -> Ppx_test.Test.collect ()
    | Some _ -> Printf.printf "Test cases disabled.\n%!"