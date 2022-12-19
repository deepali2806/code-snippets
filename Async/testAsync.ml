(* ocamlfind ocamlopt -package async,domainslib  -linkpkg -thread -o test testAsync.ml *)
(* open Printf
open Core *)
open Async

let run () =  
  let _ = Async.Deferred.bind 
                   (let v = 12 in Async.return v) 
                   ~f:(fun v ->  printf "\nAsync: Hello %d%!" v; Async.return ())
in ()

let () = run ();
       Core.never_returns (Async.Scheduler.go ())

