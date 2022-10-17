(*ocamlfind ocamlopt -package lwt,lwt.unix -linkpkg -o basic basic.ml *)
open Lwt
open Lwt.Infix
open Printf
open Eio_domainslib_interface
open Effect
open Effect.Deep

let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

(* let m = Eio_domainslib_interface.MVar.create_empty () *)

let () =
  let rec handle_io () =
    let%lwt () = Lwt_io.printl "Handling I/O" in
    let%lwt () = Lwt_unix.sleep 0.1 in
    handle_io ()
  in

  let rec compute n =
    if n = 0 then
      Lwt.return ()
    else
      ( 
				match_with Eio_domainslib_interface.MVar.take m
																						{ retc = (fun v -> Lwt.return (printf "\nReturned %d" v; v) );
																							exnc = raise;
																							effc = fun (type a) (e : a Effect.t) ->
																								match e with
																								| Sched.Suspend _ -> Some (fun (k: (a,_) continuation) ->
																									(* How to capture a continuation here? Is it even needed? *)
																									Lwt.pause ();
																									Lwt.return (printf "\nSuspend Effect?\n"; 455)
																								
																								)
																						} 
				)
      compute (n - 1)
  in

  Lwt.async handle_io;
  Lwt_main.run (compute 100_000_000)

(* 
let p ()= Lwt.join [
			Lwt.return (fib 40) >>= (fun ans -> Lwt.return (printf "\nFibonacci Answer \n%!"));
			Lwt.return (printf "\n Hello World%!")		
		]

(* let m () = Lwt.bind (Lwt_unix.sleep 3.) (fun _  -> Lwt.return (printf "\nm: Hello World%!")) *)
				
let t1 () =  Lwt.try_bind (fun () -> 
	(* (
	let x  =  *)
																			( match_with Eio_domainslib_interface.MVar.take m
																						{ retc = (fun v -> Lwt.return (printf "\nReturned %d" v; v) );
																							exnc = raise;
																							effc = fun (type a) (e : a Effect.t) ->
																								match e with
																								| Sched.Suspend _ -> Some (fun (k: (a,_) continuation) ->
																									(* How to capture a continuation here? Is it even needed? *)
																									Lwt.return (printf "\nSuspend Effect?\n"; 455)
																								
																								)
																						} 
																			) 
																		(* in 
																			Lwt.return x
																	 )  *)
												 ) 
													(fun x -> Lwt.return (printf "\nWhat is taken? %d\n" x) )
													(function
														| exn -> Lwt.pause (); printf "\n After Pause\n%!"; Lwt.return (printf "\n Exn: Suspend effect?\n%!") )

														
let () = 
let p1 =   (printf "Before Pause%!") ;Lwt.pause (); Lwt.return (printf "After Pause%!") in
let p2 =  Lwt.return (printf "\nWhat is taken? %!\n")	in
let p3 =  Lwt.join [p1; p2] in
Lwt_main.run p3 *)
																							

(* let _ = t2 () *)


(* This works: and control goesto second callback as well *)
(* let t () =  Lwt.try_bind (fun () -> (let x  = (try (Eio_domainslib_interface.MVar.take m)
													with
													| exn -> printf "\n After Mvar take effect?\n%!"; 42)
													 in
						Lwt.return x)) 
			(fun x -> Lwt.return (printf "\nWhat is taken? %d\n" x) )
			(function
					(* | Invalid_argument msg ->
						let* () = log "CONF: invalid value for %s" key in
						raise (Config_error "Cannot normalise value") *)
				| exn -> Lwt.pause (); printf "\n After Pause\n%!"; Lwt.return (printf "\n Suspend effect?\n%!") ) *)

(* let _ = Lwt_main.run (t ()) *)

