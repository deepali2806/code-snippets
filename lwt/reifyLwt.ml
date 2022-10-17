(* ocamlfind ocamlopt -package lwt,lwt.unix,lwt_ppx,eio_domainslib_interface,domainslib  -linkpkg -o reifyLwt reifyLwt.ml 
 *)
 
open Printf
(* open Eio_domainslib_interface *)

open Effect
open Effect.Deep
open Lwt.Infix

(* let m = Eio_domainslib_interface.MVar.create_empty () *)
module Sched = struct
      type 'a resumer = 'a -> unit
      type _ Effect.t += Suspend : ('a resumer -> unit) -> 'a Effect.t
end


module MVar = struct
type 'a mv_state =
  | Full  of 'a * ('a * unit Sched.resumer) Queue.t
  | Empty of 'a Sched.resumer Queue.t

type 'a t = 'a mv_state ref

let create_empty () = ref (Empty (Queue.create ()))

let create v = ref (Full (v, Queue.create ()))

let put v mv =
  match !mv with
  | Full (v', q) -> perform (Sched.Suspend (fun r -> Queue.push (v,r) q))
  | Empty q ->
      if Queue.is_empty q then
        mv := Full (v, Queue.create ())
      else
        let resume = Queue.pop q in
        resume v

let take mv =
  match !mv with
  | Empty q -> perform (Sched.Suspend (fun r -> Queue.push r q))
  | Full (v, q) ->
      if Queue.is_empty q then
        (mv := Empty (Queue.create ()); v)
      else begin
        let (v', resume) = Queue.pop q in
        mv := Full (v', q);
        resume ();
        v
      end


end

let m = MVar.create_empty ()

let x = Lwt_mvar.create_empty ()

let suspend_fn tag mv = 
      let f = (match tag with
      | "take" ->   (match (!mv) with
                        | MVar.(Empty q) ->(fun r -> Queue.push r q)
                        | _ -> fun r -> ())
      | "put" -> fun _ -> ())
      in 
      printf "\nAre we reaching inside suspend function?%!";
      let resumer v = (Lwt_mvar.put x v;()) in 
      f resumer;
      printf "\nWe should block here now!%!";
      let y = Lwt_mvar.take x in
      y




     
let main () =
let lwt_domain = Domain.spawn( fun () ->
  Lwt_main.run ( 
                  let _ = Lwt.try_bind (fun () -> let v = (try Lwt.return (MVar.take m) with
                              | exn -> printf "Inside Exception%!"; raise Exit)
                  in v) 
                  (fun v ->  printf "\nHello %d%!" v; Lwt.return ())
                  (fun _ -> let y = suspend_fn "take" m in printf "\nOhk so reaching here guys%!";y >>= 
                  fun v ->
                  printf "\nHello %d%!" v; Lwt.return ())

                 in
                        Lwt.return (printf "\nFinally return%!")                                 
			);
) in
let _ = MVar.put 45 m in 
printf "Putting value into mvar";
let _ = Domain.join lwt_domain in
printf "\nBoth the domains are done completed"


let _ = main ()

