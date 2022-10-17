(* ocamlfind ocamlopt -package lwt,lwt.unix,lwt_ppx,eio_domainslib_interface,domainslib  -linkpkg -o lwt_domainslib lwt_domainslib.ml 
 *)
open Printf
open Eio_domainslib_interface
open Effect
open Effect.Deep
(* let promises = BatDynArray.create () *)
let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
module T = Domainslib.Task

exception Suspend_take
(* exception Suspend_put of 'a *)


let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let rec fib_par pool n =
  if n <= 40 then fib n
  else
    let a = T.async pool (fun _ -> fib_par pool (n-1)) in
    let b = T.async pool (fun _ -> fib_par pool (n-2)) in
    T.await pool a + T.await pool b

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

let suspend_take mv = 
  (* When suspend function is mailnly to handle exception from take function *)
      let f = (
       (match (!mv) with
                        | MVar.(Empty q) ->(fun r -> Queue.push r q)
                        | _ -> fun r -> ())
      )
      in 
      printf "\nLwt: Are we reaching inside suspend function?%!";
      let resumer v = (Lwt_mvar.put x v;()) in 
      f resumer;
      printf "\nLwt: We should block here now!%!";
      let y = Lwt_mvar.take x in
      y

let suspend_put v mv = 
  let f = (
       (match (!mv) with
                | MVar.(Full (v', q)) ->(fun r -> Queue.push (v, r) q)
                | _ -> fun r -> ())
      )
      in 
      printf "\nLwt: Are we reaching inside suspend function?%!";
      let resumer v = (Lwt_mvar.take x;()) in 
      f resumer;
      printf "\nLwt: We should block here now!%!";
      let y = Lwt_mvar.put x v in
      y





let main () =
 let lwt_domain = Domain.spawn( fun () ->

  Lwt_main.run ( 
    let _ = Lwt.try_bind (fun () -> let v = (try Lwt.return (MVar.take m) with
                | exn -> printf "Lwt: Inside Exception%!"; raise Suspend_take)
    in v) 
    (fun v ->  printf "\nLwt: Hello %d%!" v; Lwt.return ())
    (fun e -> match e with 
              | Suspend_take -> let y = suspend_take m in printf "\nLwt: Ohk so reaching here guys%!";y >>= 
    fun v ->
    printf "\nHello %d%!" v; Lwt.return ())

   in
          Lwt.return (printf "\nLwt: Finally return%!")                                 
);
  
    ) in 

    let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () 
    in
        T.run pool (fun () -> 
            (* T.parallel_for pool ~start:0 ~finish:(no_Fibers - 1) ~body:(fun i -> *)
                let pr = T.async pool ( fun _ ->
                    printf "\nDomainslib: Async Running in domain %d%!" (Domain.self () :> int);
                        let v = fib_par pool (40) in
                        let _ = MVar.put v m in
                        printf "\nDomainslib: Fib 45 ans %d%!" v;
                    ) in
                let _ = T.await pool pr in ();
            (* )  *)
    );
        
    let _ = Domain.join lwt_domain in 
    printf "\nBoth the domains are done completed%!";
    T.teardown_pool pool

let  _ = main ()