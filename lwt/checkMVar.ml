(* ocamlfind ocamlopt -package lwt,lwt.unix,lwt_ppx,eio_domainslib_interface,domainslib  -linkpkg -o checkMVar checkMVar.ml 
 *)
 open Printf
 (* open Eio_domainslib_interface *)
 open Effect
 open Effect.Deep
 let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
 module T = Domainslib.Task
 
 
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
       type 'a suspend_fn = ('a resumer -> unit) -> 'a
 end
 
 
 module MVar = struct
 type 'a mv_state =
   | Full  of 'a * ('a * unit Sched.resumer) Queue.t
   | Empty of 'a Sched.resumer Queue.t
 
 type 'a t = 'a mv_state ref
 
 let create_empty () = ref (Empty (Queue.create ()))
 
 let create v = ref (Full (v, Queue.create ()))

 (* let some_function = ref None
 
 let get_funtion () =  !some_function *)

 let take mv =
  match !mv with
  | Empty q -> 
               perform (Sched.Suspend (fun r -> Queue.push r q)) 
  | Full (v, q) ->
      if Queue.is_empty q then
        (mv := Empty (Queue.create ()); v)
      else begin
        let (v', resume) = Queue.pop q in
        mv := Full (v', q);
        resume ();
        v
      end

 let put v mv =
   match !mv with
   | Full (v', q) -> 
    (* some_function := Some ((fun r -> Queue.push (v,r) q)); *)
                    perform (Sched.Suspend (fun r -> Queue.push (v,r) q))
   | Empty q ->
       if Queue.is_empty q then
         mv := Full (v, Queue.create ())
       else
         let resume = Queue.pop q in
         resume v
 

         (* let suspend_fn f  = 
         let (promise, resolver) = Lwt.task () in 
         let resumer v = (Lwt.wakeup resolver v; ()) in
         f resumer;
         promise *)

let take_monad m = 
  (* let flag = ref false in
  let q = Queue.create () in  *)
  let p:'a Lwt.t  = (try Lwt.return (take m) with
          | Unhandled (Sched.Suspend f) -> 
                          let (promise, resolver) = Lwt.task () in
                          let resumer v = (Lwt.wakeup resolver v; ()) in
                          f resumer;
                          Obj.magic promise
                  ) 
  in p

let put_monad v m = 
  let v = (try (put v m) with
          | Unhandled (Sched.Suspend f) -> 
                          let (promise, resolver) = Lwt.task () in
                          let resumer v = (Lwt.wakeup resolver v; ()) in
                          f resumer) 
  in Lwt.return v       
 
end
 
(* module MonadicMVar = struct *)
  (* Each scheduler will have its own suspend_fn implementation, we can just have its basic definition that, 
     it takes function f, and returns promise of the respective scheduler. *)
  
                      
  (* end *)
 
let m = MVar.create_empty ()
 
let main () =
  let lwt_domain = Domain.spawn( fun () ->
 
   Lwt_main.run ( 
     let p = Lwt.bind (let v = (MVar.take_monad m) in v) 
                      (fun v ->  printf "\nLwt: Hello %d%!" v; Lwt.return ())
    in
             Lwt.return (printf "\nLwt: Finally return%!")                                 
 );
   
     ) in 
 
     let pool = T.setup_pool ~num_domains:(num_domains - 1) () 
     in
         T.run pool (fun () -> 
             (* T.parallel_for pool ~start:0 ~finish:(no_Fibers - 1) ~body:(fun i -> *)
                 let pr = T.async pool ( fun _ ->
                     printf "\nDomainslib: Async Running in domain %d%!" (Domain.self () :> int);
                         let v = fib_par pool (45) in
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