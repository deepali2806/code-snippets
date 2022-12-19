(* ocamlfind ocamlopt -package async,domainslib  -linkpkg -thread -o checkMVar checkMVar.ml
 *)
 open Effect
 open Async
 (* open Effect.Deep *)
 
 let num_domains = 2
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
 
 (* open Lwt.Infix *)
 
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
   | Full (_, q) -> 
                    perform (Sched.Suspend (fun r -> Queue.push (v,r) q))
   | Empty q ->
       if Queue.is_empty q then
         mv := Full (v, Queue.create ())
       else
        begin
          (* printf "\nPut: Resuming with the value"; *)
          let resume = Queue.pop q in
         resume v;
         (* printf "\nPut: After resumption with the value %d" v; *)

        end

let take_monad m = 
  let p  = (try Async.return (take m) with
          | Unhandled (Sched.Suspend f) -> 
                          (* printf "\nTake: Are we reaching here?"; *)
                          let ivar1 = Async.Ivar.create () in
                          let defer1 = Async.Ivar.read ivar1 in
                          let resumer v = (Async.Ivar.fill ivar1 v;()) in
                          f resumer;
                          (* printf "Take: Before returning the promise"; *)
                          Obj.magic defer1
                  ) 
  in p

let put_monad v m = 
  let p = (try (put v m) with
          | Unhandled (Sched.Suspend f) -> 
                  (* printf "\nPut__monad: Are we reaching here?"; *)
                  let ivar1 = Async.Ivar.create () in
                  let defer1 = Async.Ivar.read ivar1 in
                  let resumer v = (Async.Ivar.fill ivar1 v;()) in
                  f resumer;
                  (* printf "\nPut_monad: Before returning the promise"; *)
                  Obj.magic defer1
          ) 
      in p
 
end
 
 
let m = MVar.create_empty ()
 

let main () =
  let async_domain = Domain.spawn ( fun _ ->
   printf "We are creating a different Domain%!";
   MVar.put 12 m
  ) in 
  
  printf "\nAsync : Are we even starting here?%!";
  let p = (let v = MVar.take_monad m in v  >>= fun v -> Async.return (printf "Value that is returned %d" v))
  in p ;
  (* >>= fun () ->
  printf "\nAsync : Checking if we are reaching here"; *)
  Core.never_returns (Scheduler.go ()) ;
  let _ = Domain.join async_domain in ()
(* printf "\nBoth the domains are done completed%!"; *)
(* T.teardown_pool pool *)

(* 
(* let q =  
  (let _ = MVar.put_monad 12 m in Async.return () >>= fun _ -> Async.return (Async.printf "Put Heyy " )) in
  q;
  Core.never_returns (Scheduler.go ())  *)

  (* let pool = T.setup_pool ~num_domains:(num_domains - 1) () in
    T.run pool (fun () -> 
            let pr = T.async pool ( fun _ ->
                (* Printf.printf "\nDomainslib: Async Running in domain %d%!" (Domain.self () :> int); *)
                    let v = fib_par pool (40) in
                    let _ = MVar.put v m in ()
                    (* Printf.printf "\nDomainslib: Fib 45 ans %d%!" v; *)
                ) in
            let _ = T.await pool pr in ();
    );
    printf "\n Before join %!"; *)
     *)




(* let main () =
      let async_domain = Domain.spawn ( fun _ ->
        let p = (let v = MVar.take_monad m in v >>= fun v -> Async.return (Async.printf "Heyy %d" v))
        in p;
        Core.never_returns (Scheduler.go ()) 

      ) in
        
      let q =  
      (let _ = MVar.put_monad 12 m in Async.return () >>= fun _ -> Async.return (Async.printf "Put Heyy " )) in
      q;
      Core.never_returns (Scheduler.go ())  *)

 
 let  _ = main ()
