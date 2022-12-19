(* ocamlfind ocamlopt -package async,domainslib  -linkpkg -thread -o checkingAsyncwidDomain checkingAsyncwidDomain.ml
 *)
 open Effect
 (* open Effect.Deep *)
 
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
 
 
(* let m = MVar.create_empty () *)
 

let main () =
      let async_domain = Domain.spawn ( fun _ ->
        Printf.printf "Hello from outside the async_domains%!"
      ) in

      let open Async in
      (Async.printf "Heyy %!");
       Core.never_returns (Scheduler.go ()) 
       (* Domain.join async_domain *)

        
 let  _ = main ()
