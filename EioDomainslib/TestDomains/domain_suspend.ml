open Printf

let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 45

module T = Domainslib.Task

let m = Eio_domainslib_interface.MVar.create_empty ();;

let rec fib n =
  if n < 2 then 1
  else fib (n-1) + fib (n-2)

let rec fib_par pool n =
  if n <= 40 then fib n
  else
    let a = T.async pool (fun _ -> fib_par pool (n-1)) in
    let b = T.async pool (fun _ -> fib_par pool (n-2)) in
    T.await pool a + T.await pool b

let main =
  let pool = T.setup_pool ~name:"test1" ~num_additional_domains:(num_domains - 1) () in
  let _ = T.run pool (
        fun () ->
          let pr2 = T.async pool (fun _ -> Printf.printf "\nInside Async 2"; 
                  (* Unix.sleep (2);          *)
                  let v = 45 in
                      Eio_domainslib_interface.MVar.put v m;            
                      Printf.printf "\nAfter put 2 %! ";
                  3)  in
        let pr1 = T.async pool (fun _ -> Printf.printf "\nInside Async 1"; 
                  (* Unix.sleep (2);          *)
                  (* let p = 42 in *)
                  let p = Eio_domainslib_interface.MVar.take m in
                  Printf.printf "\nAfter take 1 %d%! " p;
                  2) 
          in

                  (* in *)
        let _ = T.await pool pr1 in
        let _ = T.await pool pr2 in
        printf "\nValue of mvar is %!"
    ) in
(* 
    let v = 45 in
    Eio_domainslib_interface.MVar.put v m; *)
  (* printf "Final Answer is %d" res; *)
printf "\nDone%!";
  T.teardown_pool pool

