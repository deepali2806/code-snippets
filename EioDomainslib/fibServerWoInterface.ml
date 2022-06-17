open Eio_linux
open Eio.Std
let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 41
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

(* let m = Array.make 5 (Eio_domainslib_interface.MVar.create_empty ());; *)

let main =
  let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in

    run @@ fun _env ->
      (* let clock = Eio.Stdenv.clock _env in (); *)
    T.run pool (fun () -> 

   Switch.run @@ fun sw ->
    ( 
         
            Fiber.fork ~sw (
                fun () ->
                        for i = 0 to 4 do
                        
                        let _ = T.async pool (
                           fun _ ->
                           Printf.printf "\nDomains fibpar starting %d %!" i;
                            let v = fib_par pool (n+i) in
                            (* let _ = Eio_domainslib_interface.MVar.put v (m.(i)) in *)
                            Printf.printf "\nDomainslib Ends %d %d %!" i v
                       ) in Printf.printf "\nReached here at the end of domainslib function%!"
                        done
            );       
            
        );

    );

    T.teardown_pool pool 