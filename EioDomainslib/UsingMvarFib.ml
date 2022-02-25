open Eio_linux
open Eio.Std
let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let n = try int_of_string Sys.argv.(2) with _ -> 43
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

let m = Eio_domainslib_interface.MVar.create_empty ();;

let main =
  let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in

    run @@ fun _env ->
    T.run pool (fun () -> 
    Switch.run @@ fun sw ->
    (      
            Fibre.fork ~sw (
                fun () ->
                        traceln "\nDomainslib started ";
                        let _ = T.async pool (
                           fun _ ->
                            let v = fib_par pool n in
                            let _ = Eio_domainslib_interface.MVar.put v m in
                            Printf.printf "\nDomainslib Ends "
                       ) in

                        let v = Eio_domainslib_interface.MVar.take m in
                        traceln "\nEio Fibre 1 Ends %d" v
            ); 
            
            Fibre.fork ~sw
            (fun () -> 
            traceln "\nInside Second fibre ";
           
            traceln "\n Second Fibre Ends "; 
            );          
            
        );

    );

    T.teardown_pool pool


(* let _ = Eio_domainslib_interface.MVar.check ()  *)