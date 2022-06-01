open Eio_linux.Low_level
open Eio.Std
let num_domains = try int_of_string Sys.argv.(1) with _ -> 2
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

let p = ref 0

let main =
  let pool = T.setup_pool ~num_additional_domains:(num_domains - 1) () in

    Eio_linux.run @@ fun _env ->
    T.run pool (fun () -> 
    Switch.run @@ fun sw ->
    ( 
            let filename = Sys.argv.(1) in
            let fd = Unix.handle_unix_error (openfile ~sw filename Unix.[O_RDONLY]) 0 in
            let buf = alloc_fixed_or_wait () in 
                let clock = Eio.Stdenv.clock _env in

            Fiber.fork ~sw (
                fun () ->
                      (* Computation time *)
                        traceln "\nDomainslib started %!";
                        let t1 = Eio.Time.now clock in
                        let _ = T.async pool (
                            fun _ ->
                            Printf.printf "Domains fibpar starting %!";
                            let v = fib_par pool n in
                            let _ = Eio_domainslib_interface.MVar.put v m in
                            Printf.printf "\nDomainslib Ends %!"
                       ) in
                        traceln "Before mvar take%!";
                        p := Eio_domainslib_interface.MVar.take m;
                        let t2 = Eio.Time.now clock in
                        traceln "\nComputation Time %f%!" (t2-.t1);
                        traceln "\nEio Fibre 1 Ends %d%!" !p
            ); 
            
            Fiber.fork ~sw
            (fun () -> 
            
            (* Doing some interaction & Find Response Time *)
                while (!p = 0)
                do
                    let t1 = Eio.Time.now clock in 
                    (* Simulating terminal input as file input and output *)
                    let _ = read_exactly fd buf 10 in 
                    (* traceln "\nInside Second fibre "; *)
                    Eio.Time.sleep clock 0.08;
                    traceln "\n Second Fibre says Hello %!"; 
                    print_endline (Uring.Region.to_string ~len:10 buf);
                    let t2 = Eio.Time.now clock in
                    traceln "Response Time %f%!" (t2-.t1);
                done

            );         
            
            (* free_fixed buf;  *)
            
        );

    );

    T.teardown_pool pool


(* let _ = Eio_domainslib_interface.MVar.check ()  *)


(* open Eio_linux
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
         
            Fiber.fork ~sw (
                fun () ->
                        traceln "\nDomainslib started ";
                        let _ = T.async pool (
                           fun _ ->
                           Printf.printf "Domains fibpar starting %!";
                            let v = fib_par pool n in
                            let _ = Eio_domainslib_interface.MVar.put v m in
                            Printf.printf "\nDomainslib Ends "
                       ) in
                        traceln "Before mvar take";
                        let v = Eio_domainslib_interface.MVar.take m in
                        traceln "\nEio Fibre 1 Ends %d" v
            ); 
            
            Fiber.fork ~sw
            (fun () -> 
            traceln "\nInside Second fibre ";
           
            traceln "\n Second Fibre Ends "; 
            );          
            
        );

    );

    T.teardown_pool pool *)


(* let _ = Eio_domainslib_interface.MVar.check ()  *)