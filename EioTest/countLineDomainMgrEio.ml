(* open Eio_main;; *)
open Eio.Std
module Int63 = Optint.Int63
open Eio_linux

let blkSize = try int_of_string Sys.argv.(1) with _ -> 4096
let filename = try Sys.argv.(2) with _ -> "/home/deepali/INputs/test1.txt"



let rec countInBuffer buffer noOfBytes init = 
  if noOfBytes = 0 && (Cstruct.get_char buffer noOfBytes) = '\n'
  then
    init + 1
  else if noOfBytes = 0
    then init 
  else
    begin
    if (Cstruct.get_char buffer noOfBytes) = '\n' then
      countInBuffer buffer (noOfBytes-1) (init + 1)
    else
      countInBuffer buffer (noOfBytes-1) init
    end


let sum_to flag =
Switch.run @@ fun sw ->
    let fd = Unix.handle_unix_error (openfile ~sw filename Unix.[O_RDONLY]) 0 in 
    let st = fstat fd in
    let n_var = (st.st_size / blkSize) in
    let no_fibres =
    if (n_var mod 2) = 0
    then n_var+2
    else n_var+1
    in
    (* traceln "number of fibres %d , %d" no_fibres n_var; *)

    (* let clock = Eio.Stdenv.clock _env in *)
    let count = Atomic.make 0 in 
    (* let t1 = Eio.Time.now clock in *)
    (* traceln "The time is now %f" t1; *)
    Switch.run (fun sw ->
        for i = 0 to (no_fibres/2)-1
        do
             Fibre.fork ~sw
            (fun () ->  let buf = alloc () in

                        let off = if flag = 0 then i*blkSize
                                  else ((no_fibres/2)+i)*blkSize          
                        in
                        (* let _ = Unix.lseek (Eio_linux.FD.to_unix fd) off SEEK_SET in *)
                        let noOfBytes = try read_upto fd ~file_offset:(Int63.of_int off) buf blkSize with End_of_file -> 0 in 
                        if noOfBytes = 0 then
                            free buf
                        else 
                        begin
                        let buffer = Uring.Region.to_cstruct ~len:noOfBytes buf in
                        let cnt = countInBuffer buffer (noOfBytes-1) 0 in
                        let _ = Atomic.fetch_and_add count cnt in 
                        (* traceln "\nCount in each fibre %d currentvalue %d " cnt currentVal; *)
                        (* traceln "Offset and number of fibre %d, %d" off i; *)
		                    free buf
                        end
            );
        done	
    );
    (Atomic.get count)


let main ~domain_mgr env =
  let test i=
                       
    traceln "Iteration.%d = %d" i
      (Eio.Domain_manager.run domain_mgr
        (fun () -> sum_to i))
  in
  let clock = Eio.Stdenv.clock env in
  let t1 = Eio.Time.now clock in
  Fibre.both
    (fun () -> test 0)
    (fun () -> test 1);
  let t2 = Eio.Time.now clock in
  traceln "Final count is \nTime Difference %f" (t2-.t1)


let () = Eio_main.run @@ fun env ->
  main ~domain_mgr:(Eio.Stdenv.domain_mgr env) env