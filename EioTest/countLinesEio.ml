open Eio.Std
open Eio_linux
module Int63 = Optint.Int63
let blkSize = try int_of_string Sys.argv.(1) with _ -> 4096

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


let () = 
    run @@ fun _env ->
    Switch.run @@ fun sw ->
    let fd = Unix.handle_unix_error (openfile ~sw (Sys.argv.(2)) Unix.[O_RDONLY]) 0 in 
    let st = fstat fd in
    let no_fibres = (st.st_size / blkSize) in
    let clock = Eio.Stdenv.clock _env in
    let count = Atomic.make 0 in 
    let t1 = Eio.Time.now clock in
    (* traceln "The time is now %f" t1; *)
    Switch.run (fun sw ->
        for i = 0 to no_fibres
        do
             Fibre.fork ~sw
            (fun () ->  let buf = alloc () in
                        let off = i*blkSize in
                        (* let _ = Unix.lseek (Eio_linux.FD.to_unix fd) off SEEK_SET in *)
                        let noOfBytes = read_upto fd ~file_offset:(Int63.of_int off) buf blkSize in 
                        let buffer = Uring.Region.to_cstruct ~len:noOfBytes buf in
                        let cnt = countInBuffer buffer (noOfBytes-1) 0 in
                        let _ = Atomic.fetch_and_add count cnt in 
                        (* traceln "\nCount in each fibre %d currentvalue %d " cnt currentVal; *)
                        (* traceln "Offset and number of fibre %d, %d" off i; *)
		                free buf
            );
        done	
    );
    let t2 = Eio.Time.now clock in
    traceln "Final count is %d \nTime Difference %f" (Atomic.get count) (t2-.t1);
    traceln "\nSwitch is finished"
