open Eio.Std
open Eio_linux

module Int63 = Optint.Int63
let block_size = try int_of_string Sys.argv.(1) with _ -> 4096
let fibres = Eio.Semaphore.make 64


let countInBuffer buffer noOfBytes = 
let total = ref 0 in
    for i = 0 to noOfBytes
    do
       if (Cstruct.get_char buffer i) = '\n' then
          total := !total + 1
    done;
    !total

let () = 
    run ~block_size:16384 @@ fun _env ->
    Switch.run @@ fun sw ->
    let filename = Sys.argv.(2) in
    let fd = Unix.handle_unix_error (openfile ~sw filename Unix.[O_RDONLY]) 0 in 
    let st = fstat fd in
    let file_size = Int63.of_int st.st_size in

    let no_fibres = (st.st_size / blkSize) in
    let clock = Eio.Stdenv.clock _env in
    let count = ref 0 in 
    let t1 = Eio.Time.now clock in
    (* traceln "The time is now %f" t1; *)
    Switch.run (fun sw ->
       for i = 0 to no_fibres
        do
             Fibre.fork ~sw
            (fun () ->  let buf = alloc () in
                        let off = i*blkSize in
                        let _ = read_exactly fd ~file_offset:(Int63.of_int off) buf blkSize in 
                        let buffer = Uring.Region.to_cstruct ~len:noOfBytes buf in
                        let cnt = countInBuffer buffer (blkSize-1) in
                        count :=  !count + cnt;
                        (* traceln "\nCount in each fibre %d currentvalue %d " cnt currentVal; *)
                        (* traceln "Offset and number of fibre %d, %d" off i; *)
		                free buf
            );
        done

    );
    let t2 = Eio.Time.now clock in
    traceln "Final count is %d \nTime Difference %f" !count (t2-.t1);
    traceln "\nSwitch is finished"



(*  let rec read_block file_offset =
        let remaining = Int63.(sub file_size file_offset) in
        if remaining <> Int63.zero then 
        begin
          let len = Int63.to_int (min (Int63.of_int block_size) remaining) in
          Eio.Semaphore.acquire fibres;
          Fibre.fork ~sw (fun () ->
              let buf = alloc () in
              let _ = Eio_linux.read_exactly ~file_offset fd buf len in 
              let buffer = Uring.Region.to_cstruct ~len:len buf in
              let cnt = countInBuffer buffer (len-1) in
              count := !count + cnt;
              free buf;
              Eio.Semaphore.release fibres
            );
          read_block Int63.(add file_offset (of_int len))
        end
      in
      read_block Int63.zero  
       *)

(* 
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
    end *)


  (* for i = 0 to no_fibres
        do
             Fibre.fork ~sw
            (fun () ->  let buf = alloc () in
                        let off = i*blkSize in
                        (* let _ = Unix.lseek (Eio_linux.FD.to_unix fd) off SEEK_SET in *)
                        let noOfBytes = read_upto fd ~file_offset:(Int63.of_int off) buf blkSize in 
                        let buffer = Uring.Region.to_cstruct ~len:noOfBytes buf in
                        let cnt = countInBuffer buffer (noOfBytes-1) in
                        let _ = Atomic.fetch_and_add count cnt in 
                        (* traceln "\nCount in each fibre %d currentvalue %d " cnt currentVal; *)
                        (* traceln "Offset and number of fibre %d, %d" off i; *)
		                free buf
            );
        done	 *)