open Eio.Std
open Eio_linux

module Int63 = Optint.Int63
let block_size = try int_of_string Sys.argv.(1) with _ -> 4096
let fibres = Eio.Semaphore.make 64


let rec countInBuffer buffer noOfBytes init = 
  if noOfBytes = 0
    then init+1
  else
    if ((Char.code (Cstruct.get_char buffer noOfBytes)) < 128) && ((Char.code (Cstruct.get_char buffer noOfBytes)) >= 0) 
    then
      countInBuffer buffer (noOfBytes-1) (init + 1)
    else 
      countInBuffer buffer (noOfBytes-1) (init)
  

let rec countInBufferString buffer noOfBytes init = 
  if noOfBytes = 0
    then init+1
  else
    if ((Char.code (String.get buffer noOfBytes)) < 128) && ((Char.code (String.get buffer noOfBytes)) >= 0) 
    then
      countInBufferString buffer (noOfBytes-1) (init + 1)
    else 
      countInBufferString buffer (noOfBytes-1) (init)
  
(*   
let rec countInBuffer buffer noOfBytes init = 
  if noOfBytes = 0
    then init+1
  else
      countInBuffer buffer (noOfBytes-1) (init + 1) *)


let () = 
    run ~block_size:block_size @@ fun _env ->
    let filename = Sys.argv.(2) in
    Switch.run @@ fun sw ->
    let fd = Unix.handle_unix_error (openfile ~sw filename Unix.[O_RDONLY]) 0 in 
    let st = fstat fd in
    (* let file_size = Int63.of_int st.st_size in *)
    let clock = Eio.Stdenv.clock _env in
    let count = ref 0 in 
    let no_fibres = (st.st_size / block_size) in
    let t1 = Eio.Time.now clock in
    Switch.run ( fun sw ->
      for i = 0 to no_fibres
        do
             Fibre.fork ~sw
            (fun () ->  let buf = alloc () in
                        let off = i*block_size in

                        let noOfBytes = read_upto fd ~file_offset:(Int63.of_int off) buf block_size in 
                        let buffer = Uring.Region.to_string ~len:block_size buf in
                        let cnt = countInBufferString buffer (noOfBytes-1) 0 in
                        (* let cnt = countInBuffer buffer (noOfBytes-1) 0 in *)
                  
                        (* let buffer = Uring.Region.to_cstruct ~len:block_size buf in
                        let cnt = countInBuffer buffer (noOfBytes-1) 0 in    *)
                        count :=  !count + cnt;
                        (* if i = no_fibres then 
                        begin
                          let bs = (st.st_size) - (i)*block_size in
                          let _ = read_exactly fd ~file_offset:(Int63.of_int off) buf bs in
                          let buffer = Uring.Region.to_cstruct ~len:bs buf in
                          let cnt = countInBuffer buffer (bs-1) 0 in
                          count :=  !count + cnt
                        end 
                        else
                          begin
                            let _ = read_exactly fd ~file_offset:(Int63.of_int off) buf block_size in 
                            let buffer = Uring.Region.to_cstruct ~len:block_size buf in
                            let cnt = countInBuffer buffer (block_size-1) 0 in
                            count :=  !count + cnt
                          end; *)
                        (* traceln "\nCount in each fibre %d currentvalue %d " cnt currentVal; *)
                        (* traceln "Offset and number of fibre %d, %d" off i; *)
		                free buf
            );
        done
      
      );

    let t2 = Eio.Time.now clock in
    traceln "Final count is %d \nEnd Time Difference using Readupto %f" !count (t2-.t1);
    traceln "\nSwitch is finished"



(* let rec read_block file_offset =
        let remaining = Int63.(sub file_size file_offset) in
        if remaining <> Int63.zero then 
        begin
          let len = Int63.to_int (min (Int63.of_int block_size) remaining) in
          Eio.Semaphore.acquire fibres;
          Fibre.fork ~sw (fun () ->
              let buf = alloc () in
              let _ = Eio_linux.read_exactly ~file_offset fd buf len in 
              let buffer = Uring.Region.to_cstruct ~len:len buf in
              let cnt = countInBuffer buffer (len-1) 0 in
              count := !count + cnt;
              free buf;
              Eio.Semaphore.release fibres
            );
          read_block Int63.(add file_offset (of_int len))
        end
      in
      read_block Int63.zero *)

(* 
let rec read_exactly ~file_offset infd buf =
  let got = Eio_linux.readv ~file_offset infd [buf] in
  if got <> Cstruct.length buf then
    read_exactly ~file_offset:(Int63.add file_offset (Int63.of_int got)) infd (Cstruct.shift buf got)


count := 0;
let _ = Unix.lseek (Eio_linux.FD.to_unix fd) 0 SEEK_SET in
let t1 = Eio.Time.now clock in
    Switch.run ( fun sw ->
      let rec read_block file_offset =
        let remaining = Int63.(sub file_size file_offset) in
        if remaining <> Int63.zero then 
        begin
          let len = Int63.to_int (min (Int63.of_int block_size) remaining) in
          Eio.Semaphore.acquire fibres;
          Fibre.fork ~sw (fun () ->
              let buf = Cstruct.create_unsafe len in
              read_exactly ~file_offset:file_offset fd buf;
              let cnt = countInBuffer buf len 0 in
              count := !count + cnt;
              Eio.Semaphore.release fibres
            );
          read_block Int63.(add file_offset (of_int len))
        end
      in
      read_block Int63.zero);

    let t2 = Eio.Time.now clock in
    traceln "Final count is %d \nTime Difference using readv %f" !count (t2-.t1);
     *)