open Eio.Std
open Eio_linux
open Unix;;

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


let rec countInBufferBytes buffer noOfBytes init = 
  if noOfBytes = 0
    then init+1
  else
    begin
    if ((Char.code (Bytes.get buffer noOfBytes)) < 128) || ((Char.code (Bytes.get buffer noOfBytes)) >= 0) 
      then countInBufferBytes buffer (noOfBytes-1) (init + 1)
    else 
      countInBufferBytes buffer (noOfBytes-1) (init)
     end

let line_count filename =
    let fd = openfile filename [O_RDONLY] 0 in
    let buffer = Bytes.create block_size in
    let noOfBytes = read fd buffer 0 block_size in
    let t1 = Unix.gettimeofday () in
    let cnt = countInBufferBytes buffer (noOfBytes-1) 0 in    
    let t2 = Unix.gettimeofday () in
    traceln "\nTime Difference for Bytes %f" (t2-.t1);    
    traceln "Count in Bytes %d" cnt;
    close fd

let () = 
    run ~block_size:block_size @@ fun _env ->
    let filename = Sys.argv.(2) in
    Switch.run @@ fun sw ->
    let fd = Unix.handle_unix_error (Eio_linux.openfile ~sw filename Unix.[O_RDONLY]) 0 in 
    
    Switch.run ( fun sw ->
            Fibre.fork ~sw
            (fun () ->  let buf = alloc () in
                        let off = 0 in
                        let _ = read_exactly fd ~file_offset:(Int63.of_int off) buf block_size in 
                        let buffer = Uring.Region.to_cstruct ~len:block_size buf in
                        let t1 = Unix.gettimeofday () in
                        let cnt = countInBuffer buffer (block_size-1) 0 in
                        let t2 = Unix.gettimeofday () in
                        traceln "\nTime Difference for Cstruct %f" (t2-.t1);
                        traceln "Count in cstruct %d" cnt;
		                free buf
            );
      );

    
    traceln "\nCstruct is finished and Bytes is starting....";

    let _ = line_count filename in
    traceln "\nBytes is finished";
