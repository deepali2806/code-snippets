open Eio.Std
open Eio_linux
module Int63 = Optint.Int63
let block_size = try int_of_string Sys.argv.(1) with _ -> 4096
let fibres = Eio.Semaphore.make 64

let rec countInBuffer buffer noOfBytes init = 
  if noOfBytes = 0
    then init
  else
      countInBuffer buffer (noOfBytes-1) (init + 1)


let rec read_exactly ~file_offset infd buf =
  let got = Eio_linux.readv ~file_offset infd [buf] in
  if got <> Cstruct.length buf then
    read_exactly ~file_offset:(Int63.add file_offset (Int63.of_int got)) infd (Cstruct.shift buf got)

let () = 
    run ~block_size:16384 @@ fun _env ->
    let filename = Sys.argv.(2) in
    Switch.run @@ fun sw ->
    let fd = Unix.handle_unix_error (openfile ~sw filename Unix.[O_RDONLY]) 0 in 
    let st = fstat fd in
    let file_size = Int63.of_int st.st_size in
    let clock = Eio.Stdenv.clock _env in
    let count = ref 0 in 
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
count := 0;
let _ = Unix.lseek (Eio_linux.FD.to_unix fd) 0 SEEK_SET in
    let t1 = Eio.Time.now clock in
    Switch.run ( fun sw ->
      let rec read_block file_offset =
        let remaining = Int63.(sub file_size file_offset) in
        (* traceln "Reached here %d" (Int63.to_int remaining); *)
        if remaining <> Int63.zero then 
        begin
          let len = Int63.to_int (min (Int63.of_int block_size) remaining) in
          Eio.Semaphore.acquire fibres;
          Fibre.fork ~sw (fun () ->
              let buf = alloc () in
              let _ = Eio_linux.read_exactly ~file_offset fd buf len in 
              let buffer = Uring.Region.to_cstruct ~len:len buf in
              let cnt = countInBuffer buffer len 0 in
              count := !count + cnt;
              free buf;
              Eio.Semaphore.release fibres
            );
          read_block Int63.(add file_offset (of_int len))
        end
      in
      read_block Int63.zero);

    let t2 = Eio.Time.now clock in
    traceln "Final count is %d \nTime Difference using Readupto %f" !count (t2-.t1);





    traceln "\nSwitch is finished"
