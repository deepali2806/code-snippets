
(* open Unix *)
let block_size = try int_of_string Sys.argv.(1) with _ -> 4096

let rec countInBufferCstruct buffer noOfBytes init = 
  if noOfBytes = 0
    then init+1
  else
    if ((Char.code (Cstruct.get_char buffer noOfBytes)) < 128) && ((Char.code (Cstruct.get_char buffer noOfBytes)) >= 0) 
    then
      countInBufferCstruct buffer (noOfBytes-1) (init + 1)
    else 
      countInBufferCstruct buffer (noOfBytes-1) (init)


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


let rec countInBufferBigarray buffer noOfBytes init = 
  if noOfBytes = 0
    then init+1
  else
    begin
    if ((Char.code (Bigarray.Array1.get buffer noOfBytes)) < 128) || ((Char.code (Bigarray.Array1.get buffer noOfBytes)) >= 0) 
      then countInBufferBigarray buffer (noOfBytes-1) (init + 1)
    else 
      countInBufferBigarray buffer (noOfBytes-1) (init)
     end


let test_bytes () =
    let buffer = Bytes.make block_size 'A' in
    let t1 = Unix.gettimeofday () in
    let _ = countInBufferBytes buffer (block_size-1) 0 in    
    let t2 = Unix.gettimeofday () in
    Printf.printf "\nTime Difference for Bytes\t %f" (t2-.t1)   
    (* Printf.printf "\nCount in Bytes %d" cnt *)


(* Cstruct created using of_bigarray *)
let test_cstruct_ba () =
    let b = Bigarray.Array1.create Char Bigarray.c_layout block_size in
    let _ = Bigarray.Array1.fill b 'A' in    
    let buffer = Cstruct.of_bigarray b in
    let t1 = Unix.gettimeofday () in
    let _ = countInBufferCstruct buffer (block_size-1) 0 in    
    let t2 = Unix.gettimeofday () in
    Printf.printf "\nTime Difference for Cstruct_ba\t %f" (t2-.t1)  


(* Cstruct created using of_string *)
let test_cstruct_str () =
    let str = String.make block_size 'A' in
    let buffer = Cstruct.of_string str in
    let t1 = Unix.gettimeofday () in
    let _ = countInBufferCstruct buffer (block_size-1) 0 in    
    let t2 = Unix.gettimeofday () in
    Printf.printf "\nTime Difference for Cstruct_str  %f" (t2-.t1)  


let test_bigarray () = 
    let buffer = Bigarray.Array1.create Char Bigarray.c_layout block_size in
    let _ = Bigarray.Array1.fill buffer 's' in
    let t1 = Unix.gettimeofday () in
    let _ = countInBufferBigarray buffer (block_size-1) 0 in    
    let t2 = Unix.gettimeofday () in
    Printf.printf "\nTime Difference for bigarray \t %f" (t2-.t1)   


let () = 
    for _ = 0 to 4
    do
    (* Printf.printf "\nIteration %d------------------------------\n" i; *)
    let _ = test_bytes () in
    let _ = test_cstruct_str () in
    let _ = test_cstruct_ba () in
    let _ = test_bigarray () in
    Printf.printf "\n------------------------------";
    done