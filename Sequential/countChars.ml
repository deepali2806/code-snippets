open Unix;;

let buffer_size = try int_of_string Sys.argv.(1) with _ ->  4096


let rec countInBuffer buffer noOfBytes init = 
  if noOfBytes = 0
    then init+1
  else
    begin
    if ((Char.code (Bytes.get buffer noOfBytes)) < 128) || ((Char.code (Bytes.get buffer noOfBytes)) >= 0) 
      then countInBuffer buffer (noOfBytes-1) (init + 1)
    else 
      countInBuffer buffer (noOfBytes-1) (init)
     end

let line_count filename =
  let fd_in = openfile filename [O_RDONLY] 0 in
  let st = fstat fd_in in 
  let i_n = (st.st_size / buffer_size) in
  let fd = openfile filename [O_RDONLY] 0 in 
  let buffer = Bytes.create buffer_size in
    let count = ref 0 in

    for _ = 0 to i_n
    do
        let noOfBytes = read fd buffer 0 buffer_size in
        let cnt = countInBuffer buffer (noOfBytes-1) 0 in
        count := !count + cnt
    done;
    Printf.printf "Count of Lines %d" !count;
    close fd_in

let main =
    let filename = Sys.argv.(2) in   
    let start = Unix.gettimeofday () in
    let _ = line_count filename in
    let stop = Unix.gettimeofday () in
    Printf.printf "\nDone \nTime %f\n" ( stop -. start)
