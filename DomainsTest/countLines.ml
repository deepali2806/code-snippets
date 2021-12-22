open Unix;;

let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let buffer_size = try int_of_string Sys.argv.(2) with _ ->  4096


let rec countInBuffer buffer noOfBytes init = 
  if noOfBytes = 0 && (Bytes.get buffer noOfBytes) = '\n'
  then
    init + 1
  else if noOfBytes = 0
    then init 
  else
    begin
    if (Bytes.get buffer noOfBytes) = '\n' then
      countInBuffer buffer (noOfBytes-1) (init + 1)
    else
      countInBuffer buffer (noOfBytes-1) init
    end
         
let parallel_line_count pool filename =
  let fd_in = openfile filename [O_RDONLY] 0 in
  let st = fstat fd_in in 
  let i_n = (st.st_size / buffer_size) in
  let m = Mutex.create () in
  let v = Domainslib.Task.parallel_for_reduce ~start:0 ~finish:(i_n) ~body:(fun i ->
        let offset = i*buffer_size in    
        let buffer = Bytes.create buffer_size in

        Mutex.lock m;
        let _ = lseek fd_in (offset) SEEK_SET in
        let noOfBytes = read fd_in buffer 0 buffer_size in
        Mutex.unlock m;

        let cnt = countInBuffer buffer (noOfBytes-1) 0 in
        (* Printf.printf "\nLength of cnt %d" cnt;  *)
        cnt
  ) pool (+) 0 in
    Printf.printf "Count of Lines %d" v

let main =
    let start = Unix.gettimeofday () in
    let filename = Sys.argv.(3) in
    let pool = Domainslib.Task.setup_pool ~num_additional_domains:(num_domains - 1) () in
    let _ = parallel_line_count pool filename in
    let stop = Unix.gettimeofday () in
    Domainslib.Task.teardown_pool pool;
    Printf.printf "\nDone \nTime %f\n" ( stop -. start)
