open Unix;;
let buffer_size = 4096

let num_domains = 4

let parallel_line_count pool filename =
  let fd_in = openfile filename [O_RDONLY] 0 in
  let st = fstat fd_in in 
  let i_n = (st.st_size / buffer_size) in
  let count = ref 0 in 
  let m = Mutex.create () in
  Domainslib.Task.parallel_for pool ~start:0 ~finish:(i_n) ~body:(fun i ->
    let offset = i*buffer_size in
    
        let buffer = Bytes.create buffer_size in
        let _ = lseek fd_in (offset) SEEK_SET in
        let noOfBytes = read fd_in buffer 0 buffer_size in
        let cnt = ref 0 in
        for i = 0 to noOfBytes-1
        do 
            if (Bytes.get buffer i) = '\n'
            then
                begin
                cnt := !cnt + 1
                end
        done;
        Mutex.lock m;
        count := !count + !cnt;
        Mutex.unlock m
        
  );
  !count


let main =
        let start = Unix.gettimeofday () in
    let filename = Sys.argv.(1) in
    let pool = Domainslib.Task.setup_pool ~num_additional_domains:(num_domains - 1) () in
    let count = parallel_line_count pool filename in
    let stop = Unix.gettimeofday () in
    Domainslib.Task.teardown_pool pool;
    Printf.printf "Done %d \nTime %f\n" count ( stop -. start)
