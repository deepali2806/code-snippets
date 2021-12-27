open Unix;;
let num_domains = try int_of_string Sys.argv.(1) with _ -> 1
let buffer_size = try int_of_string Sys.argv.(2) with _ ->  4096

(* 128 ASCII Characters *)
let lastTable = Array.make 128 (-1)

let min a b = 
  if a < b then a
  else b

let rec prevLineIndex i iBuffer = 
  if (Bytes.get iBuffer i) = '\n' || i = 0
  then
    i
  else
    prevLineIndex (i-1) iBuffer

let rec nextLineIndex i iBuffer = 
  if (Bytes.get iBuffer i) = '\n' || (i = (Bytes.length iBuffer) - 1)
  then
    i
  else
    nextLineIndex (i+1) iBuffer


let rec append i j iBuffer buffer = 
if i = j then  Bytes.cat buffer  (Bytes.make 1 (Bytes.get iBuffer j))
  else
      append (i+1) j iBuffer ( Bytes.cat buffer (Bytes.make 1 (Bytes.get iBuffer i)))


 let preprocess pattern lastTable = 
  for i = 0 to (String.length pattern) - 1
  do 
    lastTable.((Char.code pattern.[i])) <- i
  done


let rec boyerMooreHelper iBuffer pattern lastTable i j =
  let m = (String.length pattern) in 
    let n = (Bytes.length iBuffer) in 
    if i >= n 
    then
      -1
    else  
      (
        if pattern.[j]  = (Bytes.get iBuffer i)
              then
                (
                  if j = 0 
                  then 
                    begin
                      let x = (nextLineIndex i iBuffer) in 
                      let foundString = append (prevLineIndex i iBuffer) x iBuffer (Bytes.empty) in 
                      Format.printf "%s%!" (Bytes.to_string foundString);
                      boyerMooreHelper iBuffer pattern lastTable x (m-1)                    
                    end
                  else(
                    boyerMooreHelper iBuffer pattern lastTable (i-1) (j-1))
                )
        else
            let p = i + m - (min j (1+(lastTable.((Char.code (Bytes.get iBuffer i) )))))
            in boyerMooreHelper iBuffer pattern lastTable p (m - 1)                   
      )

let boyerMoore iBuffer pattern lastTable = 
  let m = (String.length pattern) in boyerMooreHelper iBuffer pattern lastTable (m-1) (m-1)

let rec readTillNewlineFound fd newOffset newByteSequence =
  let singleByte = Bytes.create 1 in
  let _ = lseek fd (newOffset) SEEK_SET in
  let noOfBytes = (read fd singleByte 0 1) in
  if noOfBytes <= 0
  then
      begin 
      newByteSequence
      end
  else
    begin
        if ((Bytes.get singleByte 0) = '\n')
          then  
          begin
          newByteSequence
          end
        else  
          begin 
          readTillNewlineFound fd (newOffset+1) (Bytes.cat newByteSequence singleByte)
          end
    end


let parallel_patternMatch pool pattern filename =
  let fd_in = openfile filename [O_RDONLY] 0 in
  let st = fstat fd_in in 
  let i_n = (st.st_size / buffer_size) in

  Domainslib.Task.parallel_for pool ~start:0 ~finish:(i_n) ~body:(fun i ->
  let offset = i*buffer_size in
  let fd = openfile filename [O_RDONLY] 0 in

  if offset = 0
  then 
    begin
          let buffer = Bytes.create buffer_size in
          let _ = lseek fd (offset) SEEK_SET in
          let noOfBytes = read fd buffer 0 buffer_size in

          if noOfBytes < buffer_size
          then
              begin
                let newbuffer = (Bytes.sub buffer 0 noOfBytes) in
                let _ =  boyerMoore newbuffer pattern lastTable in ()
              end
          else
              begin
                if (Bytes.get buffer (buffer_size-1)) = '\n'
                  then 
                  begin
                          let _ =  boyerMoore buffer pattern lastTable in 
                          ()    
                  end 
                else
                  begin
                      let newOffset = offset + noOfBytes - 1 in
                      let newByteSequence = Bytes.empty in
                      let newByte = readTillNewlineFound fd newOffset newByteSequence in 
                      let _ =  boyerMoore (Bytes.cat buffer newByte) pattern lastTable in 
                      ()    
                  end
              end
    end
  else
    begin
          let buffer = Bytes.create (buffer_size+1) in
          let _ = lseek fd (offset) SEEK_SET in
          let noOfBytes = read fd buffer 0 (buffer_size+1) in
          if (Bytes.get buffer 0) = '\n' 
          then 
              begin 
                    if noOfBytes < (buffer_size+1)
                    then
                        begin
                          let newbuffer = try (Bytes.sub buffer 1 noOfBytes) with _ -> Printf.printf "\nError here1 "; buffer in
                          let _ =  boyerMoore newbuffer pattern lastTable in ()
                        end
                    else
                        begin
                          if (Bytes.get buffer (buffer_size)) = '\n'
                            then 
                            begin
                                    let _ =  boyerMoore (Bytes.sub buffer 1 buffer_size) pattern lastTable in 
                                    ()    
                            end 
                          else
                            begin
                                let buffer = try Bytes.sub buffer 1 buffer_size with  _ -> Printf.printf "\nError here 3"; buffer in 
                                let newOffset = offset + noOfBytes - 1 in
                                let newByteSequence = Bytes.empty in
                                let newByte = readTillNewlineFound fd newOffset newByteSequence in 
                                let _ =  boyerMoore (Bytes.cat buffer newByte) pattern lastTable in 
                                ()    
                            end
                        end
              end
          else    
            begin
                  let index = try Bytes.index buffer '\n' with _ -> 0 in

                  if noOfBytes < (buffer_size+1)
                    then
                        begin
                          let newbuffer = try (Bytes.sub buffer index (noOfBytes-index)) with _ -> Printf.printf "\nError here 4"; buffer in
                          let _ =  boyerMoore newbuffer pattern lastTable in ()
                        end
                    else
                        begin
                          if (Bytes.get buffer (buffer_size)) = '\n'
                            then 
                            begin
                                    let _ =  boyerMoore (Bytes.sub buffer index (buffer_size-index)) pattern lastTable in 
                                    ()    
                            end 
                          else
                            begin
                                let buffer = try Bytes.sub buffer index (buffer_size-index) with _ -> Printf.printf "\nError here 6  "; buffer in 
                                let newOffset = offset + noOfBytes-1 in
                                let newByteSequence = Bytes.empty in
                                let newByte = readTillNewlineFound fd newOffset newByteSequence in 
                                let _ =  boyerMoore (Bytes.cat buffer newByte) pattern lastTable in 
                                ()    
                            end
                        end

            end

          
    end; 
    close fd
  );
  close fd_in

let main =
    let pattern = Sys.argv.(3) in 
    let filename = Sys.argv.(4) in

    let start = Unix.gettimeofday () in
    preprocess pattern lastTable;
    let pool = Domainslib.Task.setup_pool ~num_additional_domains:(num_domains - 1) () in
    let _ = parallel_patternMatch pool pattern filename in
    let stop = Unix.gettimeofday () in
    Domainslib.Task.teardown_pool pool;
    Printf.printf "\nDone \nTime %fs\n" ( stop -. start)
