open Eio.Std
open Eio_linux
module Int63 = Optint.Int63

let buffer_size = try int_of_string Sys.argv.(1) with _ -> 4096


(* 128 ASCII Characters *)
let lastTable = Array.make 128 (-1)

let min a b = 
  if a < b then a
  else b

let rec prevLineIndex i iBuffer = 
  if (Cstruct.get_char iBuffer i) = '\n' || i = 0
  then
    i
  else
    prevLineIndex (i-1) iBuffer

let rec nextLineIndex i iBuffer = 
  if (Cstruct.get_char iBuffer i) = '\n' || (i = (Cstruct.length iBuffer) - 1)
  then
    i
  else
    nextLineIndex (i+1) iBuffer


let rec append i j iBuffer buffer = 
if i = j then  Cstruct.append buffer (Cstruct.of_string ~off:0 ~len:1 (String.make 1 (Cstruct.get_char iBuffer j)))
  else
      append (i+1) j iBuffer ( Cstruct.append buffer (Cstruct.of_string ~off:0 ~len:1 (String.make 1 (Cstruct.get_char iBuffer i)))  )


 let preprocess pattern lastTable = 
  for i = 0 to (String.length pattern) - 1
  do 
    lastTable.((Char.code pattern.[i])) <- i
  done


let rec boyerMooreHelper iBuffer pattern lastTable i j =
  let m = (String.length pattern) in 
    let n = (Cstruct.length iBuffer) in 
    if i >= n 
    then
      -1
    else  
      (
        if pattern.[j]  = (Cstruct.get_char iBuffer i)
              then
                (
                  if j = 0 
                  then 
                    begin
                      let x = (nextLineIndex i iBuffer) in 
                      let foundString = append (prevLineIndex i iBuffer) x iBuffer (Cstruct.empty) in 
                      Format.printf "%s%!" (Cstruct.to_string foundString);
                      boyerMooreHelper iBuffer pattern lastTable (x+1) (m-1)                    
                    end
                  else(
                    boyerMooreHelper iBuffer pattern lastTable (i-1) (j-1))
                )
        else
            let p = i + m - (min j (1+(lastTable.((Char.code (Cstruct.get_char iBuffer i) )))))
            in boyerMooreHelper iBuffer pattern lastTable p (m - 1)                   
      )

let boyerMoore iBuffer pattern lastTable = 
  let m = (String.length pattern) in boyerMooreHelper iBuffer pattern lastTable (m-1) (m-1)

let rec readTillNewlineFound fd newOffset newByteSequence buff =
  (* let single = alloc () in *)
  let noOfBytes = read_upto fd ~file_offset:(Int63.of_int newOffset) buff 1 in
  let singleByte = (Uring.Region.to_cstruct ~len:1 buff) in
      (* free single; *)

  if noOfBytes <= 0
  then
      begin 
      newByteSequence
      end
  else
    begin
        if ((Cstruct.get_char singleByte 0) = '\n')
          then  
          begin
          (* free single; *)
          newByteSequence
          end
        else  
          begin 
          let x = (Cstruct.append newByteSequence singleByte) in
          (* free single; *)
          readTillNewlineFound fd (newOffset+1) x buff
          end
    end

let () = 
    run @@ fun _env ->
    Switch.run @@ fun sw ->
    let pattern = Sys.argv.(3) in 
    let filename = Sys.argv.(2) in
    let fd = Unix.handle_unix_error (openfile ~sw filename Unix.[O_RDONLY]) 0 in 
    let st = fstat fd in
    let no_fibres = (st.st_size / buffer_size) in
    let clock = Eio.Stdenv.clock _env in
    let t1 = Eio.Time.now clock in
    preprocess pattern lastTable;

    (* traceln "The time is now %f" t1; *)
    Switch.run (fun sw ->
        for i = 0 to no_fibres
        do
             Fibre.fork ~sw
            (fun () ->  let buf = alloc () in
                        let offset = i*buffer_size in
                        (* let _ = Unix.lseek (Eio_linux.FD.to_unix fd) off SEEK_SET in *)
                        

                        if offset = 0
                        then 
                            begin
                                let noOfBytes = read_upto fd ~file_offset:(Int63.of_int offset) buf buffer_size in 
                                let buffer = Uring.Region.to_cstruct ~len:noOfBytes buf in
                                if noOfBytes < buffer_size
                                then
                                    begin
                                        let newbuffer = Cstruct.sub buffer 0 noOfBytes in
                                        let _ =  boyerMoore newbuffer pattern lastTable in ()
                                    end
                                else
                                    begin
                                        if (Cstruct.get_char buffer (buffer_size-1)) = '\n'
                                        then 
                                        begin
                                                let _ =  boyerMoore buffer pattern lastTable in 
                                                ()    
                                        end 
                                        else
                                        begin
                                            let newOffset = offset + noOfBytes - 1 in
                                            let newByteSequence = Cstruct.empty in
                                            let newByte = readTillNewlineFound fd newOffset newByteSequence buf in 
                                            let _ =  boyerMoore (Cstruct.append buffer newByte) pattern lastTable in 
                                            ()    
                                        end
                                    end
                            end
                        else    
                            begin
                            let noOfBytes = read_upto fd ~file_offset:(Int63.of_int offset) buf (buffer_size+1) in 
                            let buffer = Uring.Region.to_cstruct ~len:noOfBytes buf in
                            if (Cstruct.get_char buffer 0) = '\n' 
                            then 
                                begin 
                                        if noOfBytes < (buffer_size+1)
                                        then
                                            begin
                                            let newbuffer = try (Cstruct.sub buffer 1 noOfBytes) with _ -> Printf.printf "\nError here1 "; buffer in
                                            let _ =  boyerMoore newbuffer pattern lastTable in ()
                                            end
                                        else
                                            begin
                                            if (Cstruct.get_char buffer (buffer_size)) = '\n'
                                                then 
                                                begin
                                                        let _ =  boyerMoore (Cstruct.sub buffer 1 buffer_size) pattern lastTable in 
                                                        ()    
                                                end 
                                            else
                                                begin
                                                    let buffer = try Cstruct.sub buffer 1 buffer_size with  _ -> Printf.printf "\nError here 3"; buffer in 
                                                    let newOffset = offset + noOfBytes - 1 in
                                                    let newByteSequence = Cstruct.empty in
                                                    let newByte = readTillNewlineFound fd newOffset newByteSequence buf in 
                                                    let _ =  boyerMoore (Cstruct.append buffer newByte) pattern lastTable in 
                                                        (* Printf.printf "\nNew Buffer3 %s" (Cstruct.to_string (Cstruct.append buffer newByte) ) *)                                                   
                                                    ()    
                                                end
                                            end
                                end
                            else    
                                begin
                                    let index = try Bytes.index (Cstruct.to_bytes buffer) '\n' with _ -> 0 in

                                    if noOfBytes < (buffer_size+1)
                                        then
                                            begin
                                            let newbuffer = try (Cstruct.sub buffer index (noOfBytes-index)) with _ -> Printf.printf "\nError here 4"; buffer in
                                            let _ =  boyerMoore newbuffer pattern lastTable in 
                                            (* Printf.printf "\nNew Buffer4 %s" (Cstruct.to_string newbuffer ) *)                  
                                            ()
                                            end
                                        else
                                            begin
                                            if (Cstruct.get_char buffer (buffer_size)) = '\n'
                                                then 
                                                begin
                                                        let _ =  boyerMoore (Cstruct.sub buffer index (buffer_size-index)) pattern lastTable in 
                                                        (* Printf.printf "\nNew Buffer5 %s" (Cstruct.to_string (Cstruct.sub buffer index (buffer_size-index)) ) *)

                                                        ()    
                                                end 
                                            else
                                                begin
                                                    let buffer = try Cstruct.sub buffer index (buffer_size-index) with _ -> Printf.printf "\nError here 6  "; buffer in 
                                                    let newOffset = offset + noOfBytes-1 in
                                                    let newByteSequence = Cstruct.empty in
                                                    let newByte = readTillNewlineFound fd newOffset newByteSequence buf in 
                                                    let _ =  boyerMoore (Cstruct.append buffer newByte) pattern lastTable in 
                                                    (* Printf.printf "\nNew Buffer6 %s" (Cstruct.to_string (Cstruct.append buffer newByte)) *)
                                                    ()    
                                                end
                                            end

                                end

                            
                        end;
                                    free buf

                        (* traceln "\nCount in each fibre %d currentvalue %d " cnt currentVal; *)
                        (* traceln "Offset and number of fibre %d, %d" off i; *)
            );

        done	
    );
    let t2 = Eio.Time.now clock in
    traceln "\nDone \nTime Difference %f"  (t2-.t1);
    traceln "\nSwitch is finished"
