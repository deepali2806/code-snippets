(* effect Async : (unit -> 'a) -> unit
let async f = perform (Async f) *)

let square n = n * n

let x = 5
let y = 10

let suffix_char s c = s ^ String.make 1 c

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
  Printf.printf "\nLength %d " (Bytes.length iBuffer);
  if (Bytes.get iBuffer i) = '\n' || (i = ((Bytes.length iBuffer) - 1))
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
                      Unix.sleep (5);
                      let foundString = append (prevLineIndex i iBuffer) x iBuffer (Bytes.empty) in 
                      Format.printf "\n%s%!" (Bytes.to_string foundString);
                      Printf.printf "\nx %d" x;
                      boyerMooreHelper iBuffer pattern lastTable (x+1) (m-1)                    
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

let _ =
  let pattern = "world" in

    preprocess pattern lastTable;

  let d = Domain.spawn (fun _ -> square x) in
  let sy = square y in
  let sx = Domain.join d in
  let stringBuffer = "Hello world" in
  Printf.printf "x = %d, y = %d a\n" sx sy;
  boyerMoore (Bytes.of_string stringBuffer) pattern lastTable
  (* Printf.printf "After appending lets see %s \n" (append 1 4 stringBuffer str) *)
