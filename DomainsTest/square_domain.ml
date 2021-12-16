effect Async : (unit -> 'a) -> unit
let async f = perform (Async f)

let square n = n * n

let x = 5
let y = 10

let suffix_char s c = s ^ String.make 1 c


let rec append i j stringBuffer str = 
if i = j then  str ^ (String.make 1 stringBuffer.[j])
  else begin 
        Printf.printf "i = %d, j = %d a\n" i j;

      append (i+1) j stringBuffer (str ^ (String.make 1 stringBuffer.[i]))
      end


let _ =
  let d = Domain.spawn (fun _ -> square x) in
  let sy = square y in
  let sx = Domain.join d in
  let str = "" in
  let stringBuffer = "Hello world" in
  Printf.printf "x = %d, y = %d a\n" sx sy;
  Printf.printf "After appending lets see %s \n" (append 1 4 stringBuffer str)

