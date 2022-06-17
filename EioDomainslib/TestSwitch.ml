(* open Eio_linux *)
open Eio.Std


module S = Eio.Stream

let add t v =
  traceln "Adding %d to stream" v;
  S.add t v;
  traceln "Added %d to stream" v

let take t =
  traceln "Reading from stream";
  traceln "Got %d from stream" (S.take t)

(* let main = Eio_linux.run @@ fun _env ->
  Switch.run (fun sw ->
    let t = S.create 1 in
    Fiber.fork ~sw
      (fun () -> take t);
    traceln "First thread forked";
    add t 3;
  );
  traceln "Switch is finished";; *)


(* let main = Eio_linux.run @@ fun _env ->
  Switch.run (fun sw ->
    let t = S.create 1 in
    take t;

    Fiber.fork ~sw
      (fun () -> add t 3);
    traceln "First thread forked";
  );
  traceln "Switch is finished";; *)

  let main = Eio_linux.run @@ fun _env ->
  Switch.run (fun sw ->
    let t = S.create 1 in
    Fiber.fork ~sw
      (fun () -> 
      Switch.run (fun _ -> take t) 
      );
      add t 3;
    traceln "First thread forked";
  );
  traceln "Switch is finished";;