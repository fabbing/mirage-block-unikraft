type t = {
  mutex : Lwt_mutex.t;
  mutable count : int;
  nonzero : unit Lwt_condition.t;
}

let make count =
  if count < 0 then raise (Invalid_argument "Invalid initial count");
  { mutex = Lwt_mutex.create (); count; nonzero = Lwt_condition.create () }
  
let acquire t =
  Lwt_mutex.with_lock t.mutex Lwt.(fun () ->
      let rec aux () =
        if t.count = 0 then Lwt_condition.wait ~mutex:t.mutex t.nonzero >>= aux
        else Lwt.return_unit
      in
      aux () >>= fun () ->
      t.count <- t.count - 1;
      Lwt.return_unit)

let release t =
  Lwt_mutex.with_lock t.mutex (fun () ->
      t.count <- t.count + 1;
      Lwt_condition.signal t.nonzero ();
      Lwt.return_unit)
