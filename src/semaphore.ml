
type t = {
  mutex : Lwt_mutex.t;
  mutable count : int;
  nonzero : unit Lwt_condition.t
}

let make count =
  if count < 0 then raise (Invalid_argument "Invalid initial count");
  { mutex = Lwt_mutex.create (); count; nonzero = Lwt_condition.create () }
  
let acquire t =
  Lwt_mutex.with_lock t.mutex (fun () ->
    while t.count = 0 do
      Lwt_condition.wait ~mutex:t.mutex t.nonzero
    done;
    t.count <- t.count - 1;
    Lwt.return_unit)

let release t =
  Lwt_mutex.with_lock t.mutex (fun () ->
    t.count <- t.count + 1;
    Lwt_condition.signal t.nonzero ();
    Lwt.return_unit)
