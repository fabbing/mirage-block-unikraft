type netdev_ptr = int

external uk_blkdev_init : int -> (netdev_ptr, string) result = "uk_blkdev_init"

open Lwt.Infix

let src = Logs.Src.create "block" ~doc:"Mirage Unikraft block module"
module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  id : int;
  handle : netdev_ptr;
  info : Mirage_block.info;
}

type error =
  [ Mirage_block.error
  | `Invalid_argument
  | `Unspecified_error
  | `Buffer_alignment ]

let pp_error ppf = function
  | #Mirage_block.error as e -> Mirage_block.pp_error ppf e
  | `Invalid_argument -> Fmt.string ppf "Invalid argument"
  | `Unspecified_error -> Fmt.string ppf "Unspecified error"
  | `Buffer_alignment ->
      Fmt.string ppf "Invalid argument: buffers must be sector aligned"

type write_error =
  [ Mirage_block.write_error
  | `Invalid_argument
  | `Unspecified_error
  | `Buffer_alignment ]

let pp_write_error ppf = function
  | #Mirage_block.write_error as e -> Mirage_block.pp_write_error ppf e
  | `Invalid_argument -> Fmt.string ppf "Invalid argument"
  | `Unspecified_error -> Fmt.string ppf "Unspecified error"
  | `Buffer_alignment ->
      Fmt.string ppf "Invalid argument: buffers must be sector aligned"

let connect devid =
  let aux id = 
    match uk_blkdev_init id with
    | Ok handle ->
      let read_write = true in (* FIXME *)
      let sector_size = 8 in
      let size_sectors = 64L in
      let t = { id; handle; info = { read_write; sector_size; size_sectors }} in
      Lwt.return t
    | Error msg -> Lwt.fail_with msg
  in
  match int_of_string_opt devid with
  | Some id when id >= 0 && id < 63 ->
      Log.Info (fun f -> f "Plugging into blkdev %d" id);
      aux id
  | _ -> Lwt.fail_with (Fmt.str "Blkdev: connect(%s): Invalid argument" devid)

let disconnect _t =
  Lwt.return_unit

let get_info t =
  Lwt.return t.info
  
let read _t _sector_start _buffers =
  Lwt.return (Ok ())

let write _t _sector_start _buffers =
  Lwt.return (Ok ())
