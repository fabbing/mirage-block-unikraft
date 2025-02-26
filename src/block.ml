type block_ptr = int

external uk_block_init : int -> (block_ptr, string) result = "uk_block_init"
external uk_block_info : block_ptr -> bool * int * int64 = "uk_block_info"
external uk_token_max : unit -> int = "uk_token_max"

external uk_block_read :
  block_ptr -> int64 -> int -> Cstruct.buffer -> (int, string) result
  = "uk_block_read"

external uk_block_write :
  block_ptr -> int64 -> int -> Cstruct.buffer -> (int, string) result
  = "uk_block_write"

external uk_complete_io : block_ptr -> int -> bool = "uk_complete_io"

open Lwt.Infix
let ( let* ) = Lwt.bind

let src = Logs.Src.create "block" ~doc:"Mirage Unikraft block module"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  id : int;
  handle : block_ptr;
  semaphore : Semaphore.t;
  info : Mirage_block.info
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
    match uk_block_init id with
    | Ok handle ->
        let read_write, sector_size, size_sectors = uk_block_info handle in
        let tokens = uk_token_max () in
        let semaphore = Semaphore.make tokens in
        let t =
          {
            id;
            handle;
            semaphore;
            info = { read_write; sector_size; size_sectors };
          }
        in
        Lwt.return t
    | Error msg -> Lwt.fail_with msg
  in
  match int_of_string_opt devid with
  | Some id when id >= 0 && id < 63 ->
      Log.info (fun f -> f "Plugging into blkdev %d" id);
      aux id
  | _ -> Lwt.fail_with (Fmt.str "Blkdev: connect(%s): Invalid argument" devid)

let disconnect _t = Lwt.return_unit
let get_info t = Lwt.return t.info

let check_bounds t sector_start buffer =
  let buff_size = buffer.Cstruct.len in
  if buff_size mod t.info.sector_size <> 0 then Error `Buffer_alignment
  else
    let end_ =
      Int64.(add sector_start (of_int (buff_size / t.info.sector_size)))
    in
    if end_ > t.info.size_sectors then Error `Invalid_argument else Ok ()

let rec read t sector_start buffers =
  Log.info (fun f -> f "read: on dev #%d at %Ld" t.id sector_start);
  match buffers with
  | [] -> Lwt.return (Ok ())
  | buf :: tl -> (
      match check_bounds t sector_start buf with
      | Error e -> Lwt.return (Error e)
      | Ok () -> (
          let size = buf.Cstruct.len / t.info.sector_size in
          let* () = Semaphore.acquire t.semaphore in
          match uk_block_read t.handle sector_start size buf.Cstruct.buffer with
          | Ok tokid ->
              let* () =
                Unikraft_os.Main.UkEngine.wait_for_work_blkdev t.id tokid
              in
              let ok = uk_complete_io t.handle tokid in
              let* () = Semaphore.release t.semaphore in
              if ok then read t (Int64.add sector_start (Int64.of_int size)) tl
              else Lwt.return (Error `Unspecified_error)
          | Error msg ->
              let* () = Semaphore.release t.semaphore in
              Log.info (fun f -> f "read: %s" msg);
              Lwt.return (Error `Unspecified_error)))

let rec write t sector_start buffers =
  Log.info (fun f -> f "write: on dev #%d at %Ld" t.id sector_start);
  match buffers with
  | [] -> Lwt.return (Ok ())
  | buf :: tl -> (
      match check_bounds t sector_start buf with
      | Error e -> Lwt.return (Error e)
      | Ok () -> (
          let size = buf.Cstruct.len / t.info.sector_size in
          let* () = Semaphore.acquire t.semaphore in
          match
            uk_block_write t.handle sector_start size buf.Cstruct.buffer
          with
          | Ok tokid ->
              let* () =
                Unikraft_os.Main.UkEngine.wait_for_work_blkdev t.id tokid
              in
              let ok = uk_complete_io t.handle tokid in
              let* () = Semaphore.release t.semaphore in
              if ok then write t (Int64.add sector_start (Int64.of_int size)) tl
              else Lwt.return (Error `Unspecified_error)
          | Error msg ->
              let* () = Semaphore.release t.semaphore in
              Log.info (fun f -> f "write: %s" msg);
              Lwt.return (Error `Unspecified_error)))
