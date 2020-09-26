external now : unit -> (int64[@unboxed])
  = "clock_linux_get_time_bytecode" "clock_linux_get_time_native"
  [@@noalloc]

external read : Unix.file_descr -> De.bigstring -> int -> int -> int
  = "bs_read" [@@noalloc]

external write : Unix.file_descr -> De.bigstring -> int -> int -> int
  = "bs_write" [@@noalloc]

let w = De.make_window ~bits:15
let o = De.bigstring_create De.io_buffer_size
let i = De.bigstring_create De.io_buffer_size
let q = De.Queue.create 16384
let allocate _ = w

let rec fully_write fd buf off len =
  let len' = write fd buf off len in
  if len' < len then fully_write fd buf (off + len') (len - len')

exception Stop

let plot metrics max =
  Fmt.pr "time,in,out,live\n%!" ;
  for i = 0 to max - 1 do
    Fmt.pr "%d," (succ i) ;
    for j = 0 to 2 do
      Fmt.pr "%d" metrics.((i * 3) + j) ;
      if j < 2 then Fmt.pr "," ;
    done ; Fmt.pr "\n%!" ;
  done

let inflate max =
  let open Zl in
  let decoder = Inf.decoder `Manual ~o ~allocate in
  let metrics = Array.make (max * 3) 0 (* in bytes, out bytes, live words *) in 
  let rec go idx ts decoder =
    let idx, ts =
      if Int64.sub (now ()) ts >= 1_000_000_000L
      then 
        ( let { Gc.live_words; _ } = Gc.quick_stat () in
          metrics.((idx * 3) + 2) <- live_words ;
          if succ idx >= max then raise Stop ;
          succ idx, Int64.add 1_000_000_000L ts )
      else idx, ts in
    match Inf.decode decoder with
    | `Await decoder ->
      let len = read Unix.stdin i 0 De.io_buffer_size in
      metrics.((idx * 3) + 0) <- metrics.((idx * 3) + 0) + len ;
      go idx ts (Inf.src decoder i 0 len)
    | `Flush decoder ->
      let len = De.io_buffer_size - Inf.dst_rem decoder in
      fully_write Unix.stderr o 0 len ;
      metrics.((idx * 3) + 1) <- metrics.((idx * 3) + 1) + len ;
      go idx ts (Inf.flush decoder)
    | `Malformed err -> invalid_arg err
    | `End _ ->
      let len = De.io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then fully_write Unix.stderr o 0 len ;
      metrics.((idx * 3) + 1) <- metrics.((idx * 3) + 1) + len in
  ( try go 0 (now ()) decoder with Stop -> () ) ;
  plot metrics max

let () = match Sys.argv with
  | [| _; ts |] -> inflate (int_of_string ts)
  | _ -> inflate 30

