external now : unit -> (int64[@unboxed])
  = "clock_linux_get_time_bytecode" "clock_linux_get_time_native"
  [@@noalloc]

external read : Unix.file_descr -> De.bigstring -> int -> int -> int = "bs_read"
  [@@noalloc]

external write : Unix.file_descr -> De.bigstring -> int -> int -> int
  = "bs_write"
  [@@noalloc]

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
  Fmt.pr "time,in,out,live\n%!"
  ; for i = 0 to max - 1 do
      Fmt.pr "%d," (succ i)
      ; for j = 0 to 2 do
          Fmt.pr "%d" metrics.((i * 3) + j)
          ; if j < 2 then Fmt.pr ","
        done
      ; Fmt.pr "\n%!"
    done

type result = {
    time: int
  ; in_bytes_per_sec: int
  ; out_bytes_per_sec: int
  ; live_heap_bytes: int
}
[@@deriving to_yojson]

type results = {metrics: result list} [@@deriving to_yojson]

let transform_arr_to_results metrics max =
  let indices = List.init max (fun x -> x) in
  let metrics =
    List.map
      (fun index ->
        let time = index + 1 in
        let in_bytes_per_sec = metrics.((index * 3) + 0) in
        let out_bytes_per_sec = metrics.((index * 3) + 1) in
        let live_heap_bytes = metrics.((index * 3) + 2) in
        {time; in_bytes_per_sec; out_bytes_per_sec; live_heap_bytes})
      indices in
  {metrics}

let plot_json metrics max =
  let metrics = transform_arr_to_results metrics max in
  let fmt = stdout |> Format.formatter_of_out_channel in
  let open Yojson.Safe in
  let obj = `Assoc ["results", results_to_yojson metrics] in
  pretty_print fmt obj

let inflate max flag =
  let open Zl in
  let decoder = Inf.decoder `Manual ~o ~allocate in
  let metrics = Array.make (max * 3) 0 (* in bytes, out bytes, live words *) in
  let rec go idx ts decoder =
    let idx, ts =
      if Int64.sub (now ()) ts >= 1_000_000_000L then (
        let {Gc.top_heap_words; _} = Gc.quick_stat () in
        metrics.((idx * 3) + 2) <- top_heap_words
        ; if succ idx >= max then raise Stop
        ; succ idx, Int64.add 1_000_000_000L ts)
      else idx, ts in
    match Inf.decode decoder with
    | `Await decoder ->
      let len = read Unix.stdin i 0 De.io_buffer_size in
      metrics.((idx * 3) + 0) <- metrics.((idx * 3) + 0) + len
      ; go idx ts (Inf.src decoder i 0 len)
    | `Flush decoder ->
      let len = De.io_buffer_size - Inf.dst_rem decoder in
      fully_write Unix.stderr o 0 len
      ; metrics.((idx * 3) + 1) <- metrics.((idx * 3) + 1) + len
      ; go idx ts (Inf.flush decoder)
    | `Malformed err -> invalid_arg err
    | `End _ ->
      let len = De.io_buffer_size - Inf.dst_rem decoder in
      if len > 0 then fully_write Unix.stderr o 0 len
      ; metrics.((idx * 3) + 1) <- metrics.((idx * 3) + 1) + len in
  (try go 0 (now ()) decoder with Stop -> ())
  ; match flag with true -> plot_json metrics max | _ -> plot metrics max

open Cmdliner

let max =
  let doc = "Maximum time count in seconds" in
  Arg.(value & opt int 30 & info ["m"; "max"] ~doc)

let json =
  let doc = "Print json output." in
  Arg.(value & flag & info ["j"; "json"] ~doc)

let cmd =
  Cmd.v
    (Cmd.info "bench" ~doc:"Run benchmarks")
    Term.(const inflate $ max $ json)

let () = exit @@ Cmd.eval cmd
