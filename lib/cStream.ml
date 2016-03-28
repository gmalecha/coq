type 'a stream =
{ stream_run : 'b. ('a -> 'a stream -> 'b) ->
  (unit -> 'b) -> 'b }

let stream_map (type a) (type b) (f : a -> b)
: a stream -> b stream =
  let rec stream_map s =
    { stream_run = fun cons nil ->
          s.stream_run (fun x xs -> cons (f x) (stream_map xs))
            nil }
  in
  stream_map

let stream_append (type a) (xs : a stream)
    (ys : a stream) : a stream =
  let rec sapp (xs : a stream) =
    { stream_run =
    fun cons nil ->
      xs.stream_run (fun x xs -> cons x (sapp xs))
        (fun _ -> ys.stream_run cons nil) }
  in sapp xs

let stream_flat_map (type a) (type b) (f : a -> b stream)
: a stream -> b stream =
  let rec stream_flat_map (s : a stream) : b stream =
    { stream_run = fun cons nil ->
      s.stream_run
        (fun x xs ->
           let rest = stream_append (f x) (stream_flat_map xs) in
           rest.stream_run cons nil)
        nil }
  in
  stream_flat_map

let stream_filter (type a) (p : a -> bool)
: a stream -> a stream =
  let rec filter (s : a stream) : a stream =
    { stream_run = fun cons nil ->
      s.stream_run (fun x xs ->
          if p x
          then cons x (filter xs)
          else (filter xs).stream_run cons nil)
        nil }
  in filter

[@@inline]
let stream_yield_then (type a) (x : a) (k : a stream)
: a stream =
  { stream_run = fun cons _ -> cons x k }

[@@inline]
let stream_done (type a) : a stream =
  { stream_run = fun _ nil -> nil () }

[@@inline]
let stream_yield (type a) (x : a) : a stream =
  { stream_run = fun cons _ -> cons x stream_done }

let stream_to_list =
  let rec all_to_list str =
    str.stream_run
      (fun x xs -> x :: all_to_list xs)
      (fun _ -> [])
  in
  let rec some_to_list n str =
    if n > 0 then
      str.stream_run
        (fun x xs -> x :: some_to_list (n - 1) xs)
        (fun _ -> [])
    else
      []
  in
  let result_to_list ?limit =
    match limit with
      None -> all_to_list
    | Some n -> some_to_list n
  in result_to_list

let stream_of_list (type a) (ls : a list) (str : a stream)
: a stream =
  let rec consume (ls : a list) =
    { stream_run = fun cons nil ->
      match ls with
        [] -> str.stream_run cons nil
      | l :: ls -> cons l (consume ls) }
  in consume ls

let stream_of_list_flat (type a) (type b)
    (f : a -> b stream -> b stream)
    (ls : a list) (str : b stream) : b stream =
  let rec consume (ls : a list) : b stream =
    { stream_run = fun cons nil ->
      match ls with
        [] -> str.stream_run cons nil
      | l :: ls -> (f l (consume ls)).stream_run cons nil }
  in consume ls
