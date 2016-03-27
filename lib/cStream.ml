type ('a,'b) stream =
  ('a -> ('a,'b) stream -> 'b) ->
  (unit -> 'b) -> 'b

[@@inline]
let stream_map (type a) (type b) (type c) (f : a -> b)
: (a,c) stream -> (b,c) stream =
  let rec stream_map s =
    fun cons nil ->
      s (fun x xs -> cons (f x) (stream_map xs))
        nil
  in
  stream_map

let stream_append (type a) (type b) (xs : (a,b) stream)
    (ys : (a,b) stream) : (a,b) stream =
  let rec sapp xs =
    fun cons nil ->
      xs (fun x xs -> cons x (sapp xs))
        (fun _ -> ys cons nil)
  in sapp xs

let stream_flat_map (type a) (type b) (type c) (f : a -> (b,c) stream)
: (a,c) stream -> (b,c) stream =
  let rec stream_flat_map (s : (a,c) stream) : (b,c) stream =
    fun cons nil ->
      s (fun x xs -> stream_append (f x) (stream_flat_map xs) cons nil)
        nil
  in
  stream_flat_map

let stream_filter (type a) (type b) (p : a -> bool)
: (a,b) stream -> (a,b) stream =
  let rec filter (s : (a,b) stream) : (a,b) stream =
    fun cons nil ->
      s (fun x xs ->
          if p x
          then cons x (filter xs)
          else filter xs cons nil)
        nil
  in filter

[@@inline]
let stream_yield_then (type a) (type b) (x : a) (k : (a,b) stream)
: (a,b) stream =
  fun cons _ -> cons x k

[@@inline]
let stream_done (type a) (type b) : (a,b) stream =
  fun _ nil -> nil ()

[@@inline]
let stream_yield (type a) (type b) (x : a) : (a,b) stream =
  fun cons _ -> cons x stream_done


let stream_to_list =
  let rec all_to_list str =
    str
      (fun x xs -> x :: all_to_list xs)
      (fun _ -> [])
  in
  let rec some_to_list n str =
    if n > 0 then
      str
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

let stream_of_list (type a) (type b) (ls : a list) (str : (a,b) stream)
: (a,b) stream =
  let rec consume (ls : a list) =
    fun cons nil ->
      match ls with
        [] -> str cons nil
      | l :: ls -> cons l (consume ls)
  in consume ls

let stream_of_list_flat (type a) (type b) (type c)
    (f : a -> (b,c) stream -> (b,c) stream)
    (ls : a list) (str : (b,c) stream) : (b,c) stream =
  let rec consume (ls : a list) : (b,c) stream =
    fun cons nil ->
      match ls with
        [] -> str cons nil
      | l :: ls -> f l (consume ls) cons nil
  in consume ls
