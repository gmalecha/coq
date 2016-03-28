type 'a stream =
{ stream_run : 'b. ('a -> 'a stream -> 'b) -> (unit -> 'b) -> 'b }

val stream_map : ('a -> 'b) -> 'a stream -> 'b stream

val stream_append : 'a stream -> 'a stream -> 'a stream

val stream_to_list : ?limit:int -> 'a stream -> 'a list

val stream_filter : ('a -> bool) -> 'a stream -> 'a stream

val stream_yield : 'a -> 'a stream

val stream_yield_then : 'a -> 'a stream -> 'a stream

val stream_done : 'a stream

val stream_flat_map : ('a -> 'b stream) -> 'a stream -> 'b stream

val stream_of_list : 'a list -> 'a stream -> 'a stream

val stream_of_list_flat : ('a -> 'b stream -> 'b stream) ->
  'a list -> 'b stream -> 'b stream
