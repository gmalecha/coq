type ('a,'b) stream =
  ('a -> ('a,'b) stream -> 'b) -> (unit -> 'b) -> 'b

val stream_map : ('a -> 'b) ->
  ('a,'c) stream -> ('b,'c) stream

val stream_append : ('a,'b) stream -> ('a,'b) stream -> ('a,'b) stream

val stream_to_list : ?limit:int -> ('a,'a list) stream -> 'a list

val stream_filter : ('a -> bool) -> ('a,'b) stream -> ('a,'b) stream

val stream_yield : 'a -> ('a,'b) stream

val stream_yield_then : 'a -> ('a,'b) stream -> ('a,'b) stream

val stream_done : ('a,'b) stream

val stream_flat_map : ('a -> ('b,'c) stream) -> ('a,'c) stream -> ('b,'c) stream

val stream_of_list : 'a list -> ('a,'b) stream -> ('a,'b) stream

val stream_of_list_flat : ('a -> ('b,'c) stream -> ('b,'c) stream) ->
  'a list -> ('b,'c) stream -> ('b,'c) stream
