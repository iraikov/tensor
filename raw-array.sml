(* Primitive operations on "raw" arrays. Based on code by Vesa Karvonen. *)

signature RAW_ARRAY = sig
   type 'a array

   structure Rep : sig
      type 'a t
      val int8 : Int8.int t
      val int32 : Int32.int t
      val real32 : Real32.real t
      val real64 : Real64.real t
   end

   val mmap      : 'a Rep.t -> string -> 'a array
   val length    : 'a array -> int
   val sub       : 'a array * int -> 'a
   val update    : 'a array * int * 'a -> unit
   val unsafeSub : 'a array * int -> 'a
   val unsafeUpdate : 'a array * int * 'a -> unit
end

structure RawArray :> RAW_ARRAY = struct
   datatype 'a array =
      ARPTR of {
                ptr : MLton.Pointer.t,
                len : int,
                sub : MLton.Pointer.t * int -> 'a,
                update : MLton.Pointer.t * int * 'a -> unit
               }

   fun length (ARPTR {len, ...}) = len
   fun unsafeSub (ARPTR {ptr, sub, ...}, i) = sub (ptr, i)
   fun unsafeUpdate (ARPTR {ptr, update, ...}, i, v) = update (ptr, i, v)

   fun sub (xs, i) =
       if i < 0 orelse length xs <= i
       then raise Subscript
       else unsafeSub (xs, i)
   fun update (xs, i, v) =
       if i < 0 orelse length xs <= i
       then raise Subscript
       else unsafeUpdate (xs, i, v)

   structure Rep = struct
      datatype 'a t =
         ARPTR of {bytesPerElem : C_Size.word,
                   sub : MLton.Pointer.t * int -> 'a,
                   update : MLton.Pointer.t * int * 'a -> unit}

      val int8 = ARPTR {bytesPerElem = 0w1, 
                        sub = MLton.Pointer.getInt8,
                        update = MLton.Pointer.setInt8}
      val int32 = ARPTR {bytesPerElem = 0w4, 
                         sub = MLton.Pointer.getInt32,
                         update = MLton.Pointer.setInt32}
      val real32 = ARPTR {bytesPerElem = 0w4, 
                         sub = MLton.Pointer.getReal32,
                         update = MLton.Pointer.setReal32}
      val real64 = ARPTR {bytesPerElem = 0w8, 
                         sub = MLton.Pointer.getReal64,
                         update = MLton.Pointer.setReal64}
   end

   fun mmap (Rep.ARPTR {bytesPerElem, sub, update, ...}) s = let
      val bytes = ref 0w0
      val ptr = (_import "mmap_file" :
                 string * C_Size.word ref -> MLton.Pointer.t ;)
                 (s^"\000", bytes)
   in
      if MLton.Pointer.null = ptr then raise Fail "mmap_file" else ();
      ARPTR {len = C_Size.toInt (C_Size.div (!bytes, bytesPerElem)),
             ptr = ptr,
             sub = sub,
             update = update}
   end
end

(* Extended signature for raw arrays with non-primitive operations. *)
signature RAW_ARRAY = 
sig
   include RAW_ARRAY
   val foldl : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
   val app : ('a -> unit) -> 'a array -> unit
end

(* Extended implementation of raw vectors with non-primitive operations. *)
structure RawArray : RAW_ARRAY = 
struct
   open RawArray
   fun foldl f s xs = let
      fun recur i s =
          if i < length xs
          then recur (i+1) (f (unsafeSub (xs, i), s))
          else s
   in
      recur 0 s
   end
   fun app ef = foldl (fn (x, ()) => ef x) ()
end
