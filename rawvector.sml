(* Signature for primitive operations on "raw" vectors. *)
signature RAW_VECTOR = sig
   type 'a t

   structure Rep : sig
      type 'a t
      val int32 : Int32.t t
   end

   val mmap : 'a Rep.t -> String.t -> 'a t
   val length : 'a t -> Int.t
   val sub : 'a t * Int.t -> 'a
   val unsafeSub : 'a t * Int.t -> 'a
end

(* Implementation of primitive operations on raw vectors. *)
structure RawVector :> RAW_VECTOR = struct
   datatype 'a t =
      IN of {ptr : MLton.Pointer.t,
             len : Int.t,
             sub : MLton.Pointer.t * Int.t -> 'a}

   fun length (IN {len, ...}) = len
   fun unsafeSub (IN {ptr, sub, ...}, i) = sub (ptr, i)
   fun sub (xs, i) =
       if i < 0 orelse length xs <= i
       then raise Subscript
       else unsafeSub (xs, i)

   structure Rep = struct
      datatype 'a t =
         IN of {bytesPerElem : C_Size.word,
                sub : MLton.Pointer.t * Int.t -> 'a}
      val int32 = IN {bytesPerElem = 0w4, sub = MLton.Pointer.getInt32}
   end

   fun mmap (Rep.IN {bytesPerElem, sub, ...}) s = let
      val bytes = ref 0w0
      val ptr = (_import "map_file" :
                 String.t * C_Size.word Ref.t -> MLton.Pointer.t ;)
                 (s^"\000", bytes)
   in
      if MLton.Pointer.null = ptr then fail "mmap" else ()
    ; IN {len = C_Size.toInt (C_Size.div (!bytes, bytesPerElem)),
          ptr = ptr,
          sub = sub}
   end
end

(* Extended signature for raw vectors with non-primitive operations. *)
signature RAW_VECTOR = sig
   include RAW_VECTOR
   val foldl : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
   val app : 'a Effect.t -> 'a t Effect.t
   val for : 'a t -> 'a Effect.t Effect.t
end

(* Extended implementation of raw vectors with non-primitive operations. *)
structure RawVector : RAW_VECTOR = struct
   open RawVector
   fun foldl f s xs = let
      fun lp i s =
          if i < length xs
          then lp (i+1) (f (unsafeSub (xs, i), s))
          else s
   in
      lp 0 s
   end
   fun app ef = foldl (fn (x, ()) => ef x) ()
   fun for xs ef = app ef xs
end
