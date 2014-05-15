
(*
 Copyright (c) Juan Jose Garcia Ripoll and Ivan Raikov.
 All rights reserved.

Redistribution and use in source and binary forms, with or
without modification, are permitted provided that the following
conditions are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following
   disclaimer in the documentation and/or other materials provided
   with the distribution.

3. All advertising materials mentioning features or use of this
   software must display the following acknowledgement:
        This product includes software developed by Juan Jose
        Garcia Ripoll.

4. The name of Juan Jose Garcia Ripoll may not be used to endorse
   or promote products derived from this software without
   specific prior written permission.

*)

(*
structure IntArray =
    struct
        open Array
        type array = int array
        type vector = int vector
        type elem  = int
        structure Vector =
            struct
                open Vector
                type vector = int Vector.vector
                type elem = int
            end
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end
*)


signature MONO_VECTOR =
  sig
    type vector
    type elem
    val maxLen : int
    val fromList : elem list -> vector
    val tabulate : (int * (int -> elem)) -> vector
    val length : vector -> int
    val sub : (vector * int) -> elem
    val extract : (vector * int * int option) -> vector
    val concat : vector list -> vector
    val mapi : ((int * elem) -> elem) -> (vector * int * int option) -> vector
    val map : (elem -> elem) -> vector -> vector
    val appi : ((int * elem) -> unit) -> (vector * int * int option) -> unit
    val app : (elem -> unit) -> vector -> unit
    val foldli : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a
    val foldri : ((int * elem * 'a) -> 'a) -> 'a -> (vector * int * int option) -> 'a
    val foldl : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a
    val foldr : ((elem * 'a) -> 'a) -> 'a -> vector -> 'a 
  end

structure Loop =
    struct
        fun all (a, b, f) =
            if a > b then
                true
            else if f a then
                all (a+1, b, f)
            else
                false

        fun any (a, b, f) =
            if a > b then
                false
            else if f a then
                true
            else
                any (a+1, b, f)

        fun app (a, b, f) =
            if a < b then
                (f a; app (a+1, b, f))
            else
                ()

        fun app2 (a1, b1, a2, b2, f) =
            if ((a1 < b1) andalso (a2 < b2)) then
                (f (a1, a2); app2 (a1+1, b1, a2+1, b2, f))
            else ()

        fun app' (a, b, d, f) =
            if a < b then
                (f a; app' (a+d, b, d, f))
            else
                ()

        fun appi' (a, b, d, f) =
            if a < b then
                (f a; appi' (a+d, b, d, f))
            else
                ()

        fun foldi (a, b, f, init) =
            if a < b then
                foldi (a+1, b, f, f (a, init))
            else
                init
    end
(*
  INDEX         -Signature-

  Indices are a enumerable finite set of data with an order and a map
  to a continous nonnegative interval of integers.  In the sample
  implementation, Index, each index is a list of integers,
        [i1,...,in]
  and each set of indices is defined by a shape, which has the same
  shape of an index but with each integer incremented by one
        shape = [k1,...,kn]
        0 <= i1 < k1

  type storage = RowMajor | ColumnMajor
  order : storage
        Identifies:
                1) the underlying algorithms for this structure
                2) the most significant index
                3) the index that varies more slowly
                4) the total order
        RowMajor means that first index is most significant and varies
        more slowly, while ColumnMajor means that last index is the most
        significant and varies more slowly. For instance
                RowMajor => [0,0]<[0,1]<[1,0]<[1,1] (C, C++, Pascal)
                ColumnMajor => [0,0]>[1,0]>[0,1]>[1,1] (Fortran, Matlab)
  last shape
  first shape
        Returns the last/first index that belongs to the set defined by
        'shape'.
  inBounds shape index
        Checkes whether 'index' belongs to the set defined by 'shape'.
  toInt shape index
        As we said, indices can be sorted and mapped to a finite set of
        integers. 'toInt' obtaines the integer number that corresponds to
        a certain index.
  indexer shape
        It is equivalent to the partial evaluation 'toInt shape' but
        optimized for 'shape'.

  next shape index
  prev shape index
  next' shape index
  prev' shape index
        Obtain the following or previous index to the one we supply.
        next and prev return an object of type 'index option' so that
        if there is no such following/previous, the output is NONE.
        On the other hand, next'/prev' raise an exception when the
        output is not well defined and their output is always of type
        index. next/prev/next'/prev' raise an exception if 'index'
        does not belong to the set of 'shape'.

  all shape f
  any shape f
  app shape f
        Iterates 'f' over every index of the set defined by 'shape'.
        'all' stops when 'f' first returns false, 'any' stops when
        'f' first returns true and 'app' does not stop and discards the
        output of 'f'.

  compare(a,b)
        Returns LESS/GREATER/EQUAL according to the total order which
        is defined in the set of all indices.
  <,>,eq,<=,>=,<>
        Reduced comparisons which are defined in terms of 'compare'.
  +,-
        Index addition and subtraction
  incr,decr
        Index increment and decrement by a constant

  validShape t
  validIndex t
        Checks whether 't' conforms a valid shape or index.

*)

signature INDEX =
    sig
        type t
        type indexer = t -> int
        datatype storage = RowMajor | ColumnMajor

        exception Index
        exception Shape

        val order : storage
        val toInt : t -> t -> int
        val length : t -> int
        val first : t -> t
        val last : t -> t
        val next : t -> t -> t option
        val prev : t -> t -> t option
        val next' : t -> t -> t
        val prev' : t -> t -> t
        val indexer : t -> (t -> int)

        val inBounds : t -> t -> bool
        val compare : t * t -> order
        val < : t * t -> bool
        val > : t * t -> bool
        val eq : t * t -> bool
        val <= : t * t -> bool
        val >= : t * t -> bool
        val <> : t * t -> bool
        val - : t * t -> t
        val + : t * t -> t

        val validShape : t -> bool
        val validIndex : t -> bool

        val all : t -> (t -> bool) -> bool
        val any : t -> (t -> bool) -> bool
        val app : t -> (t -> unit) -> unit
    end

structure Index : INDEX =
    struct
	type t = int list
	type indexer = t -> int
	datatype storage = RowMajor | ColumnMajor

	exception Index
	exception Shape

	val order = ColumnMajor

	fun validShape shape = List.all (fn x => x > 0) shape

	fun validIndex index = List.all (fn x => x >= 0) index

	fun toInt shape index =
	    let fun loop ([], [], accum, p) = accum
		  | loop ([], _, _, _) = raise Index
		  | loop (_, [], _, _) = raise Index
		  | loop (i::ri, l::rl, accum, p) =
		if (i >= 0) andalso (i < l) then
		    loop (ri, rl, i * p + accum, p * l)
		else
		    raise Index
	    in loop (index, shape, 0, 1)
	    end

	(* ----- CACHED LINEAR INDEXER -----

	   An indexer is a function that takes a list of
	   indices, validates it and produces a nonnegative
	   integer number. In short, the indexer is the
	   mapper from indices to element positions in
	   arrays.

	   'indexer' builds such a mapper by optimizing
	   the most common cases, which are 1d and 2d
	   tensors.
	 *)
    local
	fun doindexer [] _ = raise Shape
	  | doindexer [a] [dx] =
	    let fun f [x] = if (x > 0) andalso (x < a)
		            then x
			    else raise Index
		  | f _ = raise Index
	    in f end
	  | doindexer [a,b] [dx, dy] =
		let fun f [x,y] = if ((x >= 0) andalso (x < a) andalso
				      (y >= 0) andalso (y < b))
				  then x + dy * y
				  else raise Index
		      | f _ = raise Index
		in f end
	  | doindexer [a,b,c] [dx,dy,dz] =
		let fun f [x,y,z] = if ((x >= 0) andalso (x < a) andalso
					(y >= 0) andalso (y < b) andalso
					(z >= 0) andalso (z < c))
			            then x + dy * y + dz * z
				    else raise Index
		      | f _ = raise Index
		in f end
	  | doindexer shape memo =
		let fun f [] [] accum [] = accum
		      | f (fact::rf) (ndx::ri) accum (dim::rd) =
			(if (ndx >= 0) andalso (ndx < dim) then
			     f rf ri (accum + ndx * fact) rd
			 else
			     raise Index)
                      | f _ [] _ _  = raise Index
                      | f [] _ _ _  = raise Index
                      | f _ _ _ []  = raise Index

		in f shape memo 0
		end
    in
	fun indexer shape =
	    let fun memoize accum [] = []
		  | memoize accum (dim::rd) =
		accum :: (memoize (dim * accum) rd)
	    in if validShape shape then
		   doindexer shape (memoize 1 shape)
	       else
		   raise Shape
	    end
    end

	fun length shape =
	    let fun prod (a,b) =
		if b < 0 then raise Shape else a * b
	    in foldl prod 1 shape
	    end

	fun first shape = map (fn x => 0) shape

	fun last [] = []
	  | last (size :: rest) = size - 1 :: last rest

	fun next' [] [] = raise Subscript
	  | next' _ [] = raise Index
	  | next' [] _ = raise Index
	  | next' (dimension::restd) (index::resti) =
	    if (index + 1) < dimension then
		(index + 1) :: resti
	    else
		0 :: (next' restd resti)

	fun prev' [] [] = raise Subscript
	  | prev' _ [] = raise Index
	  | prev' [] _ = raise Index
	  | prev' (dimension::restd) (index::resti) =
	    if (index > 0) then
		(index - 1) :: resti
	    else
		(dimension - 1) :: (prev' restd resti)

	fun next shape index = (SOME (next' shape index)) handle
	    Subscript => NONE

	fun prev shape index = (SOME (prev' shape index)) handle
	    Subscript => NONE

	fun inBounds shape index =
	    ListPair.all (fn (x,y) => (x >= 0) andalso (x < y))
	    (index, shape)

	fun compare ([],[]) = EQUAL
	  | compare (_, []) = raise Index
	  | compare ([],_) = raise Index
	  | compare (a::ra, b::rb) =
	    case Int.compare (a,b) of
		EQUAL => compare (ra,rb)
	      | LESS => LESS
	      | GREATER => GREATER

    local
	fun iterator a inner =
	    let fun loop accum f =
		let fun innerloop i =
		    if i < a then
			if inner (i::accum) f then
			    innerloop (i+1)
			else
			    false
		    else
			true
		in innerloop 0
		end
	    in loop
	    end
	fun build_iterator [a] =
	    let fun loop accum f =
		let fun innerloop i =
		    if i < a then
			if f (i::accum) then
			    innerloop (i+1)
			else
			    false
		    else
			true
		in innerloop 0
		end
	    in loop
	    end
	  | build_iterator (a::rest) = iterator a (build_iterator rest)
          | build_iterator [] = raise Shape
    in
	fun all shape = build_iterator shape []
    end

    local
	fun iterator a inner =
	    let fun loop accum f =
		let fun innerloop i =
		    if i < a then
			if inner (i::accum) f then
			    true
			else
			    innerloop (i+1)
		    else
			false
		in innerloop 0
		end
	    in loop
	    end
	fun build_iterator [a] =
	    let fun loop accum f =
		let fun innerloop i =
		    if i < a then
			if f (i::accum) then
			    true
			else
			    innerloop (i+1)
		    else
			false
		in innerloop 0
		end
	    in loop
	    end
	  | build_iterator (a::rest) = iterator a (build_iterator rest)
          | build_iterator [] = raise Shape
    in
	fun any shape = build_iterator shape []
    end

    local
	fun iterator a inner =
	    let fun loop accum f =
		let fun innerloop i =
		        case i < a of
			    true => (inner (i::accum) f; innerloop (i+1))
		          | false => ()
		in innerloop 0
		end
	    in loop
	    end
	fun build_iterator [a] =
	    let fun loop accum f =
		let fun innerloop i =
		        case i < a of
			    true => (f (i::accum); innerloop (i+1))
		          | false => ()
		in innerloop 0
		end
	    in loop
	    end
	  | build_iterator (a::rest) = iterator a (build_iterator rest)
          | build_iterator [] = raise Shape
    in
	fun app shape = build_iterator shape []
    end
    

    fun a < b = compare(a,b) = LESS
    fun a > b = compare(a,b) = GREATER
    fun eq (a, b) = compare(a,b) = EQUAL
    fun a <> b = not (a = b)
    fun a <= b = not (a > b)
    fun a >= b = not (a < b)
    fun a - b = ListPair.map Int.- (a,b)
    fun a + b = ListPair.map Int.+ (a,b)
                
    fun decr n a = List.map (fn (x) => Int.-(x,n)) a
    fun incr n a = List.map (fn (x) => Int.+(x,n)) a
                 
  end


signature RANGE =
    sig
	structure Index : INDEX
	type index = Index.t
	type t

	exception Range

	val fromto : index -> index * index -> t
	val fromto' : index -> index * index -> t
	val upto : index -> index -> t
	val below : index -> index -> t

	val first : t -> index
	val last : t -> index

	val length : t -> int
	val shapes : t -> index list
	val inRange : t -> index -> bool
	val next : t -> index -> index option
	val prev : t -> index -> index option
        val ranges : index -> ((index * index) list) -> t

	val iteri : (index -> bool) -> t -> bool
	val iteri2 : (index * index -> bool) -> (t * t) -> bool

	val iteri_range : (index * index -> bool) -> t -> bool
	val iteri2_range : (((index * index) * (index * index)) -> bool) -> (t * t) -> bool

	val foldi_range : (((index * index) * 'a) -> 'a) -> 'a -> t -> 'a
        
    end

structure Range : RANGE =
struct

    structure Index = Index
    type index = Index.t

    datatype t = RangeEmpty | RangeIn of index * index * index | RangeSet of index * ((index * index) list)

    exception Range

    local
	fun next'' [] [] [] = raise Range
	  | next'' [] _  _  = raise Index.Index
	  | next'' _  [] _  = raise Index.Index
	  | next'' _  _  [] = raise Index.Index
	  | next'' (low::rl) (up::ru) (index::ri) =
	    if index < up then
		index + 1 :: ri
	    else
		low :: (next'' rl ru ri)

	fun prev'' [] [] [] = raise Range
	  | prev'' [] _  _  = raise Index.Index
	  | prev'' _  [] _  = raise Index.Index
	  | prev'' _  _  [] = raise Index.Index
	  | prev'' (low::rl) (up::ru) (index::ri) =
	    if index > low then
		index - 1 :: ri
	    else
		up :: (prev'' rl ru ri)

	(* Builds the simple loop
	   for i := first to last
	      if not (g (i::ndx)) then
		  break
	   endfor;
	   *)

	fun simple_loop (first : int) (last : int) =
	    let fun loop (ndx : index) (g: index -> bool) =
		let fun innerloop i =
		    if i > last then
			true
		    else if g (i::ndx) then
			innerloop (i+1)
		    else
			false
		in innerloop first end
	    in loop end

	(* Builds the nested loop
	   for i := first to last
	      if not (f (i:ndx) g) then
		  break
	   endfor
	 *)
	fun nested_loop f (first : int) (last : int) =
	    let fun loop (ndx: index) (g: index -> bool) =
		let fun innerloop i =
                     (if i > last then
			true
		    else if (f (i::ndx) g) then
			innerloop (i+1)
		    else
			false)
		in 
                     innerloop first 
                end
	    in loop end

	fun build_iterator ([a] : index) ([b] : index) = 
            simple_loop a b
	  | build_iterator (a::ra) (b::rb) =
	    nested_loop (build_iterator ra rb) a b
	  | build_iterator [] _ = raise Range
	  | build_iterator _ [] = raise Range

	fun simple_loop2 (first : int) (last : int) (first' : int) (last' : int) =
	    let fun loop (ndx : index) (ndx' : index) (g: index * index -> bool) =
		let fun innerloop (i,j) =
		    if i > last andalso j > last' then
			true
		    else if g (i::ndx,j::ndx') then
			innerloop (i+1,j+1)
		    else
			false
		in innerloop (first,first') end
	    in loop end

	fun nested_loop2 f (first : int) (last : int) (first' : int) (last' : int) =
	    let fun loop (ndx: index) (ndx': index) (g: index * index -> bool) =
		let fun innerloop (i,j) =
		    (if i > last andalso j > last' then
			true
		    else if (f (i::ndx) (j::ndx') g) then
			innerloop (i+1,j+1)
		    else
			false)
		in 
                     innerloop (first,first')
                end
	    in loop end

	fun build_iterator2 ([a] : index) ([b] : index) ([a'] : index) ([b'] : index) = 
            simple_loop2 a b a' b'
	  | build_iterator2 (a::ra) (b::rb) (a'::ra') (b'::rb') =
	    nested_loop2 (build_iterator2 ra rb ra' rb') a b a' b'
	  | build_iterator2 _ _ _ [] = raise Range
	  | build_iterator2 _ _ [] _ = raise Range
	  | build_iterator2 _ [] _ _ = raise Range
	  | build_iterator2 [] _ _ _ = raise Range


	fun simple_range_loop (first : int) (last : int) (ndx: index)
                              (f: (index * index) -> bool) =
            f ((first::ndx,last::ndx))

	fun nested_range_loop (g: index -> ((index * index) -> bool) -> bool)
                         (first : int) (last : int) =
	    let
                fun loop (ndx: index) (f: (index * index) -> bool) =
		    let
                        fun innerloop i =
                            (if i > last then
			         true
		             else 
                                 if (g (i::ndx) f) 
                                 then innerloop (i+1)
                                 else false)
		    in 
                        innerloop first
                    end
	    in loop end
            
	fun build_range_iterator ([a] : index) ([b] : index) = 
            simple_range_loop a b
	  | build_range_iterator (a::ra) (b::rb) =
	    nested_range_loop (build_range_iterator ra rb) a b
	  | build_range_iterator [] _ = raise Range
	  | build_range_iterator _ [] = raise Range


	fun simple_range_loop2 (first : int) (last : int) (first' : int) (last' : int)
                               (ndx: index) (ndx': index) 
                               (f: ((index * index) * (index * index)) -> bool) =
            f ((first::ndx,last::ndx),(first'::ndx',last'::ndx'))

	fun nested_range_loop2 f (first : int) (last : int) (first' : int) (last' : int) =
	    let fun loop (ndx: index) (ndx': index) (g: ((index * index) * (index * index)) -> bool) =
		let fun innerloop (i,j) =
		    (if i > last andalso j > last' then
			true
		    else if (f (i::ndx) (j::ndx') g) then
			innerloop (i+1,j+1)
		    else
			false)
		in 
                     innerloop (first,first')
                end
	    in loop end


	fun build_range_iterator2 ([a] : index) ([b] : index) ([a'] : index) ([b'] : index) = 
            simple_range_loop2 a b a' b'
	  | build_range_iterator2 (a::ra) (b::rb) (a'::ra') (b'::rb') =
	    nested_range_loop2 (build_range_iterator2 ra rb ra' rb') a b a' b'
	  | build_range_iterator2 _ _ _ [] = raise Range
	  | build_range_iterator2 _ _ [] _ = raise Range
	  | build_range_iterator2 _ [] _ _ = raise Range
	  | build_range_iterator2 [] _ _ _ = raise Range



	fun simple_range_fold (first : int) (last : int) (ndx: index) 
                              (f: ((index * index) * 'a) -> 'a) (init: 'a) =
            f ((first::ndx,last::ndx),init)

	fun nested_range_fold (g: index -> (((index * index) * 'a) -> 'a) -> 'a -> 'a)
                              (first : int) (last : int) =
	    let
                fun loop (ndx: index) (f: ((index * index) * 'a) -> 'a) (init: 'a) =
		    let
                        fun innerloop i init =
                            (if i > last then
			         init
		             else 
			         innerloop (i+1) (g (i::ndx) f init))
		    in 
                        innerloop first init
                    end
	    in loop end
            
	fun build_range_fold ([a] : index) ([b] : index) = 
            simple_range_fold a b
	  | build_range_fold (a::ra) (b::rb) =
	    nested_range_fold (build_range_fold ra rb) a b
	  | build_range_fold [] _ = raise Range
	  | build_range_fold _ [] = raise Range


    in

	(* ----- CONSTRUCTORS ----- *)

	fun fromto shape (lo, up) =
	    if (Index.validShape shape) andalso (Index.validIndex lo) andalso (Index.validIndex up) andalso
               (Index.inBounds shape lo) andalso (Index.inBounds shape up) andalso Index.< (lo,up)
            then
                let
                    val shape' = ListPair.map (fn (x,y) => y-x+1) (lo,up)
                in
		    RangeIn(shape,lo,up)
                end
	    else
		RangeEmpty

	fun fromto' shape (lo, up) = fromto shape (lo, (Index.prev' shape up))

	fun upto shape index = fromto shape (Index.first index, index)		

	fun below shape index = fromto' shape (Index.first index, index)

        fun range_append ((lo,up),((lo',up')::ranges)) = 
            if Index.< (up,lo') then ((lo,up)::(lo',up')::ranges) else
               (if Index.> (up,up') then (lo',up')::(range_append ((lo,up),ranges)) else
                 (if Index.> (lo,lo') then ((lo',up')::ranges) else ((lo,up')::ranges) ))
          | range_append ((lo,up),[]) = [(lo,up)]


        fun ranges' shape ((lo, up) :: rest) = 
	    (if (Index.validShape shape) andalso (Index.validIndex lo) andalso (Index.validIndex up) andalso
               (Index.inBounds shape lo) andalso (Index.inBounds shape up) andalso Index.< (lo,up)
               then range_append ((lo,up), (ranges' shape rest)) else (ranges' shape rest))
            | ranges' shape ([]) = []

        fun ranges shape xs = 
            let val set = ranges' shape xs
            in 
                case set of
                    []        => RangeEmpty
                  | [(lo,up)] => 
                    let
                        val shape' = ListPair.map (fn (x,y) => y-x+1) (lo,up)
                    in
                        RangeIn (shape,lo,up)
                    end
                  | _         => 
                    let
                        val shapes = List.map (fn (lo,up) => (ListPair.map (fn (x,y) => y-x+1) (lo,up))) set
                        val shape' = List.foldl (fn(s,ax) => (ListPair.map (fn (x,y) => x+y) (s, ax))) 
                                                (hd shapes) 
                                                (tl shapes)
                    in
                        RangeSet (shape,set)
                    end
            end

	fun length RangeEmpty = 0
	  | length (RangeIn(shape,lo,up)) =
	    let fun diff (x,y) = (y-x+1) in
		Index.length (ListPair.map diff (lo,up))
	    end
	  | length (RangeSet(shape,set)) =
	    let fun diff (x,y) = (y-x+1) in
		foldl (fn ((lo,up),ax) => (Index.length (ListPair.map diff (lo,up))) + ax)
                      0 set
	    end
 
	fun shapes RangeEmpty = []
	  | shapes (RangeIn(shape,lo,up)) =
	    let fun diff (x,y) = (y-x+1) 
            in
		[ListPair.map diff (lo,up)]
	    end
	  | shapes (RangeSet(shape,set)) =
	    let fun diff (x,y) = (y-x+1) in
		List.map (fn (lo,up) => ListPair.map diff (lo,up)) set
	    end
 

	fun first RangeEmpty = raise Range
	  | first (RangeIn(shape,lo,up)) = lo
	  | first (RangeSet(shape,(lo,up)::_)) = lo
	  | first (RangeSet(shape,[])) = raise Range

	fun last RangeEmpty = raise Range
	  | last (RangeIn(shape,lo,up)) = up
	  | last (RangeSet(shape,set)) = let val (lo,up) = List.last set in up end

	(* ----- PREDICATES & OPERATIONS ----- *)

	fun inRange RangeEmpty _ = false
	  | inRange (RangeIn(shape,lo,up)) ndx =
	    (ListPair.all (op <=) (lo,ndx)) andalso (ListPair.all (op <=) (ndx,up))
	  | inRange (RangeSet(shape,set)) ndx =
	    List.exists (fn ((lo,up)) => (ListPair.all (op <=) (lo,ndx)) andalso (ListPair.all (op <=) (ndx,up))) set

	fun next RangeEmpty _ = NONE
	  | next (RangeIn(shape,lo,up)) index =
	    (SOME (next'' lo up index) handle Range => NONE)
	  | next (RangeSet(shape,set)) index =
            let val m = List.find (fn ((lo,up)) => (Index.<=(lo,index)) andalso (Index.<=(index,up))) set
            in
               case m of NONE => NONE
                       | SOME (lo,up) => (SOME (next'' lo up index) handle Range => NONE)
            end

	fun prev RangeEmpty _ = NONE
	  | prev (RangeIn(shape,lo,up)) index =
	    (SOME (prev'' lo up index) handle Range => NONE)
	  | prev (RangeSet(shape,set)) index =
            let val m = List.find (fn ((lo,up)) => (Index.<=(lo,index)) andalso (Index.<=(index,up))) set
            in
               case m of NONE => NONE
                       | SOME (lo,up) => (SOME (prev'' lo up index) handle Range => NONE)
            end

	fun next' RangeEmpty _ = raise Range
	  | next' (RangeIn(shape,lo,up)) index = next'' lo up index
	  | next' (RangeSet(shape,set)) index =
            let val m = List.find (fn ((lo,up)) => (Index.<=(lo,index)) andalso (Index.<=(index,up))) set
                val (lo,up) = valOf m
            in
                next'' lo up index 
            end

	fun prev' RangeEmpty _ = raise Range
	  | prev' (RangeIn(shape,lo,up)) index = prev'' lo up index
	  | prev' (RangeSet(shape,set)) index =
            let val m = List.find (fn ((lo,up)) => (Index.<=(lo,index)) andalso (Index.<=(index,up))) set
                val (lo,up) = valOf m
            in
                prev'' lo up index 
            end

	(* ----- ITERATION ----- *)

	(* Builds an iterator that applies 'f' sequentially to
	   all the indices in the range, *)
	fun iteri f RangeEmpty = f []
	  | iteri (f: index -> bool) (RangeIn(shape,lo: index,up: index)) = 
           (case Index.order of
                Index.RowMajor => ((build_iterator lo up) [] f)
              | Index.ColumnMajor => ((build_iterator (List.rev lo) (List.rev up)) [] f))
	  | iteri (f: index -> bool) (RangeSet(shape,set)) = 
           (case Index.order of
                Index.RowMajor => (List.all (fn (lo,up) => ((build_iterator lo up) [] f)) set)
              | Index.ColumnMajor => (List.all (fn (lo,up) => ((build_iterator (List.rev lo) (List.rev up)) [] f)) set))

	(* Builds an interator that applies 'f' sequentially to
	   all the indices of the two ranges, *)
	fun iteri2 f (RangeEmpty,RangeEmpty) = f ([],[])
	  | iteri2 (f: index * index -> bool) (RangeIn(shape,lo: index,up: index),RangeIn(shape',lo': index,up': index)) = 
            (case Index.order of
                 Index.RowMajor => ((build_iterator2 lo up lo' up') [] [] f )
               | Index.ColumnMajor => ((build_iterator2 (List.rev lo) (List.rev up) (List.rev lo') (List.rev up')) [] [] f ))
	  | iteri2 (f: index * index -> bool) (RangeSet(shape,set),RangeSet(shape',set')) = 
            (case Index.order of
                 Index.RowMajor => (ListPair.all (fn ((lo,up),(lo',up')) => 
                                                     (build_iterator2 lo up lo' up') [] [] f)
                                                 (set,set') )
               | Index.ColumnMajor => (ListPair.all (fn ((lo,up),(lo',up')) => 
                                                        (build_iterator2 (List.rev lo) (List.rev up) (List.rev lo') (List.rev up')) [] [] f) 
                                                    (set,set') ))
	  | iteri2 f (_,_) = raise Range


	(* Builds an iterator that applies 'f' sequentially to
	   all the ranges of contiguous indices (i,j) *)
	fun iteri_range f RangeEmpty = f ([],[])

	  | iteri_range (f: index * index -> bool) (RangeIn(shape,lo: index,up: index)) = 
           (case Index.order of
                Index.RowMajor => ((build_range_iterator lo up) [] f)
              | Index.ColumnMajor => ((build_range_iterator (List.rev lo) (List.rev up)) [] f))

	  | iteri_range (f: index * index -> bool) (RangeSet(shape,set)) = 
           (case Index.order of
                Index.RowMajor => (List.all (fn (lo,up) => ((build_range_iterator lo up) [] f)) set)
              | Index.ColumnMajor => (List.all (fn (lo,up) => ((build_range_iterator (List.rev lo) (List.rev up)) [] f)) set))

	(* Builds an interator that applies 'f' sequentially to
	   all the contiguous indices of the two ranges, *)
	fun iteri2_range f (RangeEmpty,RangeEmpty) = f (([],[]),([],[]))
	  | iteri2_range (f: ((index * index) * (index * index)) -> bool) 
                         (RangeIn(shape,lo: index,up: index),RangeIn(shape',lo': index,up': index)) = 
            (case Index.order of
                 Index.RowMajor => ((build_range_iterator2 lo up lo' up') [] [] f )
               | Index.ColumnMajor => ((build_range_iterator2 (List.rev lo) (List.rev up) (List.rev lo') (List.rev up')) [] [] f ))
	  | iteri2_range (f: ((index * index) * (index * index)) -> bool) 
                   (RangeSet(shape,set),RangeSet(shape',set')) = 
            (case Index.order of
                 Index.RowMajor => (ListPair.all (fn ((lo,up),(lo',up')) => 
                                                     (build_range_iterator2 lo up lo' up') [] [] f)
                                                 (set,set') )
               | Index.ColumnMajor => (ListPair.all (fn ((lo,up),(lo',up')) => 
                                                        (build_range_iterator2 (List.rev lo) (List.rev up) (List.rev lo') (List.rev up')) [] [] f) 
                                                    (set,set') ))
	  | iteri2_range f (_,_) = raise Range


	(* Builds an iterator that applies 'f' sequentially to
	   all the ranges of contiguous indices (i,j) *)
	fun foldi_range f init RangeEmpty = init

	  | foldi_range (f: ((index * index) * 'a -> 'a)) init (RangeIn(shape,lo: index,up: index)) = 
            (case Index.order of
                 Index.RowMajor => ((build_range_fold lo up) [] f init)
               | Index.ColumnMajor => ((build_range_fold (List.rev lo) (List.rev up)) [] f init))

	  | foldi_range (f: ((index * index) * 'a -> 'a)) init (RangeSet(shape,set)) = 
            (case Index.order of
                 Index.RowMajor => (List.foldl (fn ((lo,up),init) => ((build_range_fold lo up) [] f init)) init set)
               | Index.ColumnMajor => (List.foldl (fn ((lo,up),init) => ((build_range_fold (List.rev lo) (List.rev up)) [] f init)) init set))

            

    end
end

(*
 TENSOR         - Signature -

 Polymorphic tensors of any type. With 'tensor' we denote a (mutable)
 array of any rank, with as many indices as one wishes, and that may
 be traversed (map, fold, etc) according to any of those indices.

 type 'a tensor
        Polymorphic tensor whose elements are all of type 'a.
 val storage = RowMajor | ColumnMajor
        RowMajor = data is stored in consecutive cells, last index
        varying fastest (C,C++,Pascal)
        ColumnMajor = data is stored in consecutive cells, first
        index varying fastest (Fortran,Matlab)
 new ([i1,...,in],init)
        Build a new tensor with n indices, each of sizes i1...in,
        filled with 'init'.
 fromArray (shape,data)
 fromList (shape,data)
        Use 'data' to fill a tensor of that shape. An exception is
        raised if 'data' is too large or too small to properly
        fill the vector. Later use of a 'data' array is disregarded
        -- one must think that the tensor now owns the array.
 length tensor
 rank tensor
 shape tensor
        Return the number of elements, the number of indices and
        the shape (size of each index) of the tensor.
 toArray tensor
        Return the data of the tensor in the form of an array.
        Mutation of this array may lead to unexpected behavior.

 sub (tensor,[i1,...,in])
 update (tensor,[i1,...,in],new_value)
        Access the element that is indexed by the numbers [i1,..,in]

 app f a
 appi f a
        The same as 'map' and 'mapi' but the function 'f' outputs
        nothing and no new array is produced, i.e. one only seeks
        the side effect that 'f' may produce.
 map2 operation a b
        Apply function 'f' to pairs of elements of 'a' and 'b'
        and build a new tensor with the output. Both operands
        must have the same shape or an exception is raised.
        The procedure is sequential, as specified by 'storage'.
 foldl operation a n
        Fold-left the elements of tensor 'a' along the n-th
        index.
 all test a
 any test a
        Folded boolean tests on the elements of the tensor.

 insert a b index
        Inserts b into a starting at the given index
        a and b must be of the same rank, with b smaller than a

 cat a b int
        Concatenates a and b along the given axis

*)

signature TENSOR =
    sig
        structure Array : ARRAY
        structure Index : INDEX
        type index = Index.t
        type 'a tensor

        val new : index * 'a -> 'a tensor
        val tabulate : index * (index -> 'a) -> 'a tensor
        val length : 'a tensor -> int
        val rank : 'a tensor -> int
        val shape : 'a tensor -> (index)
        val reshape : index -> 'a tensor -> 'a tensor
        val fromList : index * 'a list -> 'a tensor
        val fromArray : index * 'a array -> 'a tensor
        val toArray : 'a tensor -> 'a array

        val sub : 'a tensor * index -> 'a
        val update : 'a tensor * index * 'a -> unit
        val map : ('a -> 'b) -> 'a tensor -> 'b tensor
        val map2 : ('a * 'b -> 'c) -> 'a tensor -> 'b tensor -> 'c tensor
        val app : ('a -> unit) -> 'a tensor -> unit
        val appi : (index * 'a -> unit) -> 'a tensor -> unit
        val foldl : ('a * 'b -> 'b) -> 'b -> 'a tensor -> 'b
        val foldln : ('c * 'a -> 'c) -> 'c -> 'a tensor -> int -> 'c tensor
        val all : ('a -> bool) -> 'a tensor -> bool
        val any : ('a -> bool) -> 'a tensor -> bool

        val insert :  'a tensor * 'a tensor * index -> bool        
        val cat :  'a tensor * 'a tensor * int -> 'a tensor        
        val prepad : 'a tensor * int * 'a * int -> 'a tensor        
        val postpad : 'a tensor * int * 'a * int -> 'a tensor        
    end


signature TENSOR_SLICE =
    sig
        structure Tensor : TENSOR
        structure Range : RANGE

        type index = Tensor.Index.t
        type range = Range.t
        type 'a tensor = 'a Tensor.tensor
        type 'a slice

        val fromto : index * index * 'a tensor -> 'a slice
        val slice  : ((index * index) list) * 'a tensor -> 'a slice

        val length : 'a slice -> int
        val base   : 'a slice -> 'a tensor
        val shapes : 'a slice -> index list
        val range  : 'a slice -> (range)

        val app : ('a -> unit) -> 'a slice -> unit
        val map : ('a -> 'b) -> 'a slice -> 'b tensor
        val map2 : ('a * 'b -> 'c) -> 'a slice -> 'b slice -> 'c tensor
        val foldl  : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b

    end

structure Tensor : TENSOR =
    struct
        structure Array = Array
        structure Index = Index
            
        type index = Index.t
        type 'a tensor = {shape : index, indexer : Index.indexer, data : 'a array}

        exception Shape
        exception Match
        exception Index

    local
    (*----- LOCALS -----*)

        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}

        fun toInt {shape, indexer, data} index = indexer index

        fun array_map f a =
            let fun apply index = f(Array.sub(a,index)) in
                Array.tabulate(Array.length a, apply)
            end

        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end
            | splitList ([], _) = ([],0,[])

    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)

        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end

        fun toArray {shape, indexer, data} = data

        fun length {shape, indexer, data} =  Array.length data

        fun shape {shape, indexer, data} = shape

        fun rank t = List.length (shape t)

        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape

        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape

        fun fromList (s, a) = fromArray (s, Array.fromList a)

        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         case i of
                             0 => c
                           | i => dotable(c, Index.prev' shape indices, i-1))
                in
                    make'(shape,dotable(c, Index.prev' shape last, length-1))
                end
            else
                raise Shape

        fun insert (x: 'a tensor, y: 'a tensor, i) =
            let
                val xshape     = (#shape x)
                val yshape     = (#shape y)
                val xdata      = (#data x)
                val ydata      = (#data y)
            in
                if not (rank x = rank y) 
                then raise Shape
                else 
                    (let 
                        val yr = Range.fromto yshape (Index.first yshape, Index.last yshape)
                        val xr = Range.fromto xshape (i, Index.+ (i, Index.last yshape))
                    in
                        Range.iteri2_range 
                            (fn ((xi,xi'),(yi,yi')) => 
                                let 
                                    val yn0  = Index.toInt yshape yi
                                    val yn1  = Index.toInt yshape yi'
                                    val len  = yn1-yn0+1
                                    val ysl = ArraySlice.slice (ydata, yn0, SOME len)
                                in
                                    (ArraySlice.copy {src=ysl,dst=xdata,di=(Index.toInt xshape xi)};
                                     true)
                                end)
                            (xr,yr)
                    end)
            end

        fun cat (x: 'a tensor, y: 'a tensor, dim) =
            (let val xshape = (#shape x)
                 val yshape = (#shape y)
                 val xdata  = (#data x)
                 val ydata  = (#data y)
             in
                 if not (rank x  < rank y) then
                     raise Shape
                 else
                     let 
                         val (_,newshape)   = ListPair.foldr
                                                  (fn (x,y,(i,ax)) => if (dim = i) then (i-1,(x+y) :: ax) 
                                                                      else if not (x=y) then raise Shape else (i-1,x :: ax))
                                                  ((rank x)-1,[]) (xshape, yshape)
                         val newlength  = Index.length newshape 
                         val newdata    = Array.array(newlength,Array.sub(xdata,0))
                     in
                         Array.copy {src=xdata,dst=newdata,di=0};
                         Array.copy {src=ydata,dst=newdata,di=(Index.length xshape)};
                         {shape = newshape,
                          indexer = Index.indexer newshape,
                          data = newdata}
                     end
             end)
                
                
        fun prepad (x: 'a tensor, len, c, dim) =
            (let val xshape = (#shape x)
                 val xdata  = (#data x)
             in
                 if (rank x) <= dim then
                     raise Shape
                 else
                     let 
                         val (_,newshape)   = List.foldr
                                                  (fn (x,(i,ax)) => 
                                                      if (dim = i) then (i-1,len :: ax)
                                                      else (i-1,x :: ax))
                                                  ((rank x)-1,[]) xshape
                     in
                         cat (new (newshape, c), x, dim)
                     end
             end)
                
        fun postpad (x: 'a tensor, len, c, dim) =
            (let val xshape = (#shape x)
                 val xdata  = (#data x)
             in
                 if (rank x) <= dim then
                     raise Shape
                 else
                     let 
                         val (_,newshape)   = List.foldr
                                                  (fn (x,(i,ax)) => 
                                                      if (dim = i) then (i-1,len :: ax)
                                                      else (i-1,x :: ax))
                                                  ((rank x)-1,[]) xshape
                     in
                         cat (x, new (newshape, c), dim)
                     end
             end)



        (*----- ELEMENTWISE OPERATIONS -----*)

        fun sub (t, index) = Array.sub(#data t, toInt t index)

        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)

        fun map f {shape, indexer, data} =
            {shape = shape, indexer = indexer, data = array_map f data}

        fun map2 f t1 t2=
            let val {shape, indexer, data} = t1
                val {shape=shape2, indexer=indexer2, data=data2} = t2
                fun apply i = f (Array.sub(data,i), Array.sub(data2,i))
                val len = Array.length data
            in
                if Index.eq(shape, shape2) then
                    {shape = shape,
                     indexer = indexer,
                     data = Array.tabulate(len, apply)}
                else
                    raise Match
        end

        fun appi f tensor = 
            let 
                val shape = shape tensor
                val next = Index.next shape
            in
                (Array.foldl 
                     (fn (v,i) => (f (i,v); valOf (next i)))
                     (Index.first shape) 
                     (toArray tensor); ())
            end

        fun app f tensor = Array.app f (toArray tensor)

        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end

        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun foldl f init tensor = Array.foldl f init (toArray tensor)

        fun foldln f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end

    end
    end (* Tensor *)


structure TensorSlice : TENSOR_SLICE =
    struct
        structure Tensor = Tensor
        structure Index  = Tensor.Index
        structure Range  = Range
            
        type index = Tensor.Index.t
        type range = Range.t
        type 'a tensor = 'a Tensor.tensor

        type 'a slice = {range : range, shapes: index list, tensor : 'a tensor}

        fun fromto (lo,up,tensor) =
            let val r = Range.fromto (Tensor.shape tensor) (lo,up)
            in
                {range=r,
                 shapes=(Range.shapes r),
                 tensor=tensor}
            end

        fun slice (rs,tensor) =
            let val r = (Range.ranges (Tensor.shape tensor) rs)
            in
                {range=r,
                 shapes=(Range.shapes r),
                 tensor=tensor}
            end

        fun length ({range, shapes, tensor}) = Range.length range
        fun base ({range, shapes, tensor})   = tensor
        fun shapes ({range, shapes, tensor}) = shapes
        fun range ({range, shapes, tensor})  = range

        fun map f slice = 
        let
           val te    = base slice
           val ra    = range slice
           val len   = length slice
           val fndx  = Range.first ra
           val arr   = Array.array (length slice, f (Tensor.sub (te,fndx)))
           val i     = ref 0
        in 
           Range.iteri (fn (ndx) => let val v = f (Tensor.sub (te,ndx)) in (Array.update (arr, !i, v); i := (!i + 1); true) end) ra;
           Tensor.fromArray ([1,len], arr)
        end

        fun app f (slice: 'a slice) = 
        let
           val te   = base slice
           val ra   = range slice
           val fndx = Range.first ra
        in 
           Range.iteri (fn (ndx) => (f (Tensor.sub (te,ndx)); true)) ra; ()
        end

        fun map2 f (sl1: 'a slice) (sl2: 'b slice) = 
        let
           val _      = if not ((shapes sl1) = (shapes sl2)) then raise Index.Shape else ()
           val te1    = base sl1
           val te2    = base sl2
           val ra1    = range sl1
           val ra2    = range sl2
           val len    = length sl1
           val fndx1  = Range.first ra1
           val fndx2  = Range.first ra2
           val arr    = Array.array (length(sl1), f (Tensor.sub(te1,fndx1),Tensor.sub(te2,fndx2)))
           val i      = ref 0
        in 
            Range.iteri2 (fn (ndx,ndx') => 
                             let 
                                 val v = f (Tensor.sub (te1,ndx),Tensor.sub (te2,ndx')) 
                             in 
                                 (Array.update (arr, !i, v); i := (!i + 1); true) 
                             end) 
                         (ra1,ra2);
           Tensor.fromArray ([1,len], arr)
        end

        fun foldl f init (slice: 'a slice) = 
        let
           val te     = base slice
           val sh     = Tensor.shape te
           val arr    = Tensor.toArray te
           val ra    = range slice
        in 
            Range.foldi_range
                (fn ((i,j),ax) => 
                    Loop.foldi (Index.toInt sh i, (Index.toInt sh j)+1,
                             fn (n,ax) => f (Array.sub (arr,n),ax), 
                                ax))
                init ra
        end

    end                                

(*
 MONO_TENSOR            - signature -

 Monomorphic tensor of arbitrary data (not only numbers). Operations
 should be provided to run the data in several ways, according to one
 index.

 type tensor
        The type of the tensor itself
 type elem
        The type of every element
 val storage = RowMajor | ColumnMajor
        RowMajor = data is stored in consecutive cells, last index
        varying fastest (C,C++,Pascal)
        ColumnMajor = data is stored in consecutive cells, first
        index varying fastest (Fortran,Matlab)
 new ([i1,...,in],init)
        Build a new tensor with n indices, each of sizes i1...in,
        filled with 'init'.
 fromArray (shape,data)
 fromList (shape,data)
        Use 'data' to fill a tensor of that shape. An exception is
        raised if 'data' is too large or too small to properly
        fill the vector. Later use of a 'data' array is disregarded
        -- one must think that the tensor now owns the array.
 length tensor
 rank tensor
 shape tensor
        Return the number of elements, the number of indices and
        the shape (size of each index) of the tensor.
 toArray tensor
        Return the data of the tensor in the form of an array.
        Mutation of this array may lead to unexpected behavior.
        The data in the array is stored according to `storage'.

 sub (tensor,[i1,...,in])
 update (tensor,[i1,...,in],new_value)
        Access the element that is indexed by the numbers [i1,..,in]

 map f a
 mapi f a
        Produce a new array by mapping the function sequentially
        as specified by 'storage', to each element of tensor 'a'.
        In 'mapi' the function receives a (indices,value) tuple,
        while in 'map' it only receives the value.
 app f a
 appi f a
        The same as 'map' and 'mapi' but the function 'f' outputs
        nothing and no new array is produced, i.e. one only seeks
        the side effect that 'f' may produce.
 map2 operation a b
        Apply function 'f' to pairs of elements of 'a' and 'b'
        and build a new tensor with the output. Both operands
        must have the same shape or an exception is raised.
        The procedure is sequential, as specified by 'storage'.
 foldl operation a n
        Fold-left the elements of tensor 'a' along the n-th
        index.
 all test a
 any test a
        Folded boolean tests on the elements of the tensor.

 map', map2', foldl'
        Polymorphic versions of map, map2, foldl.
*)

signature MONO_TENSOR =
    sig
        structure Array : MONO_ARRAY
        structure Index : INDEX
        type index = Index.t
        type elem
        type tensor
        type t = tensor

        val new : index * elem -> tensor
        val tabulate : index * (index -> elem) -> tensor
        val length : tensor -> int
        val rank : tensor -> int
        val shape : tensor -> (index)
        val reshape : index -> tensor -> tensor
        val fromList : index * elem list -> tensor
        val fromArray : index * Array.array -> tensor
        val toArray : tensor -> Array.array

        val sub : tensor * index -> elem
        val update : tensor * index * elem -> unit
        val map : (elem -> elem) -> tensor -> tensor
        val map2 : (elem * elem -> elem) -> tensor -> tensor -> tensor
        val app : (elem -> unit) -> tensor -> unit
        val appi : (index * elem -> unit) -> tensor -> unit
        val foldl : (elem * 'a -> 'a) -> 'a -> tensor -> 'a
        val foldln : (elem * elem -> elem) -> elem -> tensor -> int -> tensor
        val all : (elem -> bool) -> tensor -> bool
        val any : (elem -> bool) -> tensor -> bool

        val map' : (elem -> 'a) -> tensor -> 'a Tensor.tensor
        val map2' : (elem * elem -> 'a) -> tensor -> tensor -> 'a Tensor.tensor
        val foldl' : ('a * elem -> 'a) -> 'a -> tensor -> int -> 'a Tensor.tensor
    end

signature MONO_TENSOR_SLICE =
    sig
        structure Tensor : MONO_TENSOR
        structure Range : RANGE

        type index = Tensor.Index.t
        type range = Range.t
        type tensor = Tensor.tensor
        type slice
        type elem

        val fromto : index * index * tensor -> slice
        val slice  : ((index * index) list) * tensor -> slice

        val length : slice -> int
        val base   : slice -> tensor
        val shapes : slice -> index list
        val range  : slice -> range

        val app : (elem -> unit) -> slice -> unit
        val map : (elem -> elem) -> slice -> tensor
        val map2 : (elem * elem -> elem) -> slice -> slice -> tensor
        val foldl  : (elem * 'a -> 'a) -> 'a -> slice -> 'a

    end

(*
 NUMBER         - Signature -

 Guarantees a structure with a minimal number of mathematical operations
 so as to build an algebraic structure named Tensor.
 *)

signature NUMBER =
    sig
        type t
        val zero : t
        val one : t

        val + : t * t -> t
        val - : t * t -> t
        val * : t * t -> t
        val *+ : t * t * t -> t
        val *- : t * t * t -> t
        val ** : t * int -> t

        val ~ : t -> t
        val abs : t -> t
        val signum : t -> t

        val == : t * t -> bool
        val != : t * t -> bool

        val toString : t -> string
        val fromInt : int -> t
        val scan : (char,'a) StringCvt.reader -> (t,'a) StringCvt.reader
    end

signature INTEGRAL_NUMBER =
    sig
        include NUMBER

        val quot : t * t -> t
        val rem  : t * t -> t
        val mod  : t * t -> t
        val div  : t * t -> t

        val compare : t * t -> order
        val < : t * t -> bool
        val > : t * t -> bool
        val <= : t * t -> bool
        val >= : t * t -> bool

        val max : t * t -> t
        val min : t * t -> t
    end

signature FRACTIONAL_NUMBER =
    sig
        include NUMBER

        val pi : t
        val e : t

        val / : t * t -> t
        val recip : t -> t

        val ln : t -> t
        val pow : t * t -> t
        val exp : t -> t
        val sqrt : t -> t

        val cos : t -> t
        val sin : t -> t
        val tan : t -> t
        val sinh : t -> t
        val cosh : t -> t
        val tanh : t -> t

        val acos : t -> t
        val asin : t -> t
        val atan : t -> t
        val asinh : t -> t
        val acosh : t -> t
        val atanh : t -> t
        val atan2 : t * t -> t
    end

signature REAL_NUMBER =
    sig
        include FRACTIONAL_NUMBER

        val compare : t * t -> order
        val < : t * t -> bool
        val > : t * t -> bool
        val <= : t * t -> bool
        val >= : t * t -> bool

        val max : t * t -> t
        val min : t * t -> t
    end

signature COMPLEX_NUMBER =
    sig
        include FRACTIONAL_NUMBER

        structure Real : REAL_NUMBER
        type real = Real.t

        val make : real * real -> t
        val split : t -> real * real
        val realPart : t -> real
        val imagPart : t -> real
        val abs2 : t -> real
    end

structure INumber : INTEGRAL_NUMBER =
    struct
        open Int
        type t = Int.int
        val zero = 0
        val one = 1

        infix **
        fun i ** n =
            let fun loop 0 = 1
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if n < 0
               then raise Domain
               else loop n
            end

        fun signum i = case compare(i, 0) of
            GREATER => 1
          | EQUAL => 0
          | LESS => ~1

        infix ==
        infix !=
        fun a == b = a = b
        fun a != b = (a <> b)
        fun *+(b,c,a) = b * c + a
        fun *-(b,c,a) = b * c - b

        fun scan getc = Int.scan StringCvt.DEC getc
    end

structure RNumber : REAL_NUMBER =
    struct
        open Real
        open Real.Math
        type t = Real.real
        val zero = 0.0
        val one = 1.0

        fun signum x = case compare(x,0.0) of
            LESS => ~1.0
          | GREATER => 1.0
          | EQUAL => 0.0

        fun recip x = 1.0 / x

        infix **
        fun i ** n =
            let fun loop 0 = one
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if Int.<(n, 0)
               then raise Domain
               else loop n
            end

        fun max (a, b) = if a < b then b else a
        fun min (a, b) = if a < b then a else b

        fun asinh x = ln (x + sqrt(1.0 + x * x))
        fun acosh x = ln (x + (x + 1.0) * sqrt((x - 1.0)/(x + 1.0)))
        fun atanh x = ln ((1.0 + x) / sqrt(1.0 - x * x))

        fun toString (r) = (if r < zero 
                            then ("-" ^ Real.toString (abs r))
                            else Real.toString r)
    end
(*
 Complex(R)     - Functor -

 Provides support for complex numbers based on tuples. Should be
 highly efficient as most operations can be inlined.
 *)

structure CNumber : COMPLEX_NUMBER =
struct
        structure Real = RNumber

        type t = Real.t * Real.t
        type real = Real.t

        val zero = (0.0,0.0)
        val one = (1.0,0.0)
        val pi = (Real.pi, 0.0)
        val e = (Real.e, 0.0)

        fun make (r,i) = (r,i) : t
        fun split z = z
        fun realPart (r,_) = r
        fun imagPart (_,i) = i

        fun abs2 (r,i) = Real.+(Real.*(r,r),Real.*(i,i)) (* FIXME!!! *)
        fun arg (r,i) = Real.atan2(i,r)
        fun modulus z = Real.sqrt(abs2 z)
        fun abs z = (modulus z, 0.0)
        fun signum (z as (r,i)) =
            let val m = modulus z
            in (Real./(r,m), Real./(i,m))
            end

        fun ~ (r1,i1) = (Real.~ r1, Real.~ i1)
        fun (r1,i1) + (r2,i2) = (Real.+(r1,r2), Real.+(i1,i2))
        fun (r1,i1) - (r2,i2) = (Real.-(r1,r2), Real.-(i1,i1))
        fun (r1,i1) * (r2,i2) = (Real.-(Real.*(r1,r2),Real.*(i1,i2)),
                                 Real.+(Real.*(r1,i2),Real.*(r2,i1)))
        fun (r1,i1) / (r2,i2) =
            let val modulus = abs2(r2,i2)
                val (nr,ni) = (r1,i1) * (r2,i2)
            in
                (Real./(nr,modulus), Real./(ni,modulus))
            end
        fun *+((r1,i1),(r2,i2),(r0,i0)) =
            (Real.*+(Real.~ i1, i2, Real.*+(r1,r2,r0)),
             Real.*+(r2, i2, Real.*+(r1,i2,i0)))
        fun *-((r1,i1),(r2,i2),(r0,i0)) =
            (Real.*+(Real.~ i1, i2, Real.*-(r1,r2,r0)),
             Real.*+(r2, i2, Real.*-(r1,i2,i0)))

        infix **
        fun i ** n =
            let fun loop 0 = one
                  | loop 1 = i
                  | loop n =
                let val x = loop (Int.div(n, 2))
                    val m = Int.mod(n, 2)
                in
                    if m = 0 then
                        x * x
                    else
                        x * x * i
                end
            in if Int.<(n, 0)
                   then raise Domain
               else loop n
            end

        fun recip (r1, i1) = 
            let val modulus = abs2(r1, i1)
            in (Real./(r1, modulus), Real./(Real.~ i1, modulus))
            end
        fun ==(z, w) = Real.==(realPart z, realPart w) andalso Real.==(imagPart z, imagPart w)
        fun !=(z, w) = Real.!=(realPart z, realPart w) andalso Real.!=(imagPart z, imagPart w)
        fun fromInt i = (Real.fromInt i, 0.0)
        fun toString (r,i) =
            String.concat ["(",Real.toString r,",",Real.toString i,")"]

        fun exp (x, y) =
            let val expx = Real.exp x
            in (Real.*(x, (Real.cos y)), Real.*(x, (Real.sin y)))
            end

    local
        val half = Real.recip (Real.fromInt 2)
    in
        fun sqrt (z as (x,y)) =
            if Real.==(x, 0.0) andalso Real.==(y, 0.0) then
                zero
            else
                let val m = Real.+(modulus z, Real.abs x)
                    val u' = Real.sqrt (Real.*(m, half))
                    val v' = Real./(Real.abs y , Real.+(u',u'))
                    val (u,v) = if Real.<(x, 0.0) then (v',u') else (u',v')
                in (u, if Real.<(y, 0.0) then Real.~ v else v)
                end
    end
        fun ln z = (Real.ln (modulus z), arg z)

        fun pow (z, n) =
            let val l = ln z
            in exp (l * n)
            end

        fun sin (x, y) = (Real.*(Real.sin x, Real.cosh y),
                          Real.*(Real.cos x, Real.sinh y))
        fun cos (x, y) = (Real.*(Real.cos x, Real.cosh y),
                          Real.~ (Real.*(Real.sin x, Real.sinh y)))
        fun tan (x, y) =
            let val (sx, cx) = (Real.sin x, Real.cos x)
                val (shy, chy) = (Real.sinh y, Real.cosh y)
                val a = (Real.*(sx, chy), Real.*(cx, shy))
                val b = (Real.*(cx, chy), Real.*(Real.~ sx, shy))
            in a / b
            end

        fun sinh (x, y) = (Real.*(Real.cos y, Real.sinh x),
                           Real.*(Real.sin y, Real.cosh x))
        fun cosh (x, y) = (Real.*(Real.cos y, Real.cosh x),
                           Real.*(Real.sin y, Real.sinh x))
        fun tanh (x, y) =
            let val (sy, cy) = (Real.sin y, Real.cos y)
                val (shx, chx) = (Real.sinh x, Real.cosh x)
                val a = (Real.*(cy, shx), Real.*(sy, chx))
                val b = (Real.*(cy, chx), Real.*(sy, shx))
            in a / b
            end

        fun asin (z as (x,y)) =
            let val w = sqrt (one - z * z)
                val (x',y') = ln ((Real.~ y, x) + w)
            in (y', Real.~ x')
            end

        fun acos (z as (x,y)) = 
            let val (x', y') = sqrt (one + z * z)
                val (x'', y'') = ln (z + (Real.~ y', x'))
            in (y'', Real.~ x'')
            end

        fun atan (z as (x,y)) =
            let val w = sqrt (one + z*z)
                val (x',y') = ln ((Real.-(1.0, y), x) / w)
            in (y', Real.~ x')
            end

        fun atan2 (y, x) = atan(y / x)

        fun asinh x = ln (x + sqrt(one + x * x))
        fun acosh x = ln (x + (x + one) * sqrt((x - one)/(x + one)))
        fun atanh x = ln ((one + x) / sqrt(one - x * x))

        fun scan getc =
            let val scanner = Real.scan getc
            in fn stream => 
                  case scanner stream of
                      NONE => NONE
                    | SOME (a, rest) =>
                      case scanner rest of
                          NONE => NONE
                        | SOME (b, rest) => SOME (make(a,b), rest)
            end

end (* ComplexNumber *)

structure INumberArray =
    struct
        open Array
        type array = INumber.t array
        type vector = INumber.t vector
        type elem  = INumber.t
        structure Vector =
            struct
                open Vector
                type vector = INumber.t Vector.vector
                type elem = INumber.t
            end
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end

structure RNumberArray =
    struct
        open Real64Array
        val sub = Unsafe.Real64Array.sub
        val update = Unsafe.Real64Array.update
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end


structure INumberArray =
    struct
        open Array
        type array = INumber.t array
        type vector = INumber.t vector
        type elem  = INumber.t
        structure Vector =
            struct
                open Vector
                type vector = INumber.t Vector.vector
                type elem = INumber.t
            end
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end
structure RNumberArray =
    struct
        open Real64Array
        val sub = Unsafe.Real64Array.sub
        val update = Unsafe.Real64Array.update
        fun map f a = tabulate(length a, fn x => (f (sub(a,x))))
        fun mapi f a = tabulate(length a, fn x => (f (x,sub(a,x))))
        fun map2 f a b = tabulate(length a, fn x => (f(sub(a,x),sub(b,x))))
    end
(*--------------------- COMPLEX ARRAY -------------------------*)
structure BasicCNumberArray =
struct
        structure Complex : COMPLEX_NUMBER = CNumber
        structure Array : MONO_ARRAY = RNumberArray
        type elem = Complex.t
        type array = Array.array * Array.array
        val maxLen = Array.maxLen
        fun length (a,b) = Array.length a
        fun sub ((a,b),index) = Complex.make(Array.sub(a,index),Array.sub(b,index))
        fun update ((a,b),index,z) =
            let val (re,im) = Complex.split z in
                Array.update(a, index, re);
                Array.update(b, index, im)
            end
    local
        fun makeRange (a, start, NONE) = makeRange(a, start, SOME (length a - 1))
          | makeRange (a, start, SOME last) =
            let val len = length a
                val diff = last - start
            in
                if (start >= len) orelse (last >= len) then
                    raise Subscript
                else if diff < 0 then
                    (a, start, 0)
                else
                    (a, start, diff + 1)
            end
    in
        fun array (size,z:elem) =
            let val realsize = size * 2
                val r = Complex.realPart z
                val i = Complex.imagPart z in
                    (Array.array(size,r), Array.array(size,i))
            end
        fun zeroarray size =
            (Array.array(size,Complex.Real.zero),
             Array.array(size,Complex.Real.zero))
        fun tabulate (size,f) =
            let val a = array(size, Complex.zero)
                fun loop i =
                    case i = size of
                        true => a
                      | false => (update(a, i, f i); loop (i+1))
            in
                loop 0
            end
        fun fromList list =
            let val length = List.length list
                val a = zeroarray length
                fun loop (_, []) = a
                  | loop (i, z::rest) = (update(a, i, z);
                                         loop (i+1, rest))
            in
                loop(0,list)
            end
        fun extract range =
            let val (a, start, len) = makeRange range
                fun copy i = sub(a, i + start)
            in tabulate(len, copy)
            end
        fun concat array_list =
            let val total_length = foldl (op +) 0 (map length array_list)
                val a = array(total_length, Complex.zero)
                fun copy (_, []) = a
                  | copy (pos, v::rest) =
                    let fun loop i =
                        case i = 0 of
                            true => ()
                          | false => (update(a, i+pos, sub(v, i)); loop (i-1))
                    in (loop (length v - 1); copy(length v + pos, rest))
                    end
            in
                copy(0, array_list)
            end
        fun copy {src : array, si : int, len : int option, dst : array, di : int } =
            let val (a, ia, la) = makeRange (src, si, len)
                val (b, ib, lb) = makeRange (dst, di, len)
                fun copy i =
                    case i < 0 of
                        true => ()
                      | false => (update(b, i+ib, sub(a, i+ia)); copy (i-1))
            in copy (la - 1)
            end
        val copyVec = copy
        fun modifyi f range =
            let val (a, start, len) = makeRange range
                val last = start + len
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (update(a, i, f(i, sub(a,i))); loop (i+1))
            in loop start
            end
        fun modify f a =
            let val last = length a
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (update(a, i, f(sub(a,i))); loop (i+1))
            in loop 0
            end
        fun app f a =
            let val size = length a
                fun loop i =
                    case i = size of
                        true => ()
                      | false => (f(sub(a,i)); loop (i+1))
            in
                loop 0
            end
        fun appi f range =
            let val (a, start, len) = makeRange range
                val last = start + len
                fun loop i =
                    case i >= last of
                        true => ()
                      | false => (f(i, sub(a,i)); loop (i+1))
            in
                loop start
            end
        fun map f a =
            let val len = length a
                val c = zeroarray len
                fun loop ~1 = c
                  | loop i = (update(a, i, f(sub(a,i))); loop (i-1))
            in loop (len-1)
            end
        fun map2 f a b =
            let val len = length a
                val c = zeroarray len
                fun loop ~1 = c
                  | loop i = (update(c, i, f(sub(a,i),sub(b,i)));
                              loop (i-1))
            in loop (len-1)
            end
        fun mapi f range =
            let val (a, start, len) = makeRange range
                fun rule i = f (i+start, sub(a, i+start))
            in tabulate(len, rule)
            end
        fun foldli f init range =
            let val (a, start, len) = makeRange range
                val last = start + len - 1
                fun loop (i, accum) =
                    case i > last of
                        true => accum
                      | false => loop (i+1, f(i, sub(a,i), accum))
            in loop (start, init)
            end
        fun foldri f init range =
            let val (a, start, len) = makeRange range
                val last = start + len - 1
                fun loop (i, accum) =
                    case i < start of
                        true => accum
                      | false => loop (i-1, f(i, sub(a,i), accum))
            in loop (last, init)
            end
        fun foldl f init a = foldli (fn (_, a, x) => f(a,x)) init (a,0,NONE)
        fun foldr f init a = foldri (fn (_, x, a) => f(x,a)) init (a,0,NONE)
    end
end (* BasicCNumberArray *)
structure CNumberArray =
    struct
        structure Vector =
            struct
                open BasicCNumberArray
                type vector = array
            end : MONO_VECTOR
        type vector = Vector.vector
        open BasicCNumberArray
    end (* CNumberArray *)
structure ITensor =
    struct
        structure Number = INumber
        structure Array = INumberArray

structure MonoTensor  =
    struct
(* PARAMETERS
        structure Array = Array
*)
        structure Index  = Index
        type elem = Array.elem
        type index = Index.t
        type tensor = {shape : index, indexer : Index.indexer, data : Array.array}
        type t = tensor
        exception Shape
        exception Match
        exception Index
    local
    (*----- LOCALS -----*)
        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}
        fun toInt {shape, indexer, data} index = indexer index
        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end
          | splitList ([], _) = ([],0,[])
    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)

        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end
        fun toArray {shape, indexer, data} = data
        fun length {shape, indexer, data} =  Array.length data
        fun shape {shape, indexer, data} = shape
        fun rank t = List.length (shape t)
        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape
        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape
        fun fromList (s, a) = fromArray (s, Array.fromList a)
        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         if i <= 1
                         then c
                         else dotable(c, Index.prev' shape indices, i-1))
                in make'(shape,dotable(c, Index.prev' shape last, length-2))
                end
            else
                raise Shape



      fun cat (x: tensor, y: tensor, dim) =
        (let val xshape = (#shape x)
             val yshape = (#shape y)
             val xdata  = (#data x)
             val ydata  = (#data y)
        in
           if  not (rank x  = rank y) then
           raise Shape
           else
                let 
                   val (_,newshape)   = ListPair.foldr
                                      (fn (x,y,(i,ax)) => if (dim = i) then (i-1,(x+y) :: ax) 
                                                                       else if not (x=y) then raise Shape else (i-1,x :: ax))
                                       ((rank x)-1,[]) (xshape, yshape)
                   val newlength  = Index.length newshape 
                   val newdata    = Array.array(newlength,Array.sub(xdata,0))
                in
                    Array.copy {src=xdata,dst=newdata,di=0};
                    Array.copy {src=ydata,dst=newdata,di=(Index.length xshape)};
                    {shape = newshape,
                     indexer = Index.indexer newshape,
                     data = newdata}
                end
        end)


      fun prepad (x: tensor, len, c, dim) =
          (let val xshape = (#shape x)
               val xdata  = (#data x)
           in
               if (rank x) <= dim then
                   raise Shape
               else
                   let 
                       val (_,newshape)   = List.foldr
                                                (fn (x,(i,ax)) => 
                                                    if (dim = i) then (i-1,len :: ax)
                                                    else (i-1,x :: ax))
                                                ((rank x)-1,[]) xshape
                   in
                       cat (new (newshape, c), x, dim)
                   end
           end)

      fun postpad (x: tensor, len, c, dim) =
          (let val xshape = (#shape x)
               val xdata  = (#data x)
           in
               if (rank x) <= dim then
                   raise Shape
               else
                   let 
                       val (_,newshape)   = List.foldr
                                                (fn (x,(i,ax)) => 
                                                    if (dim = i) then (i-1,len :: ax)
                                                    else (i-1,x :: ax))
                                                ((rank x)-1,[]) xshape
                   in
                       cat (x, new (newshape, c), dim)
                   end
           end)



        (*----- ELEMENTWISE OPERATIONS -----*)
        fun sub (t, index) = Array.sub(#data t, toInt t index)
        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)
        fun map f {shape, indexer, data} =
            {shape = shape, indexer = indexer, data = Array.map f data}
        fun map2 f t1 t2=
            let val {shape=shape1, indexer=indexer1, data=data1} = t1
                val {shape=shape2, indexer=indexer2, data=data2} = t2
            in
                if Index.eq(shape1,shape2) then
                    {shape = shape1,
                     indexer = indexer1,
                     data = Array.map2 f data1 data2}
                else
                    raise Match
        end
        fun appi f tensor = 
            let 
                val shape = shape tensor
                val next = Index.next shape
            in
                (Array.foldl 
                     (fn (v,i) => (f (i,v); valOf (next i)))
                     (Index.first shape) 
                     (toArray tensor); ())
            end
        fun app f tensor = Array.app f (toArray tensor)
        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun foldl f init tensor = Array.foldl f init (toArray tensor)
        fun foldln f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
        (* --- POLYMORPHIC ELEMENTWISE OPERATIONS --- *)
        fun array_map' f a =
            let fun apply index = f(Array.sub(a,index)) in
                Tensor.Array.tabulate(Array.length a, apply)
            end
        fun map' f t = Tensor.fromArray(shape t, array_map' f (toArray t))
        fun map2' f t1 t2 =
            let val d1 = toArray t1
                val d2 = toArray t2
                fun apply i = f (Array.sub(d1,i), Array.sub(d2,i))
                val len = Array.length d1
            in
                if Index.eq(shape t1, shape t2) then
                    Tensor.fromArray(shape t1, Tensor.Array.tabulate(len,apply))
                else
                    raise Match
            end
        fun foldl' f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Tensor.Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Tensor.Array.update(c,ic,f(Tensor.Array.sub(c,ic),Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
    end
    end (* MonoTensor *)
        open MonoTensor
    local
        (*
         LEFT INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(a(k,i2,...,jn)*b(k,j2,...jn)) forall k
         MEANINGFUL VARIABLES:
         lk = i1 = j1
         li = i2*...*in
         lj = j2*...*jn
         *)
        fun do_fold_first a b c lk lj li =
            let fun loopk (0, _,  _,  accum) = accum
                  | loopk (k, ia, ib, accum) =
                    let val delta = Number.*(Array.sub(a,ia),Array.sub(b,ib))
                    in loopk (k-1, ia+1, ib+1, Number.+(delta,accum))
                    end
                fun loopj (0, ib, ic) = c
                  | loopj (j, ib, ic) =
                    let fun loopi (0, ia, ic) = ic
                          | loopi (i, ia, ic) =
                        (Array.update(c, ic, loopk(lk, ia, ib, Number.zero));
                         loopi(i-1, ia+lk, ic+1))
                    in
                        loopj(j-1, ib+lk, loopi(li, 0, ic))
                    end
            in loopj(lj, 0, 0)
            end
    in
        fun +* ta tb =
            let 
                val (rank_a,a) = (rank ta, toArray ta)
                val (rank_b,b) = (rank tb, toArray tb)
            in 
                case (shape ta, shape tb) of
                    (lk::rest_a,lk2::rest_b) =>
                    (if not(lk = lk2)
                     then raise Match
                     else let val li = Index.length rest_a
                              val lj = Index.length rest_b
                              val c = Array.array(li*lj,Number.zero)
                          in fromArray(rest_a @ rest_b,
                                       do_fold_first a b c lk li lj)
                          end)
                  | _ => raise Match
            end
    end
    local
        (*
         LAST INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(mult(a(i1,i2,...,k),b(j1,j2,...,k))) forall k
         MEANINGFUL VARIABLES:
         lk = in = jn
         li = i1*...*i(n-1)
         lj = j1*...*j(n-1)
         *)
        fun do_fold_last a b c lk lj li =
            let fun loopi (0, ia, ic, fac) = ()
                  | loopi (i, ia, ic, fac) =
                    let val old = Array.sub(c,ic)
                        val inc = Number.*(Array.sub(a,ia),fac)
                    in
                        Array.update(c,ic,Number.+(old,inc));
                        loopi(i-1, ia+1, ic+1, fac)
                    end
                fun loopj (j, ib, ic) =
                    let fun loopk (0, ia, ib) = ()
                          | loopk (k, ia, ib) =
                            (loopi(li, ia, ic, Array.sub(b,ib));
                             loopk(k-1, ia+li, ib+lj))
                    in case j of
                           0 => c
                         | _ => (loopk(lk, 0, ib);
                                 loopj(j-1, ib+1, ic+li))
                    end (* loopj *)
            in
                loopj(lj, 0, 0)
            end
    in
        fun *+ ta tb  =
            let val (rank_a,shape_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,shape_b,b) = (rank tb, shape tb, toArray tb)
            in 
                case (List.rev (shape_a), List.rev (shape_b)) of
                    (lk::rest_a,lk2::rest_b) =>
                    (if not(lk = lk2)
                     then raise Match
                     else let val li = Index.length rest_a
                              val lj = Index.length rest_b
                              val c = Array.array(li*lj,Number.zero)
                          in fromArray(List.rev rest_a @ List.rev rest_b,
                                       do_fold_last a b c lk li lj)
                          end)
                    | _ => raise Match
            end
    end
        (* ALGEBRAIC OPERATIONS *)
        infix **
        infix ==
        infix !=
        fun a + b = map2 Number.+ a b
        fun a - b = map2 Number.- a b
        fun a * b = map2 Number.* a b
        fun a ** i = map (fn x => (Number.**(x,i))) a
        fun ~ a = map Number.~ a
        fun abs a = map Number.abs a
        fun signum a = map Number.signum a
        fun a == b = map2' Number.== a b
        fun a != b = map2' Number.!= a b
        fun toString a = raise Domain
        fun fromInt a = new([1], Number.fromInt a)
        (* TENSOR SPECIFIC OPERATIONS *)
        fun *> n = map (fn x => Number.*(n,x))
        fun normInf a =
            let fun accum (y,x) = Number.max(x,Number.abs y)
            in  foldl accum Number.zero a
            end
    end (* NumberTensor *)

structure RTensor =
    struct
        structure Number = RNumber
        structure Array = RNumberArray
        structure ArraySlice = Real64ArraySlice

structure MonoTensor  =
    struct
(* PARAMETERS
        structure Array = Array
*)
        structure Index  = Index
        type elem = Array.elem
        type index = Index.t
        type tensor = {shape : index, indexer : Index.indexer, data : Array.array}
        type t = tensor
        exception Shape
        exception Match
        exception Index
    local
    (*----- LOCALS -----*)
        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}
        fun toInt {shape, indexer, data} index = indexer index
        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end
          | splitList ([], _) = ([],0,[])
    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)

        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end
        fun toArray {shape, indexer, data} = data
        fun length {shape, indexer, data} =  Array.length data
        fun shape {shape, indexer, data} = shape
        fun rank t = List.length (shape t)
        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape
        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape
        fun fromList (s, a) = fromArray (s, Array.fromList a)
        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         if i <= 1
                         then c
                         else dotable(c, Index.prev' shape indices, i-1))
                in make'(shape,dotable(c, Index.prev' shape last, length-2))
                end
            else
                raise Shape


        fun insert (x: tensor, y: tensor, i) =
            let
                val xshape     = (#shape x)
                val yshape     = (#shape y)
                val xdata      = (#data x)
                val ydata      = (#data y)
            in
                if not (rank x = rank y) 
                then raise Shape
                else 
                    (let 
                        val yr = Range.fromto yshape (Index.first yshape, Index.last yshape)
                        val xr = Range.fromto xshape (i, Index.+ (i, Index.last yshape))
                    in
                        Range.iteri2_range 
                            (fn ((xi,xi'),(yi,yi')) => 
                                let 
                                    val yn0  = Index.toInt yshape yi
                                    val yn1  = Index.toInt yshape yi'
                                    val len  = yn1-yn0+1
                                    val ysl = ArraySlice.slice (ydata, yn0, SOME len)
                                in
                                    (ArraySlice.copy {src=ysl,dst=xdata,di=(Index.toInt xshape xi)};
                                     true)
                                end)
                            (xr,yr)
                    end)
            end

      fun cat (x: tensor, y: tensor, dim) =
        (let val xshape = (#shape x)
             val yshape = (#shape y)
             val xdata  = (#data x)
             val ydata  = (#data y)
        in
           if  not (rank x  = rank y) then
           raise Shape
           else
                let 
                   val (_,newshape)   = ListPair.foldr
                                      (fn (x,y,(i,ax)) => if (dim = i) then (i-1,(x+y) :: ax) 
                                                                       else if not (x=y) then raise Shape else (i-1,x :: ax))
                                       ((rank x)-1,[]) (xshape, yshape)
                   val newlength  = Index.length newshape 
                   val newdata    = Array.array(newlength,Array.sub(xdata,0))
                in
                    Array.copy {src=xdata,dst=newdata,di=0};
                    Array.copy {src=ydata,dst=newdata,di=(Index.length xshape)};
                    {shape = newshape,
                     indexer = Index.indexer newshape,
                     data = newdata}
                end
        end)

      fun prepad (x: tensor, len, c, dim) =
          (let val xshape = (#shape x)
               val xdata  = (#data x)
           in
               if (rank x) <= dim then
                   raise Shape
               else
                   let 
                       val (_,newshape)   = List.foldr
                                                (fn (x,(i,ax)) => 
                                                    if (dim = i) then (i-1,len :: ax)
                                                    else (i-1,x :: ax))
                                                ((rank x)-1,[]) xshape
                   in
                       cat (new (newshape, c), x, dim)
                   end
           end)

      fun postpad (x: tensor, len, c, dim) =
          (let val xshape = (#shape x)
               val xdata  = (#data x)
           in
               if (rank x) <= dim then
                   raise Shape
               else
                   let 
                       val (_,newshape)   = List.foldr
                                                (fn (x,(i,ax)) => 
                                                    if (dim = i) then (i-1,len :: ax)
                                                    else (i-1,x :: ax))
                                                ((rank x)-1,[]) xshape
                   in
                       cat (x, new (newshape, c), dim)
                   end
           end)


        (*----- ELEMENTWISE OPERATIONS -----*)
        fun sub (t, index) = Array.sub(#data t, toInt t index)
        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)
        fun map f {shape, indexer, data} =
            {shape = shape, indexer = indexer, data = Array.map f data}
        fun map2 f t1 t2=
            let val {shape=shape1, indexer=indexer1, data=data1} = t1
                val {shape=shape2, indexer=indexer2, data=data2} = t2
            in
                if Index.eq(shape1,shape2) then
                    {shape = shape1,
                     indexer = indexer1,
                     data = Array.map2 f data1 data2}
                else
                    raise Match
        end
        fun appi f tensor = 
            let 
                val shape = shape tensor
                val next = Index.next shape
            in
                (Array.foldl 
                     (fn (v,i) => (f (i,v); valOf (next i)))
                     (Index.first shape) 
                     (toArray tensor); ())
            end
        fun app f tensor = Array.app f (toArray tensor)
        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun foldl f init tensor = Array.foldl f init (toArray tensor)

        fun foldln f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
        (* --- POLYMORPHIC ELEMENTWISE OPERATIONS --- *)
        fun array_map' f a =
            let fun apply index = f(Array.sub(a,index)) in
                Tensor.Array.tabulate(Array.length a, apply)
            end
        fun map' f t = Tensor.fromArray(shape t, array_map' f (toArray t))
        fun map2' f t1 t2 =
            let val d1 = toArray t1
                val d2 = toArray t2
                fun apply i = f (Array.sub(d1,i), Array.sub(d2,i))
                val len = Array.length d1
            in
                if Index.eq(shape t1, shape t2) then
                    Tensor.fromArray(shape t1, Tensor.Array.tabulate(len,apply))
                else
                    raise Match
            end
        fun foldl' f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Tensor.Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Tensor.Array.update(c,ic,f(Tensor.Array.sub(c,ic),Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
    end
    end (* MonoTensor *)
        open MonoTensor
    local
        (*
         LEFT INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(a(k,i2,...,jn)*b(k,j2,...jn)) forall k
         MEANINGFUL VARIABLES:
         lk = i1 = j1
         li = i2*...*in
         lj = j2*...*jn
         *)
        fun do_fold_first a b c lk lj li =
            let fun loopk (0, _,  _,  accum) = accum
                  | loopk (k, ia, ib, accum) =
                    let val delta = Number.*(Array.sub(a,ia),Array.sub(b,ib))
                    in loopk (k-1, ia+1, ib+1, Number.+(delta,accum))
                    end
                fun loopj (0, ib, ic) = c
                  | loopj (j, ib, ic) =
                    let fun loopi (0, ia, ic) = ic
                          | loopi (i, ia, ic) =
                        (Array.update(c, ic, loopk(lk, ia, ib, Number.zero));
                         loopi(i-1, ia+lk, ic+1))
                    in
                        loopj(j-1, ib+lk, loopi(li, 0, ic))
                    end
            in loopj(lj, 0, 0)
            end
    in
        fun +* ta tb =
            let val (rank_a,a) = (rank ta, toArray ta)
                val (rank_b,b) = (rank tb, toArray tb)
            in 
                case (shape ta, shape tb) of
                    (lk::rest_a,lk2::rest_b) =>
                    (if not(lk = lk2)
                     then raise Match
                     else let val li = Index.length rest_a
                              val lj = Index.length rest_b
                              val c = Array.array(li*lj,Number.zero)
                          in fromArray(rest_a @ rest_b,
                                       do_fold_first a b c lk li lj)
                          end)
                    | _ => raise Match
            end
    end
    local
        (*
         LAST INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(mult(a(i1,i2,...,k),b(j1,j2,...,k))) forall k
         MEANINGFUL VARIABLES:
         lk = in = jn
         li = i1*...*i(n-1)
         lj = j1*...*j(n-1)
         *)
        fun do_fold_last a b c lk lj li =
            let fun loopi (0, ia, ic, fac) = ()
                  | loopi (i, ia, ic, fac) =
                    let val old = Array.sub(c,ic)
                        val inc = Number.*(Array.sub(a,ia),fac)
                    in
                        Array.update(c,ic,Number.+(old,inc));
                        loopi(i-1, ia+1, ic+1, fac)
                    end
                fun loopj (j, ib, ic) =
                    let fun loopk (0, ia, ib) = ()
                          | loopk (k, ia, ib) =
                            (loopi(li, ia, ic, Array.sub(b,ib));
                             loopk(k-1, ia+li, ib+lj))
                    in case j of
                           0 => c
                         | _ => (loopk(lk, 0, ib);
                                 loopj(j-1, ib+1, ic+li))
                    end (* loopj *)
            in
                loopj(lj, 0, 0)
            end
    in
        fun *+ ta tb  =
            let val (rank_a,shape_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,shape_b,b) = (rank tb, shape tb, toArray tb)
            in 
                case (List.rev (shape_a), List.rev (shape_b)) of
                    (lk::rest_a,lk2::rest_b) =>
                    (if not(lk = lk2)
                     then raise Match
                     else let val li = Index.length rest_a
                              val lj = Index.length rest_b
                              val c = Array.array(li*lj,Number.zero)
                          in fromArray(List.rev rest_a @ List.rev rest_b,
                                       do_fold_last a b c lk li lj)
                          end)
                  | _ => raise Match
            end
    end
        (* ALGEBRAIC OPERATIONS *)
        infix **
        infix ==
        infix !=
        fun a + b = map2 Number.+ a b
        fun a - b = map2 Number.- a b
        fun a * b = map2 Number.* a b
        fun a ** i = map (fn x => (Number.**(x,i))) a
        fun ~ a = map Number.~ a
        fun abs a = map Number.abs a
        fun signum a = map Number.signum a
        fun a == b = map2' Number.== a b
        fun a != b = map2' Number.!= a b
        fun toString a = raise Domain
        fun fromInt a = new([1], Number.fromInt a)
        (* TENSOR SPECIFIC OPERATIONS *)
        fun *> n = map (fn x => Number.*(n,x))
        fun a / b = map2 Number./ a b
        fun recip a = map Number.recip a
        fun ln a = map Number.ln a
        fun pow (a, b) = map (fn x => (Number.pow(x,b))) a
        fun exp a = map Number.exp a
        fun sqrt a = map Number.sqrt a
        fun cos a = map Number.cos a
        fun sin a = map Number.sin a
        fun tan a = map Number.tan a
        fun sinh a = map Number.sinh a
        fun cosh a = map Number.cosh a
        fun tanh a = map Number.tanh a
        fun asin a = map Number.asin a
        fun acos a = map Number.acos a
        fun atan a = map Number.atan a
        fun asinh a = map Number.asinh a
        fun acosh a = map Number.acosh a
        fun atanh a = map Number.atanh a
        fun atan2 (a,b) = map2 Number.atan2 a b
        fun normInf a =
            let fun accum (y,x) = Number.max(x,Number.abs y)
            in  foldl accum Number.zero a
            end
        fun norm1 a =
            let fun accum (y,x) = Number.+(x,Number.abs y)
            in  foldl accum Number.zero a
            end
        fun norm2 a =
            let fun accum (y,x) = Number.+(x, Number.*(y,y))
            in Number.sqrt(foldl accum Number.zero a)
            end
    end (* RTensor *)
structure CTensor =
struct
    structure Number = CNumber
    structure Array = CNumberArray

structure MonoTensor  =
    struct
(* PARAMETERS
        structure Array = Array
*)
        structure Index  = Index
        type elem = Array.elem
        type index = Index.t
        type tensor = {shape : index, indexer : Index.indexer, data : Array.array}
        type t = tensor
        exception Shape
        exception Match
        exception Index
    local
    (*----- LOCALS -----*)
        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}
        fun toInt {shape, indexer, data} index = indexer index
        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end
          | splitList ([], _) = ([],0,[])
    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)
        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end
        fun toArray {shape, indexer, data} = data
        fun length {shape, indexer, data} =  Array.length data
        fun shape {shape, indexer, data} = shape
        fun rank t = List.length (shape t)
        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape
        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape
        fun fromList (s, a) = fromArray (s, Array.fromList a)
        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         if i <= 1
                         then c
                         else dotable(c, Index.prev' shape indices, i-1))
                in make'(shape,dotable(c, Index.prev' shape last, length-2))
                end
            else
                raise Shape
        (*----- ELEMENTWISE OPERATIONS -----*)
        fun sub (t, index) = Array.sub(#data t, toInt t index)
        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)
        fun map f {shape, indexer, data} =
            {shape = shape, indexer = indexer, data = Array.map f data}
        fun map2 f t1 t2=
            let val {shape=shape1, indexer=indexer1, data=data1} = t1
                val {shape=shape2, indexer=indexer2, data=data2} = t2
            in
                if Index.eq(shape1,shape2) then
                    {shape = shape1,
                     indexer = indexer1,
                     data = Array.map2 f data1 data2}
                else
                    raise Match
        end
        fun appi f tensor = 
            let 
                val shape = shape tensor
                val next = Index.next shape
            in
                (Array.foldl 
                     (fn (v,i) => (f (i,v); valOf (next i)))
                     (Index.first shape) 
                     (toArray tensor); ())
            end
        fun app f tensor = Array.app f (toArray tensor)
        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun foldl f init tensor = Array.foldl f init (toArray tensor)
        fun foldln f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
        (* --- POLYMORPHIC ELEMENTWISE OPERATIONS --- *)
        fun array_map' f a =
            let fun apply index = f(Array.sub(a,index)) in
                Tensor.Array.tabulate(Array.length a, apply)
            end
        fun map' f t = Tensor.fromArray(shape t, array_map' f (toArray t))
        fun map2' f t1 t2 =
            let val d1 = toArray t1
                val d2 = toArray t2
                fun apply i = f (Array.sub(d1,i), Array.sub(d2,i))
                val len = Array.length d1
            in
                if Index.eq(shape t1, shape t2) then
                    Tensor.fromArray(shape t1, Tensor.Array.tabulate(len,apply))
                else
                    raise Match
            end
        fun foldl' f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Tensor.Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Tensor.Array.update(c,ic,f(Tensor.Array.sub(c,ic),Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
    end
    end (* MonoTensor *)
    open MonoTensor
    local
        (*
         LEFT INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(a(k,i2,...,jn)*b(k,j2,...jn)) forall k
         MEANINGFUL VARIABLES:
         lk = i1 = j1
         li = i2*...*in
         lj = j2*...*jn
         *)
        fun do_fold_first a b c lk lj li =
            let fun loopk (0, _, _, r, i) = Number.make(r,i)
                  | loopk (k, ia, ib, r, i) =
                    let val (ar, ai) = Array.sub(a,ia)
                        val (br, bi) = Array.sub(b,ib)
                        val dr = ar * br - ai * bi
                        val di = ar * bi + ai * br
                    in loopk (k-1, ia+1, ib+1, r+dr, i+di)
                    end
                fun loopj (0, ib, ic) = c
                  | loopj (j, ib, ic) =
                    let fun loopi (0, ia, ic) = ic
                          | loopi (i, ia, ic) =
                            (Array.update(c, ic, loopk(lk, ia, ib, RNumber.zero, RNumber.zero));
                             loopi(i-1, ia+lk, ic+1))
                    in loopj(j-1, ib+lk, loopi(li, 0, ic))
                    end
            in loopj(lj, 0, 0)
            end
    in
        fun +* ta tb =
            let val (rank_a,a) = (rank ta, toArray ta)
                val (rank_b,b) = (rank tb, toArray tb)
            in 
                case (shape ta, shape tb) of
                    (lk::rest_a,lk2::rest_b) =>
                    (if not(lk = lk2)
                     then raise Match
                     else let val li = Index.length rest_a
                              val lj = Index.length rest_b
                              val c = Array.array(li*lj,Number.zero)
                          in fromArray(rest_a @ rest_b, do_fold_first a b c lk li lj)
                          end)
                    | _ => raise Match
            end
    end
    local
        (*
         LAST INDEX CONTRACTION:
         a = a(i1,i2,...,in)
         b = b(j1,j2,...,jn)
         c = c(i2,...,in,j2,...,jn)
         = sum(mult(a(i1,i2,...,k),b(j1,j2,...,k))) forall k
         MEANINGFUL VARIABLES:
         lk = in = jn
         li = i1*...*i(n-1)
         lj = j1*...*j(n-1)
         *)
        fun do_fold_last a b c lk lj li =
            let fun loopi(0, _, _, _, _) = ()
                  | loopi(i, ia, ic, br, bi) =
                    let val (cr,ci) = Array.sub(c,ic)
                        val (ar,ai) = Array.sub(a,ia)
                        val dr = (ar * br - ai * bi)
                        val di = (ar * bi + ai * br)
                    in
                        Array.update(c,ic,Number.make(cr+dr,ci+di));
                        loopi(i-1, ia+1, ic+1, br, bi)
                    end
                fun loopj(j, ib, ic) =
                    let fun loopk(0, _, _) = ()
                          | loopk(k, ia, ib) =
                            let val (br, bi) = Array.sub(b,ib)
                            in
                                loopi(li, ia, ic, br, bi);
                                loopk(k-1, ia+li, ib+lj)
                            end
                in case j of
                    0 => c
                  | _ => (loopk(lk, 0, ib);
                          loopj(j-1, ib+1, ic+li))
                end (* loopj *)
            in
                loopj(lj, 0, 0)
            end
    in
        fun *+ ta tb  =
            let val (rank_a,shape_a,a) = (rank ta, shape ta, toArray ta)
                val (rank_b,shape_b,b) = (rank tb, shape tb, toArray tb)
            in
                case (List.rev (shape_a), List.rev (shape_b)) of
                    (lk::rest_a,lk2::rest_b) =>
                    (if not(lk = lk2) then
                         raise Match
                     else
                         let val li = Index.length rest_a
                             val lj = Index.length rest_b
                             val c = Array.array(li*lj,Number.zero)
                         in
                             fromArray(List.rev rest_a @ List.rev rest_b,
                                       do_fold_last a b c lk li lj)
                         end)
                  | _ => raise Match
            end
    end
    (* ALGEBRAIC OPERATIONS *)
    infix **
    infix ==
    infix !=
    fun a + b = map2 Number.+ a b
    fun a - b = map2 Number.- a b
    fun a * b = map2 Number.* a b
    fun a ** i = map (fn x => (Number.**(x,i))) a
    fun ~ a = map Number.~ a
    fun abs a = map Number.abs a
    fun signum a = map Number.signum a
    fun a == b = map2' Number.== a b
    fun a != b = map2' Number.!= a b
    fun toString a = raise Domain
    fun fromInt a = new([1], Number.fromInt a)
    (* TENSOR SPECIFIC OPERATIONS *)
    fun *> n = map (fn x => Number.*(n,x))
    fun a / b = map2 Number./ a b
    fun recip a = map Number.recip a
    fun ln a = map Number.ln a
    fun pow (a, b) = map (fn x => (Number.pow(x,b))) a
    fun exp a = map Number.exp a
    fun sqrt a = map Number.sqrt a
    fun cos a = map Number.cos a
    fun sin a = map Number.sin a
    fun tan a = map Number.tan a
    fun sinh a = map Number.sinh a
    fun cosh a = map Number.cosh a
    fun tanh a = map Number.tanh a
    fun asin a = map Number.asin a
    fun acos a = map Number.acos a
    fun atan a = map Number.atan a
    fun asinh a = map Number.asinh a
    fun acosh a = map Number.acosh a
    fun atanh a = map Number.atanh a
    fun atan2 (a,b) = map2 Number.atan2 a b
    fun normInf a =
        let fun accum (y,x) = RNumber.max(x, Number.realPart(Number.abs y))
        in  foldl accum RNumber.zero a
        end
    fun norm1 a =
        let fun accum (y,x) = RNumber.+(x, Number.realPart(Number.abs y))
        in  foldl accum RNumber.zero a
        end
    fun norm2 a =
        let fun accum (y,x) = RNumber.+(x, Number.abs2 y)
        in RNumber.sqrt(foldl accum RNumber.zero a)
        end
end (* CTensor *)


structure RTensorSlice =
    struct
        structure Tensor = RTensor
        structure Index  = Tensor.Index
        structure Array  = Tensor.Array
        structure Range  = Range

        type elem = Array.elem
        type index = Tensor.Index.t
        type range = Range.t
        type tensor = RTensor.tensor

        type slice = {range : range, shapes: index list, tensor : tensor}

        fun fromto (lo,up,tensor) =
            let val r = Range.fromto (Tensor.shape tensor) (lo,up)
            in
                {range=r,
                 shapes=(Range.shapes r),
                 tensor=tensor}
            end

        fun slice (rs,tensor) =
            let 
                val r = (Range.ranges (Tensor.shape tensor) rs)
            in
                {range=r,
                 shapes=(Range.shapes r),
                 tensor=tensor}
            end

        fun length ({range, shapes, tensor})  = Range.length range
        fun base ({range, shapes, tensor})    = tensor
        fun shapes ({range, shapes, tensor}) = shapes
        fun range ({range, shapes, tensor})   = range

        fun map f slice = 
        let
           val te    = base slice
           val ra    = range slice
           val fndx  = Range.first ra
           val len   = length (slice)
           val arr   = Array.array(len, f (Tensor.sub(te,fndx)))
           val i     = ref 0
        in 
           
           Range.iteri (fn (ndx) => 
                         let val v = f (Tensor.sub (te,ndx)) in (Array.update (arr, !i, v); i := (!i + 1); true) end) ra;
           RTensor.fromArray ([1,len], arr)
        end

        fun app f (slice: slice) = 
        let
           val te    = base slice
           val ra    = range slice
           val fndx  = Range.first ra
        in 
           Range.iteri (fn (ndx) => (f (Tensor.sub (te,ndx)); true)) ra; ()
        end

        fun map2 f (sl1: slice) (sl2: slice) = 
        let
           val _      = if not ((shapes sl1) = (shapes sl2)) then raise Index.Shape else ()
           val te1    = base sl1
           val te2    = base sl2
           val ra1    = range sl1
           val ra2    = range sl2
           val len    = length sl1
           val fndx1  = Range.first ra1
           val fndx2  = Range.first ra2
           val arr    = Array.array (length(sl1), f (Tensor.sub(te1,fndx1), Tensor.sub(te2,fndx2)))
           val i      = ref 0
        in 
           Range.iteri2 (fn (ndx,ndx') => 
                            let 
                                val v = f (Tensor.sub (te1,ndx),Tensor.sub (te2,ndx')) 
                            in 
                                (Array.update (arr, !i, v); i := (!i + 1); true) 
                            end)
                        (ra1,ra2);
           RTensor.fromArray ([1,len], arr)
        end

        fun foldl f init (slice: slice) = 
        let
           val te     = base slice
           val sh     = Tensor.shape te
           val arr    = Tensor.toArray te
           val ra     = range slice
        in 
            Range.foldi_range
                (fn ((i,j),ax) => 
                    Loop.foldi (Index.toInt sh i, (Index.toInt sh j)+1,
                             fn (n,ax) => f (Array.sub (arr,n),ax), 
                                ax))
                init ra
        end


    end                                


structure TensorFile =
struct

type file = TextIO.instream

exception Data

fun assert NONE = raise Data
  | assert (SOME a) = a

(* ------------------ INPUT --------------------- *)

fun intRead file = assert(TextIO.scanStream INumber.scan file)
fun realRead file = assert(TextIO.scanStream RNumber.scan file)
fun complexRead file = assert(TextIO.scanStream CNumber.scan file)

fun listRead eltScan file =
    let val length = intRead file
        fun eltRead file = assert(TextIO.scanStream eltScan file)
        fun loop (0,accum) = accum
          | loop (i,accum) = loop(i-1, eltRead file :: accum)
    in
        if length < 0
        then raise Data
        else List.rev(loop(length,[]))
    end

fun intListRead file = listRead INumber.scan file
fun realListRead file = listRead RNumber.scan file
fun complexListRead file = listRead CNumber.scan file

fun intTensorRead file =
    let val shape = intListRead file
        val length = Index.length shape
        val first = intRead file
        val a = ITensor.Array.array(length, first)
        fun loop 0 = ITensor.fromArray(shape, a)
          | loop j = (ITensor.Array.update(a, length-j, intRead file);
                      loop (j-1))
    in loop (length - 1)
    end

fun realTensorRead file =
    let val shape = intListRead file
        val length = Index.length shape
        val first = realRead file
        val a = RTensor.Array.array(length, first)
        fun loop 0 = RTensor.fromArray(shape, a)
          | loop j = (RTensor.Array.update(a, length-j, realRead file);
                      loop (j-1))
    in loop (length - 1)
    end

fun complexTensorRead file =
    let val shape = intListRead file
        val length = Index.length shape
        val first = complexRead file
        val a = CTensor.Array.array(length, first)
        fun loop j = if j = length
                     then CTensor.fromArray(shape, a)
                     else (CTensor.Array.update(a, j, complexRead file);
                           loop (j+1))
    in loop 1
    end

(* ------------------ OUTPUT -------------------- *)
fun putStrLn (file, str) = 
    (TextIO.output (file, str);
     TextIO.output (file, "\n"))
    
fun putStr (file, str) = 
    (TextIO.output (file, str))

fun intWrite file x = putStrLn(file, INumber.toString x)
fun realWrite file x = putStrLn(file, RNumber.toString x)
fun complexWrite file x =
    let val (r,i) = CNumber.split x
    in putStrLn(file, concat [RNumber.toString r, " ", RNumber.toString i])
    end

fun listWrite converter file x =
    (intWrite file (length x);
     List.app (fn x => (putStrLn(file, converter x))) x)

fun listLineWrite converter file x =
    (List.app (fn x => (TextIO.output(file, " "^(converter x)))) x)

fun intListLineWrite file x = (listLineWrite INumber.toString file x; TextIO.output(file, "\n"))
fun realListLineWrite file x = (listLineWrite RNumber.toString file x; TextIO.output(file, "\n"))
fun complexListLineWrite file x = (listLineWrite CNumber.toString file x; TextIO.output(file, "\n"))

fun intListWrite file x = listWrite INumber.toString file x
fun realListWrite file x = listWrite RNumber.toString file x
fun complexListWrite file x = listWrite CNumber.toString file x

fun intArrayWrite file x = (IntArray.app (fn x => (intWrite file x)) x)

fun intTensorWrite file x = (intListWrite file (ITensor.shape x); ITensor.app (fn x => (intWrite file x)) x)
fun realTensorWrite file x = (intListWrite file (RTensor.shape x); RTensor.app (fn x => (realWrite file x)) x)
fun complexTensorWrite file x = (intListWrite file (CTensor.shape x); CTensor.app (fn x => (complexWrite file x)) x)

fun intTensorLineWrite file x = (TextIO.output (file, "["); listLineWrite INumber.toString file (ITensor.shape x); TextIO.output (file, " ]"); 
                                 ITensor.app (fn x => (TextIO.output (file, (" " ^ (INumber.toString x))))) x;
                                 putStrLn (TextIO.stdOut, ""))
fun realTensorLineWrite file x = (TextIO.output (file, "["); listLineWrite INumber.toString file (RTensor.shape x); TextIO.output (file, " ]"); 
                                  RTensor.app (fn x => (TextIO.output (file, (" " ^ (RNumber.toString x))))) x;
                                  putStrLn (TextIO.stdOut, ""))
fun complexTensorLineWrite file x = (TextIO.output (file, "["); listLineWrite INumber.toString file (CTensor.shape x); TextIO.output (file, " ]"); 
                                     CTensor.app (fn x => (TextIO.output (file, (" " ^ (CNumber.toString x))))) x)

fun realTensorSliceWrite file x = 
    (List.app (fn (l) => intListWrite file l) (RTensorSlice.shapes x); RTensorSlice.app (fn x => (realWrite file x)) x)
fun realTensorSliceLineWrite file x = 
    (List.app (fn (l) => (TextIO.output (file, "["); listLineWrite INumber.toString file l; TextIO.output (file, " ]"))) (RTensorSlice.shapes x); 
     RTensorSlice.app (fn x => (TextIO.output (file, (" " ^ (RNumber.toString x))))) x;
     putStrLn (TextIO.stdOut, ""))

end
