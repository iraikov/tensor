
(*
 Copyright 2013 Ivan Raikov.
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

*)



fun putStr out str = 
    TextIO.output (out, str)

fun putStrLn out str = 
    (TextIO.output (out, str);
     TextIO.output (out, "\n"))



signature SPARSE_INDEX =
    sig
        type t
        type array = IntArray.array
        type nonzero = { indptr: array, indices: array }
	type indexer = t -> int option
        datatype storage = CSR | CSC

        exception Index
        exception Shape

        val order : storage
        val toInt : t -> nonzero -> t -> int option

        val inBounds : t -> t -> bool

        val app : t -> nonzero -> (t -> unit) -> unit

    end


(*
 MONO_SPARSE		- Signature -

 Monomorphic sparse matrices.

 structure Number : NUMBER
	Structure that describe type type of the elements of the
	matrix and their operations (+,*,etc)

 type elem = Number.t
	The type of the elements of the matrix

 structure Tensor : MONO_TENSOR
	Tensors of 'elem' type.

 fromTensor [[number,number,...]*]
	Builds a sparse matrix up from a tensor

 sub (matrix,row,column)
 update (matrix,row,column,value)
	Retrieves or sets an element from/to a sparse matrix

 map op matrix
 mapi op matrix
 app op matrix
 appi op matrix
	Maps or applies 'op' to the elements of a matrix. 'mapi'
	and 'appi' have the pecularity that 'op' receives the
	row and the column indices plust the value.

 matrix + matrix
 matrix - matrix
 matrix * matrix
 matrix / matrix
 ~ matrix
	Elementwise operations.
*)

signature MONO_SPARSE_MATRIX =
    sig
	structure Tensor : MONO_TENSOR
	structure Number : NUMBER
	structure Index : SPARSE_INDEX

        type index = Index.t
	type elem = Number.t
	type matrix

	exception Data and Shape

	val fromTensor : index -> (Tensor.tensor * (index option)) -> matrix
	val fromTensorList : index -> (Tensor.tensor * index) list -> matrix

	val shape : matrix -> index

	val sub : matrix * index -> elem
	val update : matrix * index * elem -> unit

	val slice : (matrix * int * int) -> (Tensor.tensor * Index.array) list

	val map : (elem -> elem) -> matrix -> matrix
	val app : (elem -> unit) -> matrix -> unit


(*
	val mapi : (index * elem -> elem) -> matrix -> matrix
	val appi : (index * elem -> unit) -> matrix -> unit
	val map2 : (elem * elem -> elem) -> matrix -> matrix -> matrix


	val + : matrix * matrix -> matrix
	val - : matrix * matrix -> matrix
	val * : matrix * matrix -> matrix
	val / : matrix * matrix -> matrix
	val ~ : matrix -> matrix
*)
    end



structure SparseIndex =
    struct

	type t = int list
        type array = IntArray.array
        type nonzero = { indptr: array, indices: array }
	type indexer = t -> int option
        datatype storage = CSR | CSC
                            
	exception Index
	exception Shape

	val order = CSC

	fun validShape shape = List.all (fn x => x > 0) shape
	fun validIndex index = List.all (fn x => x >= 0) index

        val sub = IntArray.sub

        fun findFromTo (i,v,s,e) =
            let fun loop (j) = 
                    if ((j >= s) andalso (j < e)) 
                    then (if (sub (v,j) = i) then SOME j else loop (j+1))
                    else NONE
            in
                loop s
            end
            
        fun inBounds shape index =
	    ListPair.all (fn (x,y) => (x >= 0) andalso (x < y))
	    (index, shape)


	fun toInt shape {indptr, indices} index  =
            let
                val nptr = IntArray.length indptr
                val nind = IntArray.length indices
            in
                case order of 
                    CSR => 
                    (case (index, shape) of
                         ([i,j],[s,rs]) => 
                         let
                             val s = sub (indptr, j)
                             val e = if (j < (nptr-1)) then sub (indptr,j+1) else nind
                         in
                             findFromTo (i, indices, s, e)
                         end
                       | ([],[]) => SOME 0
                       | (_,_)   => raise Index)
                  | CSC => 
                    (case (index, shape) of
                         ([i,j],[s,rs]) => 
                         if (i >= 0) andalso (i < s) 
                         then
                             (let
                                  val s = sub (indptr,i)
                                  val e = if (i < (nptr-1)) then sub (indptr,i+1) else nind
                              in
                                  findFromTo (j, indices, s, e)
                              end)
                         else raise Index
                       | ([],[]) => SOME 0
                       | (_,_)   => raise Index)
                    
            end

        fun app shape {indptr, indices}   =
            (case order of 
                 CSR => 
                 (let 
                      val ni = IntArray.length indices
                      val nj = IntArray.length indptr
                      fun iterator j f =
                          if (j < nj)
                          then (let 
                                    val jj  = sub (indptr, j)
                                    val jj' = sub (indptr, if (j < (nj-1)) then j+1 else ni-1)
                                in
                                    (List.app f (List.tabulate (jj'-jj, fn (jk) => [sub(indices,jj+jk),j]));
                                     iterator (j+1) f)
                                end)
                          else ()
                  in
                      iterator 0
                  end)
               | CSC =>
                 (let 
                      val nj = IntArray.length indices
                      val ni = IntArray.length indptr
                      fun iterator i f =
                          if (i < ni)
                          then (let 
                                    val ii  = sub (indptr, i)
                                    val ii' = sub (indptr, if (i < (ni-1)) then i+1 else nj-1)
                                in
                                    (List.app f (List.tabulate (ii'-ii, fn (ik) => [i,sub(indices,ii+ik)]));
                                     iterator (i+1) f)
                                end)
                          else ()
                  in
                      iterator 0
                  end)
            )

    end



structure SparseMatrix : MONO_SPARSE_MATRIX =

struct
    structure Tensor : MONO_TENSOR = RTensor
    structure Number = RTensor.Number
    structure Index = SparseIndex

    type index   = Index.t
    type nonzero = Index.nonzero
    type elem    = Number.t
    type block   = {offset: index, shape: index, nz: nonzero, data: elem array}
    type matrix  = {shape: index, blocks: block list}

    exception Data
    exception Shape
    exception Index
    exception Overlap

    (* --- LOCALS --- *)

    fun dimVals [m,n] = (m,n) | dimVals _ = raise Shape

    fun array_map f a =
	let fun apply index = f(Array.sub(a,index)) in
	    Array.tabulate(Array.length a, apply)
	end

    fun array_mapi f a =
	let fun apply index = f(index,Array.sub(a,index)) in
	    Array.tabulate(Array.length a, apply)
	end

    fun findBlock (i,j,blocks) =
        let
            val block = List.find
                            (fn ({offset=offset, shape=shape, nz, data}) =>
                                let
                                    val (u,v) = dimVals offset
                                    val (t,s) = dimVals shape
                                in
                                    ((j>=v) andalso (j-v<s) andalso
                                     (i>=u) andalso (i-u<t))
                                end)
                            blocks
        in
            block
        end

    fun intArrayWrite file x = Array.app (fn (i) => putStr file ((Int.toString i) ^ " ")) x

    (* --- CONSTRUCTORS --- *)

    fun fromTensor shape (a: Tensor.tensor, offset) = 
        (let 
             val shape_a = Tensor.shape a 
             val (rows,cols) = dimVals shape_a
        in
            case Index.order of
                Index.CSC =>
                let 
                    val v0: (int * elem) list = []
	            val data: (((int * elem) DynArray.array) option) Array.array  = 
                        Array.array(cols,NONE)
                    val nzcount = ref 0
                    val _ = Tensor.Index.app (List.rev shape_a)
                                      (fn (i) => 
                                          let 
                                              val v = Tensor.sub (a, i)
                                          in
                                              if not (Number.== (v, Number.zero))
                                              then
                                                  let 
                                                      val (irow,icol) = dimVals i
                                                      val colv  = Array.sub (data, icol)
                                                      (*val col' = (irow,v) :: col*)
                                                  in
                                                      (case colv of
                                                          SOME col => 
                                                          (DynArray.update(col,DynArray.length col,(irow,v)))
                                                        | NONE => Array.update (data, icol, SOME (DynArray.fromList [(irow,v)]));
                                                       nzcount := (!nzcount) + 1)
                                                  end
                                              else ()
                                          end)
                    val data'   = Array.array (!nzcount, Number.zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (cols, 0)
                    val update  = IntArray.update
                    val fi      = Array.foldli
                                      (fn (n,SOME cols,i) => 
                                          let 
                                              val i' = DynArray.foldr
                                                           (fn ((rowind,v),i) => 
                                                               (Array.update (data',i,v); 
                                                                update (indices,i,rowind); 
                                                                i+1))
                                                           i cols
                                          in
                                              (update (indptr,n,i); i')
                                          end
                                      | (n,NONE,i) => (update (indptr,n,i); i))
                                      0 data
                in
                    {shape=shape,
                     blocks=[{offset=case offset of NONE => [0, 0] | SOME i => i, 
                              shape=shape_a, nz={ indptr= indptr, indices=indices }, data=data'}]}
                end
              | Index.CSR => 
                let 
                    val v0: (int * elem) list = []
	            val data: (((int * elem) DynArray.array) option) Array.array  = 
                        Array.array(rows,NONE)
                    val nzcount = ref 0
                    val _ = Tensor.Index.app shape_a
                                              (fn (i) => 
                                                  let 
                                                      val v = Tensor.sub (a, i)
                                                  in
                                                      if not (Number.== (v, Number.zero))
                                                      then
                                                          let 
                                                              val (irow,icol) = dimVals i
                                                              val rowv  = Array.sub (data, irow)
                                                              (*val row' = (icol,v) :: row*)
                                                          in
                                                              (case rowv of
                                                                   (*Array.update(data,irow,row');*)
                                                                   SOME row => DynArray.update (row,DynArray.length row,(icol,v))
                                                                 | NONE => Array.update (data, irow, SOME (DynArray.fromList [(icol,v)]));
                                                               nzcount := (!nzcount) + 1)
                                                          end
                                                      else ()
                                                  end)
                    val data'   = Array.array (!nzcount, Number.zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (rows, 0)
                    val update  = IntArray.update
                    val fi      = Array.foldli
                                      (fn (n,SOME rows,i) => 
                                          let 
                                              val i' = DynArray.foldr 
                                                           (fn ((colind,v),i) => 
                                                               (Array.update (data',i,v); 
                                                                update (indices,i,colind); 
                                                                i+1))
                                                           i rows
                                          in
                                              (update (indptr,n,i); i')
                                          end
                                      | (n,NONE,i) => (update (indptr,n,i); i))
                                      0 data
                in
                    {shape=shape, 
                     blocks=[{offset = case offset of NONE => [0,0] | SOME i => i, 
                              shape=shape_a, nz={ indptr= indptr, indices=indices }, data=data'}]}
                end
        end)

        fun insert ({shape, blocks},t,toffset) =
            let
                val (i,j) = dimVals toffset
                val {shape=_, blocks=bs} = fromTensor shape (t,SOME [i,j])
                val b': block = case bs of
                                    [b] => b
                                  | _ => raise Match
                val (m,n) = dimVals (#shape b')

                val blocks' = 
                    let
                        fun merge ([], []) = [b']
                          | merge ((b as {offset=offset,shape=shape,nz,data})::rst, ax) =
                            let 
                                val (bm,bn) = dimVals shape
                                val (bi,bj) = dimVals offset
                            in
                                if (j < bj)
                                then List.rev (rst@(b::b'::ax))
                                else (if (i >= bi) andalso (j >= bj) andalso 
                                         (i+m <= bi+bm) andalso (j+n <= bj+bn)
                                      then raise Overlap
                                      else merge (rst, b::ax))
                            end
                            | merge ([], ax) = List.rev (b'::ax)
                    in
                        merge (blocks, [])
                    end
            in
                {shape=shape, blocks=blocks'}
            end

                  
    (* Builds a sparse matrix from a list of the form:
  
       ([xoffset,yoffset],tensor) ...

     where xoffset and yoffset are the positions where to insert the
     given tensor. The tensors to be inserted must be non-overlapping.
    *)

    fun fromTensorList shape (al: (Tensor.tensor * index) list) = 
        (case al of
             (a,i)::rst => 
             (List.foldl (fn ((a,i),ax) => insert (ax,a,i))
                         (fromTensor shape (a, SOME i))
                         rst)
           | _ => raise Match)


    (* --- ACCESSORS --- *)

    fun shape {shape, blocks} = shape

    fun sub ({shape, blocks},index) =
        let
            val (i,j) = dimVals index
            val block = findBlock (i,j,blocks)
        in
            case block of
                SOME ({offset, shape, nz, data}) => 
                let 
                    val (m,n) = dimVals offset
                    val p = Index.toInt shape nz [i-m,j-n]
                 in
                     case p of SOME p' => Array.sub (data, p')
                             | NONE => Number.zero
                 end
              | NONE => Number.zero
        end

    fun update ({shape,blocks},index,new) =
        let
            val (i,j) = dimVals index
            val block = findBlock (i,j,blocks)
        in
            case block of
                SOME ({offset, shape, nz, data}) => 
                let
                    val (m,n) = dimVals shape
                    val p     = Index.toInt shape nz [i-m,j-n]
                in
                    case p of SOME p' => Array.update (data, p', new) | NONE => ()
                end
              | NONE => ()
        end

    fun findBlocks (i,axis,blocks) =
        let
            val blocks' = List.mapPartial
                            (fn (b as {offset=offset, shape=shape, nz, data}) =>
                                let
                                    val (u,v) = dimVals offset
                                    val (t,s) = dimVals shape
                                in
                                    (case axis of
                                         1 => if ((i>=v) andalso (i-v<s)) then SOME b else NONE
                                       | 0 => if ((i>=u) andalso (i-u<t)) then SOME b else NONE
                                       | _ => raise Match)
                                end)
                            blocks
        in
            blocks'
        end

    fun slice ({shape,blocks},axis,i) =
        let
            val (m,n) = dimVals shape

            val _ = case axis of
                        0 => if (i > m) then raise Index else ()
                      | 1 => if (i > n) then raise Index else ()
                      | _ => raise Data
                                                              
        in
            List.mapPartial
            (fn ({offset=offset, shape=shape, nz={indptr, indices}, data})  =>
                let 
                    val (u,v) = dimVals offset
                    val (m,n) = dimVals shape

                    val i'  = case axis of 1 => i-v | 0 => i-u | _ => raise Match
                in
                (case (Index.order,axis) of
                     (Index.CSC,1) => (let 
                                           val s   = IntArray.sub (indptr, i')
                                           val e   = (if i' < n-1
                                                      then IntArray.sub (indptr, i'+1) else Array.length data)
                                           val len = e-s
                                           val res = RNumberArray.array (len, Number.zero)
                                           val rsi = IntArray.array (len, 0)
                                           fun loop (i,n) = if i < e 
                                                            then (RNumberArray.update (res,n,Array.sub (data,i));
                                                                  IntArray.update (rsi,i-s,Index.sub (indices,i));
                                                                  loop (i+1,n+1))
                                                            else ()
                                       in
                                           loop (s,0);
                                           if len > 0 then SOME (RTensor.fromArray ([1,len],res),rsi) else NONE
                                       end)
                   | (Index.CSR,0) => (let val s   = IntArray.sub (indptr, i')
                                           val e   = (if i'< (m-1) 
                                                      then IntArray.sub (indptr, i'+1) else Array.length data)
                                           val len = e-s
                                           val res = RNumberArray.array (len, Number.zero)
                                           val rsi = IntArray.array (len, 0)
                                           fun loop (i,n) = if i < e 
                                                            then (RNumberArray.update (res,n,Array.sub (data,i));
                                                                  IntArray.update (rsi,i-s,Index.sub (indices,i));
                                                                  loop (i+1,n+1))
                                                            else ()
                                       in
                                           loop (s,0);
                                           if len > 0 then SOME (RTensor.fromArray ([1,len],res),rsi) else NONE
                                       end)
                   | (Index.CSC,0) => (let val vs = IntArray.foldri
                                                        (fn (n,ii,ax) =>  if ii=i then (Array.sub(data,n),ii)::ax else ax)
                                                        [] indices
                                           val len = List.length vs
                                       in
                                           if len > 0 
                                           then SOME (RTensor.fromList ([1,len],map #1 vs),
                                                      IntArray.fromList (map #2 vs)) 
                                           else NONE
                                       end)
                   | (Index.CSR,1) => (let val vs = IntArray.foldri
                                                        (fn (n,ii,ax) =>  if ii=i then (Array.sub(data,n),ii)::ax else ax)
                                                        [] indices
                                           val len = List.length vs
                                       in
                                           if len > 0 then SOME (RTensor.fromList ([1,len],map #1 vs),
                                                                 IntArray.fromList (map #2 vs)) else NONE
                                       end)
                   | (_,_) => raise Index)
                end)
            (findBlocks (i,axis,blocks) )
            
                                    
        end


    (* --- MAPPING --- *)

    fun map f {shape, blocks} =
        {shape=shape,
         blocks=(List.map 
                     (fn {offset, shape, nz, data} =>
                         {offset=offset, shape=shape, nz=nz, data=array_map f data})
                     blocks)}

    fun app f {shape, blocks} = 
        List.app (fn {offset, shape, nz, data} => Array.app f data) blocks


    (* --- BINOPS --- *)
(*
    fun a + b = map2 Number.+ a b
    fun a * b = map2 Number.* a b
    fun a - b = map2 Number.- a b
    fun a / b = map2 Number./ a b
    fun ~ a = map Number.~ a
*)
end



signature MONO_SPARSE_TENSOR =
    sig
	structure Tensor : MONO_TENSOR
	structure TensorSlice : MONO_TENSOR_SLICE
	structure Number : NUMBER
	structure Index  : INDEX
	structure Range  : RANGE

        type index = Index.t
	type elem = Number.t
	type tensor

	exception Shape

        val new: index * elem -> tensor

	val shape  : tensor -> index
        val length : tensor -> int
        val rank   : tensor -> int

	val sub     : tensor * index -> elem
	val update : tensor * index * elem -> unit
        val insert :  tensor * Tensor.tensor * index -> tensor

	val map : (elem -> elem) -> tensor -> Tensor.tensor list
	val app : (elem -> unit) -> tensor -> unit

    end


structure SparseTensor : MONO_SPARSE_TENSOR =

struct
    structure Tensor : MONO_TENSOR = RTensor
    structure TensorSlice : MONO_TENSOR_SLICE = RTensorSlice
    structure Number = RTensor.Number
    structure Index  = Index
    structure Range  = Range

    type index   = Index.t
    type elem    = Number.t
    type range   = Range.t
    type block   = {r: range, data: Tensor.tensor}
    type tensor  = {shape: index, nz: block list, default: elem}

    exception Shape
    exception Index
    exception Overlap

    fun new (shape, init) =
        if not (Index.validShape shape) then
            raise Shape
        else
            {shape = shape, nz=[], default=init}

    fun default {shape, nz, default} = default
                
    fun length ({shape, nz, default}: tensor) =  
        List.foldl (fn (b,ax) => (Tensor.length (#data b))+ax) 0 nz
              
    fun shape {shape, nz, default} = shape

    fun rank t = List.length (shape t)

    fun whichBlock ({shape, nz, default}: tensor) i =
        List.find (fn (b) => Range.inRange (#r b) i) nz

    fun sub (t as {shape, nz, default}, i) = 
        case whichBlock t i of
            SOME b => 
            let
                val i' = Index.-(i, Range.first (#r b)) 
            in 
                Tensor.sub ((#data b), i')
            end
          | NONE => default

    fun update (t as {shape, nz, default}, i, v) = 
        case whichBlock t i of
            SOME b => 
            let
                val i' = Index.-(i, Range.first (#r b)) 
            in Tensor.update ((#data b), i', v) end
          | NONE => raise Index
                             
    fun insert (x: tensor, y: Tensor.tensor, i) =
        if not (rank x = Tensor.rank y) 
        then raise Shape
        else 
            (let
                val xshape  = shape x
                val yshape  = Tensor.shape y
                val nzx     = #nz x
                val j       = Index.decr (Index.+ (i,yshape))
                val r       = Range.fromto xshape (i,j)
                val b'      = {r=r,data=y}

                val nzx'    = 
                    let
                        fun merge ([], []) = [b']
                          | merge (b::rst, ax) =
                            let 
                                val bi = Range.first (#r b)
                                val bj = Range.last (#r b)
                            in
                                if Index.< (j, bi)
                                then List.rev (rst@(b::b'::ax))
                                else (if Index.>= (i, bi) andalso Index.<= (j, bj)
                                      then raise Overlap
                                      else merge (rst, b::ax))
                            end
                            | merge ([], ax) = List.rev (b'::ax)
                    in
                        merge (nzx, [])
                    end
            in
                {shape=xshape, nz=nzx', default=(#default x)}
            end)

    fun map f ({shape, nz, default}: tensor) =
        List.map (fn (b) => Tensor.map f (#data b)) nz
                 
    fun app f ({shape, nz, default}: tensor) =
        List.app (fn (b) => Tensor.app f (#data b)) nz
                
end
