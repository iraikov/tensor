
(*
 Copyright 2013-2015 Ivan Raikov.
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

        val eq : t * t -> bool

        val app : t -> nonzero -> (t -> unit) -> unit
        val iteri : t -> nonzero -> (t * int -> unit) -> unit

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

 fromTensor  [[number,number,...]*]
 fromTensor' [[number,number,...]*]
	Builds a sparse matrix up from a tensor.
        fromTensor converts the tensor to sparse format;
        fromTensor' uses it as-is

 fromTensorList  [[number,number,...]*]
	Builds a sparse matrix up from a list of tensors.

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
 *> n matrix
	Elementwise operations.
*)

signature MONO_SPARSE_MATRIX =
    sig
	structure Tensor : MONO_TENSOR
	structure TensorSlice : MONO_TENSOR_SLICE
	structure Number : NUMBER
	structure Index : SPARSE_INDEX
        structure Map : ORD_MAP

        type index = Index.t
        type storage = Index.storage
	type elem = Number.t

	type matrix

	exception Data and Shape

	val zero : Number.t

	val fromList : index -> ((int * (int * elem) list) list * index * (index option)) -> matrix
	val fromLists : index -> ({l: (int * (int * elem) list) list, shape_l: index, offset: index} list) -> matrix
	val fromVector : index -> ((int * int * elem) vector) * index * (index option) -> matrix
	val fromVectors : index -> ({v: ((int * int * elem) vector), shape_v: index, offset: index} list) -> matrix
	val fromTensor : index -> (Tensor.tensor * (index option)) -> matrix
	val fromTensorList : index -> {tensor: Tensor.tensor, offset: index, sparse: bool} list -> matrix
	val fromGenerator : index -> ((index -> elem) * index * (index option)) -> matrix
	val fromGeneratorList : index -> ({f: (index -> elem), fshape: index, offset: index} list) -> matrix
	val fromMapGenerator : index -> ((int -> elem Map.map) * storage * index * (index option)) -> matrix
        val fromMapGeneratorList : index -> ({f: int -> elem Map.map, forder: storage, fshape: index, offset: index} list) -> matrix
        val insert : matrix * matrix -> matrix

	val shape : matrix -> index

	val sub : matrix * index -> elem
	val sub' : matrix * index -> elem option
	val update : matrix * index * elem -> unit

	val map : (elem -> elem) -> matrix -> matrix
	val app : (elem -> unit) -> matrix -> unit
	val appi : (index * elem -> unit) -> matrix -> unit

        val *> : elem -> matrix -> matrix

        datatype slice = SLSPARSE of {offset: index, indices: Index.array, data: Tensor.tensor}
                       | SLDENSE  of {offset: index, data: TensorSlice.slice}

	val slice : (matrix * int * int) ->  slice list
        val sliceAppi: ((int * elem) -> unit) -> slice list -> unit
        val sliceFoldi: ((int * elem * 'a) -> 'a) -> 'a -> slice list -> 'a

                                                                         
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

	fun compare ([],[]) = EQUAL
	  | compare (_, []) = raise Index
	  | compare ([],_) = raise Index
	  | compare (a::ra, b::rb) =
	    case Int.compare (a,b) of
		EQUAL => compare (ra,rb)
	      | LESS => LESS
	      | GREATER => GREATER

	fun eq (a, b) = compare(a,b) = EQUAL
                                           
        val sub = Unsafe.IntArray.sub

        fun findFromTo (i,v,s,e) = 
            Loop.find (s,e-1,fn(j) => if (sub (v,j) = i) then SOME j else NONE)
            
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
                             val s = sub (indptr, i)
                             val e = if (i < (nptr-1)) then sub (indptr,i+1) else nind
                         in
                             findFromTo (j, indices, s, e)
                         end
                       | ([],[]) => SOME 0
                       | (_,_)   => raise Index)
                  | CSC => 
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
                    
            end

        fun iteri shape {indptr, indices} f  =
            (case order of 
                 CSC => 
                 (let 
                      val ni = IntArray.length indices
                      val nj = IntArray.length indptr
                  in
                      Loop.app (0, nj,
                                fn (j) =>
                                   (let 
                                        val jj  = sub (indptr, j)
                                        val jj' = if (j < (nj-1)) then sub (indptr, j+1) else ni
                                    in
                                        List.app f (List.tabulate 
                                                     (jj'-jj, 
                                                      fn (jk) => 
                                                         let val n = jj+jk 
                                                         in 
                                                             ([sub(indices,n),j],n)
                                                         end))
                                end))
                  end)
               | CSR =>
                 (let 
                      val nj = IntArray.length indices
                      val ni = IntArray.length indptr
                  in
                      Loop.app (0, ni,
                                fn (i) =>
                                   (let 
                                        val ii  = sub (indptr, i)
                                        val ii' = if (i < (ni-1)) then sub (indptr, i+1) else nj
                                    in
                                        List.app f (List.tabulate
                                                         (ii'-ii, 
                                                          fn (ik) => 
                                                             let 
                                                                 val n = ii+ik
                                                             in
                                                                 ([i,sub(indices,n)],n)
                                                             end))
                                    end))
                  end)
            )

        fun app shape nz f = iteri shape nz (fn (index,i) => f index)
    end



structure SparseMatrix : MONO_SPARSE_MATRIX =

struct
    structure Tensor : MONO_TENSOR = RTensor
    structure TensorSlice : MONO_TENSOR_SLICE = RTensorSlice
    structure Number = RTensor.Number
    structure Index = SparseIndex
    structure Map = IntMap

    type index   = Index.t
    type storage = Index.storage
    type nonzero = Index.nonzero
    type elem    = Number.t

    val zero     = Number.zero

    datatype block = 
             SPARSE of {offset: index, shape: index, nz: nonzero, data: Tensor.Array.array}
           | DENSE of {offset: index, data: Tensor.tensor}

    type matrix  = {shape: index, blocks: block list}

    datatype slice = SLSPARSE of {offset: index, indices: Index.array, data: Tensor.tensor}
                   | SLDENSE  of {offset: index, data: TensorSlice.slice}

    exception Data
    exception Shape
    exception Index
    exception Overlap

    (* --- LOCALS --- *)

    fun dimVals [m,n] = (m,n) | dimVals _ = raise Shape

    fun array_map f a =
	let fun apply index = f(Tensor.Array.sub(a,index)) in
	    Tensor.Array.tabulate(Tensor.Array.length a, apply)
	end


    fun findBlock (i,j,blocks) =
        let
            val block = List.find
                            (fn (SPARSE {offset=offset, shape=shape, nz, data}) =>
                                (let
                                    val (u,v) = dimVals offset
                                    val (t,s) = dimVals shape
                                in
                                    ((j>=v) andalso (j-v<s) andalso
                                     (i>=u) andalso (i-u<t))
                                end)
                                | (DENSE {offset=offset, data}) =>
                                (let
                                    val (u,v) = dimVals offset
                                    val (t,s) = dimVals (Tensor.shape data)
                                in
                                    ((j>=v) andalso (j-v<s) andalso
                                     (i>=u) andalso (i-u<t))
                                end))
                            blocks
        in
            block
        end


    fun buildSparseFromArrayList (data, data', indptr, indices) =
        let
            val update  = IntArray.update
        in
            Array.foldli
                (fn (n,SOME cols,i) => 
                    let 
                        

                        val i' = List.foldr
                                     (fn ((vind,v),i) => 
                                         (Tensor.Array.update (data',i,v); 
                                          update (indices,i,vind); 
                                          i+1))
                                     i cols
                    in
                        (update (indptr,n,i); i')
                    end
                | (n,NONE,i) => (update (indptr,n,i); i))
                0 data
        end

    fun buildSparseFromArray (data, data', indptr, indices) =
        let
            val update  = IntArray.update
        in
            Array.foldli
                (fn (n,SOME cols,i) => 
                    let 
                        val i' = DynArray.foldr
                                     (fn ((vind,v),i) => 
                                         (Tensor.Array.update (data',i,v); 
                                          update (indices,i,vind); 
                                          i+1))
                                     i cols
                    in
                        (update (indptr,n,i); i')
                    end
                | (n,NONE,i) => (update (indptr,n,i); i))
                0 data
        end

    fun buildSparseFromMap (data, data', indptr, indices) =
        let
            val update  = IntArray.update
        in
            Array.foldli
                (fn (n,SOME cols,i) => 
                    let 
                        val i' = IntMap.foldri
                                     (fn (vind,v,i) => 
                                         (Tensor.Array.update (data',i,v); 
                                          update (indices,i,vind); 
                                          i+1))
                                     i cols
                    in
                        (update (indptr,n,i); i')
                    end
                | (n,NONE,i) => (update (indptr,n,i); i))
                0 data
        end

    (* --- CONSTRUCTORS --- *)

    fun fromList shape (a, shape_a, offset) = 
        (let 
             val len_a = List.length a
             val (rows,cols) = dimVals shape_a
        in
            case Index.order of
                Index.CSC =>
                let 
	            val data: (((int * elem) list) option) Array.array = Array.array(cols,NONE)
                    val nzcount = ref 0
                    val _ = List.app 
                                (fn (icol,lst) => 
                                    (let 
                                        val colv = Unsafe.Array.sub (data, icol)
                                    in 
                                        (case colv of
                                             SOME col => Array.update (data, icol, SOME (lst @ col))
                                           | NONE => (Array.update (data, icol, SOME lst));
                                         nzcount := (!nzcount) + (List.length lst))
                                    end))
                                a
                    val data'   = Tensor.Array.array (!nzcount, zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (cols, 0)
                    val update  = IntArray.update
                    val fi      = buildSparseFromArrayList (data, data', indptr, indices)
                in
                    {shape=shape,
                     blocks=[SPARSE {offset=case offset of NONE => [0, 0] | SOME i => i, 
                                     shape=shape_a, nz={ indptr= indptr, indices=indices }, 
                                     data=data'}]}
                end
              | Index.CSR => 
                let 
	            val data: (((int * elem) list) option) Array.array  = Array.array(rows,NONE)
                    val nzcount = ref 0
                    val _ = List.app 
                                (fn (irow,lst) => 
                                    (let 
                                        val rowv = Unsafe.Array.sub (data, irow)
                                    in 
                                        (case rowv of
                                             SOME row => (Array.update (data, irow, SOME (lst @ row)))
                                           | NONE => (Array.update (data, irow, SOME lst));
                                         nzcount := (!nzcount) + (List.length lst))
                                    end)) a
                    val data'   = Tensor.Array.array (!nzcount, zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (rows, 0)
                    val update  = IntArray.update
                    val fi      = buildSparseFromArrayList (data, data', indptr, indices)
                in
                    {shape=shape, 
                     blocks=[SPARSE {offset = case offset of NONE => [0,0] | SOME i => i, 
                                     shape=shape_a, nz={ indptr= indptr, indices=indices }, data=data'}]}
                end
        end)


    fun fromVector shape (a, shape_a, offset) = 
        (let 
             val len_a = Vector.length a
             val (rows,cols) = dimVals shape_a
        in
            case Index.order of
                Index.CSC =>
                let 
	            val data: (((int * elem) list) option) Array.array = Array.array(cols,NONE)
                    val nzcount = ref 0
                    val _ = Vector.app 
                                (fn (irow,icol,v) => 
                                    (let 
                                        val colv = Unsafe.Array.sub (data, icol)
                                    in 
                                        (case colv of
                                             SOME col => Array.update (data, icol, SOME ((irow,v) :: col))
                                           | NONE => (Array.update (data, icol, SOME [(irow,v)]));
                                         nzcount := (!nzcount) + 1)
                                    end))
                                a
                    val data'   = Tensor.Array.array (!nzcount, zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (cols, 0)
                    val update  = IntArray.update
                    val fi      = buildSparseFromArrayList (data, data', indptr, indices)
                in
                    {shape=shape,
                     blocks=[SPARSE {offset=case offset of NONE => [0, 0] | SOME i => i, 
                                     shape=shape_a, nz={ indptr= indptr, indices=indices }, 
                                     data=data'}]}
                end
              | Index.CSR => 
                let 
	            val data: (((int * elem) list) option) Array.array  = Array.array(rows,NONE)
                    val nzcount = ref 0
                    val _ = Vector.app 
                                (fn (irow,icol,v) => 
                                    (let 
                                        val rowv = Unsafe.Array.sub (data, irow)
                                    in 
                                        (case rowv of
                                             SOME row => (Array.update (data, irow, SOME ((icol,v) :: row)))
                                           | NONE => (Array.update (data, irow, SOME [(icol,v)]));
                                         nzcount := (!nzcount) + 1)
                                    end)) a
                    val data'   = Tensor.Array.array (!nzcount, zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (rows, 0)
                    val update  = IntArray.update
                    val fi      = buildSparseFromArrayList (data, data', indptr, indices)
                in
                    {shape=shape, 
                     blocks=[SPARSE {offset = case offset of NONE => [0,0] | SOME i => i, 
                                     shape=shape_a, nz={ indptr= indptr, indices=indices }, data=data'}]}
                end
        end)

    fun fromTensor shape (a: Tensor.tensor, offset) = 
        (let 
             val shape_a = Tensor.shape a 
             val (rows,cols) = dimVals shape_a
        in
            case Index.order of
                Index.CSC =>
                let 
	            val data: (((int * elem) DynArray.array) option) Array.array  = 
                        Array.array(cols,NONE)
                    val nzcount = ref 0
                    val _ = Tensor.Index.app
                                (List.rev shape_a)
                                (fn (i) => 
                                    let 
                                        val v = Tensor.sub (a, i)
                                    in
                                        if not (Number.== (v, zero))
                                        then
                                            let 
                                                val (irow,icol) = dimVals i
                                                val colv  = Unsafe.Array.sub (data, icol)
                                            (*val col' = (irow,v) :: col*)
                                            in
                                                (case colv of
                                                     SOME col => (DynArray.update(col,DynArray.length col,(irow,v)))
                                                   | NONE => Array.update (data, icol, SOME (DynArray.fromList [(irow,v)]));
                                                 nzcount := (!nzcount) + 1)
                                            end
                                        else ()
                                    end)
                in
                    if (!nzcount) > 0
                    then (let
                             val data'   = Tensor.Array.array (!nzcount, zero)
                             val indices = IntArray.array (!nzcount, 0)
                             val indptr  = IntArray.array (cols, 0)
                             
                             val fi      = buildSparseFromArray (data, data', indptr, indices)
                         in
                             {shape=shape,
                              blocks=[SPARSE {offset=case offset of NONE => [0, 0] | SOME i => i, 
                                              shape=shape_a, nz={ indptr= indptr, indices=indices }, 
                                              data=data'}]}
                         end)
                    else {shape=shape, blocks=[]}
                end
              | Index.CSR => 
                let 
	            val data: (((int * elem) DynArray.array) option) Array.array  = 
                        Array.array(rows,NONE)
                    val nzcount = ref 0
                    val _ = 
                        Tensor.Index.app 
                            shape_a
                            (fn (i) => 
                                let 
                                    val v = Tensor.sub (a, i)
                                in
                                    if not (Number.== (v, zero))
                                    then
                                        let 
                                            val (irow,icol) = dimVals i
                                            val rowv  = Unsafe.Array.sub (data, irow)
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
                in
                    if (!nzcount) > 0
                    then (let
                             val data'   = Tensor.Array.array (!nzcount, zero)
                             val indices = IntArray.array (!nzcount, 0)
                             val indptr  = IntArray.array (rows, 0)
                             val fi      = buildSparseFromArray (data, data', indptr, indices)
                         in
                             {shape=shape, 
                              blocks=[SPARSE {offset = case offset of NONE => [0,0] | SOME i => i, 
                                              shape=shape_a, nz={ indptr= indptr, indices=indices }, data=data'}]}
                         end)
                    else {shape=shape, 
                              blocks=[]}
                end
        end)


    fun fromTensor' shape (a: Tensor.tensor, offset) = 
        let 
            val shape_a = Tensor.shape a 
            val (rows,cols) = dimVals shape_a
        in
            {shape=shape, blocks=[DENSE {offset=(case offset of NONE => [0, 0] | SOME i => i), 
                                         data=a}]}
        end


    fun fromGenerator shape (f: index -> elem, fshape, offset) = 
        (let 
             val (rows,cols) = dimVals fshape
        in
            case Index.order of
                Index.CSC =>
                let 
	            val data: (((int * elem) DynArray.array) option) Array.array  = 
                        Array.array(cols,NONE)
                    val nzcount = ref 0
                    val _ = Tensor.Index.app 
                                (List.rev fshape)
                                (fn (i) => 
                                    let 
                                        val v = f (i)
                                    in
                                        if not (Number.== (v, zero))
                                        then
                                            let 
                                                val (irow,icol) = dimVals i
                                                val colv  = Unsafe.Array.sub (data, icol)
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
                    val data'   = Tensor.Array.array (!nzcount, zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (cols, 0)
                    val fi      = buildSparseFromArray (data, data', indptr, indices)
                in
                    {shape=shape,
                     blocks=[SPARSE {offset=case offset of NONE => [0, 0] | SOME i => i, 
                                     shape=fshape, nz={ indptr= indptr, indices=indices }, data=data'}]}
                end
              | Index.CSR => 
                let 
	            val data: (((int * elem) DynArray.array) option) Array.array  = 
                        Array.array(rows,NONE)
                    val nzcount = ref 0
                    val _ = Tensor.Index.app 
                                fshape
                                (fn (i) => 
                                    let 
                                        val v = f (i)
                                    in
                                        if not (Number.== (v, zero))
                                        then
                                            let 
                                                val (irow,icol) = dimVals i
                                                val rowv  = Unsafe.Array.sub (data, irow)
                                            in
                                                (case rowv of
                                                     SOME row => DynArray.update (row,DynArray.length row,(icol,v))
                                                   | NONE => Array.update (data, irow, SOME (DynArray.fromList [(icol,v)]));
                                                 nzcount := (!nzcount) + 1)
                                            end
                                        else ()
                                    end)
                    val data'   = Tensor.Array.array (!nzcount, zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (rows, 0)
                    val fi      = buildSparseFromArray (data, data', indptr, indices)
                in
                    {shape=shape, 
                     blocks=[SPARSE {offset = case offset of NONE => [0,0] | SOME i => i, 
                                     shape=fshape, nz={ indptr= indptr, indices=indices }, data=data'}]}
                end
        end)


    fun fromMapGenerator shape (f: int -> elem Map.map, forder, fshape, offset) = 
        (let 
             val (rows,cols) = dimVals fshape
        in
            case Index.order of
                Index.CSC =>
                let 
                    val nzcount = ref 0
                    val columnData: ((elem Map.map) option) Array.array  = 
                        Array.array(cols,NONE)

                    val _  = 
                        case forder of 
                            Index.CSC => 
                            Loop.app
                                (0, cols,
                                 (fn (icol) => 
                                     let
                                         val m = f icol
                                     in
                                         if not (Map.isEmpty m)
                                         then (Array.update (columnData,icol,SOME m);
                                               nzcount := (!nzcount) + Map.numItems (m))
                                         else ()
                                     end))
                            | Index.CSR =>
                            Loop.app
                                (0, rows,
                                 (fn (irow) => 
                                     let
                                         val m = f irow
                                     in
                                         IntMap.appi
                                             (fn (colind,v) =>
                                                 case Unsafe.Array.sub (columnData,colind) of
                                                     SOME m' => 
                                                     (let val m'' = Map.insert(m',irow,v)
                                                      in
                                                          Array.update (columnData,colind,SOME m'');
                                                          nzcount := (!nzcount) + 1
                                                      end)
                                                   | NONE => (Array.update (columnData,colind,
                                                                            SOME (IntMap.singleton(irow,v)));
                                                              nzcount := (!nzcount) + 1))
                                             m
                                     end))

                    val data    = Tensor.Array.array (!nzcount, zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (cols, 0)
                    val fi      = buildSparseFromMap (columnData, data, indptr, indices)
                in
                    {shape=shape,
                     blocks=[SPARSE {offset=case offset of NONE => [0, 0] | SOME i => i, 
                                     shape=fshape, nz={ indptr= indptr, indices=indices }, 
                                     data=data}]}
                end
              | Index.CSR => 
                let 

                    val nzcount = ref 0
                    val rowData: ((elem Map.map) option) Array.array  = 
                        Array.array(cols,NONE)

                    val _  = 
                        case forder of 
                            Index.CSR => 
                            Loop.app
                            (0, rows,
                             (fn (irow) => 
                                 let
                                     val m = f irow
                                 in
                                     if not (Map.isEmpty m)
                                     then (Array.update (rowData,irow,SOME m);
                                           nzcount := (!nzcount) + Map.numItems (m))
                                     else ()
                                 end))
                            | Index.CSC =>
                              Loop.app
                                (0, cols,
                                 (fn (icol) => 
                                     let
                                         val m = f icol
                                     in
                                         IntMap.appi
                                             (fn (rowind,v) =>
                                                 case Unsafe.Array.sub (rowData,rowind) of
                                                     SOME m' => 
                                                     (let val m'' = Map.insert(m',icol,v)
                                                      in
                                                          Array.update (rowData,rowind,SOME m'');
                                                          nzcount := (!nzcount) + 1
                                                      end)
                                                   | NONE => (Array.update (rowData,rowind,
                                                                            SOME (IntMap.singleton(icol,v)));
                                                              nzcount := (!nzcount) + 1))
                                             m
                                     end))

                    val data    = Tensor.Array.array (!nzcount, zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (rows, 0)
                    val fi      = buildSparseFromMap (rowData, data, indptr, indices)
                in
                    {shape=shape, 
                     blocks=[SPARSE {offset = case offset of NONE => [0,0] | SOME i => i, 
                                     shape=fshape, nz={ indptr= indptr, indices=indices }, data=data}]}
                end
        end)



    fun insertBlock ({shape, blocks},b',boffset) =
        let
            val (i,j) = dimVals boffset
            val (m,n) = dimVals (case b' of
                                     SPARSE {offset, shape, nz, data} => shape
                                   | DENSE {offset, data} => Tensor.shape data)

                              

            val blocks' = 
                let
                    fun merge ([], []) = [b']
                      | merge (b::rst, ax) =
                        let 
                            val (bm,bn) = dimVals (case b of 
                                                       SPARSE {offset,shape=shape,nz,data} => shape
                                                     | DENSE {offset,data} => Tensor.shape data)
                            val (bi,bj) = dimVals (case b of
                                                       SPARSE {offset=offset,shape,nz,data} => offset
                                                     | DENSE {offset=offset,data} => offset)

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

            
    fun insertList (S as {shape, blocks},a,shape_a,offset) =
        let
            val (i,j) = dimVals offset
            val {shape=_, blocks=bs} = fromList shape (a,shape_a,SOME [i,j])
            val b': block = case bs of
                                [b] => b
                              | _ => raise Match
        in
            insertBlock (S,b',offset)
        end

    fun fromLists shape (al: ({l: (int * (int * elem) list) list, shape_l: index, offset: index}) list) = 
        (case al of
             {l,shape_l,offset}::rst => 
             (List.foldl (fn ({l,shape_l,offset},ax) => insertList (ax,l,shape_l,offset))
                         (fromList shape (l, shape_l, SOME offset))
                         rst)
           | _ => raise Match)

    fun insertVector (S as {shape, blocks},a,shape_a,offset) =
        let
            val (i,j) = dimVals offset
            val {shape=_, blocks=bs} = fromVector shape (a,shape_a,SOME [i,j])
            val b': block = case bs of
                                [b] => b
                              | _ => raise Match
        in
            insertBlock (S,b',offset)
        end

    fun fromVectors shape (al: ({v: (int * int * elem) vector, shape_v: index, offset: index}) list) = 
        (case al of
             {v,shape_v,offset}::rst => 
             (List.foldl (fn ({v,shape_v,offset},ax) => insertVector (ax,v,shape_v,offset))
                         (fromVector shape (v, shape_v, SOME offset))
                         rst)
           | _ => raise Match)

    fun insertTensor (S as {shape, blocks},t,offset) =
        let
            val (i,j) = dimVals offset
            val {shape=_, blocks=bs} = fromTensor shape (t,SOME [i,j])
            val b': block = case bs of
                                [b] => b
                              | _ => raise Match
        in
            insertBlock (S,b',offset)
        end

    fun insertTensor' (S as {shape, blocks},t,offset) =
        let
            val (i,j) = dimVals offset
            val {shape=_, blocks=bs} = fromTensor' shape (t,SOME [i,j])
            val b': block = case bs of
                                [b] => b
                              | _ => raise Match
        in
            insertBlock (S,b',offset)
        end
                                               
                  
    (* Builds a sparse matrix from a list of the form:
  
       {tensor,offset=[xoffset,yoffset],sparse) ...

      where xoffset and yoffset are the positions where to insert the
      given tensor. The tensors to be inserted must be non-overlapping.
      sparse is a boolean flag that indicates whether the tensor should
      be converted to sparse form.
    *)

    fun fromTensorList shape (al: ({tensor: Tensor.tensor, offset: index, sparse: bool}) list) = 
        (case al of
             {tensor,offset,sparse}::rst => 
             (List.foldl (fn ({tensor,offset,sparse},ax) => 
                             if sparse
                             then insertTensor (ax,tensor,offset)
                             else insertTensor' (ax,tensor,offset))
                         (if sparse 
                          then fromTensor shape (tensor, SOME offset)
                          else fromTensor' shape (tensor, SOME offset) )
                         rst)
           | _ => raise Match)


    fun fromGeneratorList shape (gg: ({f: index -> elem, fshape: index, offset: index}) list) = 
        case gg of 
            ({f,fshape,offset}::rst) =>
            (List.foldl 
                 (fn ({f,fshape,offset},S) => 
                     let
                         val {shape=_, blocks=bs} = fromGenerator shape (f,fshape,SOME offset)
                         val b': block = case bs of
                                             [b] => b
                                           | _ => raise Match
                     in
                         insertBlock (S,b',offset)
                 end)
                 (fromGenerator shape (f,fshape,SOME offset)) rst)
            | _ => raise Match

    fun fromMapGeneratorList shape (gg: ({f: int -> elem Map.map, forder: storage, fshape: index, offset: index}) list) = 
        case gg of 
            ({f,forder,fshape,offset}::rst) =>
            (List.foldl 
                 (fn ({f,forder,fshape,offset},S) => 
                     let
                         val {shape=_, blocks=bs} = fromMapGenerator shape (f,forder,fshape,SOME offset)
                         val b': block = case bs of
                                             [b] => b
                                           | _ => raise Match
                     in
                         insertBlock (S,b',offset)
                 end)
                 (fromMapGenerator shape (f,forder,fshape,SOME offset)) rst)
            | _ => raise Match


    fun insert (x: matrix, y: matrix) =
        let
            val {shape=_, blocks=bs} = x
        in
            foldl (fn (b,S) => 
                      let
                          val offset = (case b of 
                                            SPARSE {offset, shape, nz, data} =>  offset
                                          | DENSE {offset, data} => offset)
                      in
                          insertBlock (S,b,offset)
                      end) y bs
        end



    (* --- ACCESSORS --- *)

    fun shape {shape, blocks} = shape

    fun sub ({shape, blocks},index) =
        let
            val (i,j) = dimVals index
            val block = findBlock (i,j,blocks)
        in
            case block of
                SOME (b) => 
                (case b of 
                     SPARSE {offset, shape, nz, data} => 
                     (let 
                         val (m,n) = dimVals offset
                         val p = Index.toInt shape nz [i-m,j-n]
                       in
                           case p of SOME p' => Tensor.Array.sub (data, p')
                                   | NONE => zero
                       end)
                     | DENSE {offset, data} => 
                     (let 
                         val (m,n) = dimVals offset
                       in
                           Tensor.sub (data,[i-m,j-n])
                       end)
                )
              | NONE => zero
        end

    fun sub' ({shape, blocks},index) =
        let
            val (i,j) = dimVals index
            val block = findBlock (i,j,blocks)
        in
            case block of
                SOME (b) => 
                (case b of 
                     SPARSE {offset, shape, nz, data} => 
                     (let 
                         val (m,n) = dimVals offset
                         val p = Index.toInt shape nz [i-m,j-n]
                       in
                           case p of SOME p' => SOME (Tensor.Array.sub (data, p'))
                                   | NONE => NONE
                       end)
                     | DENSE {offset, data} => 
                     (let 
                         val (m,n) = dimVals offset
                       in
                           SOME (Tensor.sub (data,[i-m,j-n]))
                       end)
                )
              | NONE => NONE
        end

    fun update ({shape,blocks},index,new) =
        let
            val (i,j) = dimVals index
            val block = findBlock (i,j,blocks)
        in
            case block of
                SOME (b) => 
                (case b of 
                     SPARSE {offset, shape, nz, data} => 
                     (let
                         val (m,n) = dimVals shape
                         val p     = Index.toInt shape nz [i-m,j-n]
                     in
                         case p of SOME p' => Tensor.Array.update (data, p', new) | NONE => ()
                     end)
                     | DENSE {offset, data} =>
                       (let 
                         val (m,n) = dimVals offset
                       in
                           Tensor.update (data,[i-m,j-n],new)
                       end)
                )
              | NONE => ()
        end

    fun findBlocks (i,axis,blocks) =
        let
            val blocks' = List.mapPartial
                            (fn (b ) =>
                                let
                                    val (u,v) = case b of 
                                                    SPARSE {offset=offset, shape=shape, nz, data} => 
                                                      dimVals offset
                                                    | DENSE {offset, data} =>
                                                      dimVals offset
                                    val (t,s) = case b of 
                                                    SPARSE {offset=offset, shape=shape, nz, data} => 
                                                    dimVals shape
                                                    | DENSE {offset, data} =>
                                                      dimVals (Tensor.shape data)
                                                            
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
            (fn (SPARSE {offset=offset, shape=shape, nz={indptr, indices}, data})  =>
                let 
                    val (u,v) = dimVals offset
                    val (m,n) = dimVals shape

                    val i'  = case axis of 1 => i-v | 0 => i-u | _ => raise Match
                in
                (case (Index.order,axis) of
                     (Index.CSC,1) => (let 
                                           val s   = Unsafe.IntArray.sub (indptr, i')
                                           val e   = (if i' < (n-1)
                                                      then Unsafe.IntArray.sub (indptr, i'+1) 
                                                      else Tensor.Array.length data)
                                           val len = e-s
                                           val res = Tensor.Array.array (len, zero)
                                           val rsi = IntArray.array (len, 0)
                                           fun loop (i,n) = if i < e 
                                                            then (Tensor.Array.update (res,n,Tensor.Array.sub (data,i));
                                                                  Unsafe.IntArray.update (rsi,i-s,Index.sub (indices,i));
                                                                  loop (i+1,n+1))
                                                            else ()
                                       in
                                           loop (s,0);
                                           if len > 0 
                                           then SOME (SLSPARSE {data=Tensor.fromArray ([1,len],res),
                                                                indices=rsi,
                                                                offset=offset})
                                           else NONE
                                       end)
                   | (Index.CSR,0) => (let val s   = Unsafe.IntArray.sub (indptr, i')
                                           val e   = (if i' < (m-1) 
                                                      then Unsafe.IntArray.sub (indptr, i'+1) 
                                                      else Tensor.Array.length data)
                                           val len = e-s
                                           val res = Tensor.Array.array (len, zero)
                                           val rsi = IntArray.array (len, 0)
                                           fun loop (i,n) = if i < e 
                                                            then (Tensor.Array.update (res,n,Tensor.Array.sub (data,i));
                                                                  Unsafe.IntArray.update (rsi,i-s,Index.sub (indices,i));
                                                                  loop (i+1,n+1))
                                                            else ()
                                       in
                                           loop (s,0);
                                           if len > 0 
                                           then SOME (SLSPARSE {data=Tensor.fromArray ([1,len],res),
                                                                indices=rsi,
                                                                offset=offset})
                                           else NONE
                                       end)
                   | (Index.CSC,0) => (let fun findcol (n) = case IntArray.findi (fn (_,x) => n < x) indptr of
                                                                 SOME (m,_) => (m-1)
                                                               | NONE => m-1
                                           val vs = IntArray.foldri
                                                        (fn (n,ii,ax) => if i'=ii then (Tensor.Array.sub(data,n),findcol n)::ax else ax)
                                                        [] indices
                                           val len = List.length vs
                                       in
                                           if len > 0 
                                           then SOME (SLSPARSE {data=Tensor.fromList ([1,len],map #1 vs),
                                                                indices=IntArray.fromList (map #2 vs),
                                                                offset=offset})
                                           else NONE
                                       end)
                   | (Index.CSR,1) => (let fun findrow (n) = case IntArray.findi (fn (_,x) => n < x) indptr of
                                                                 SOME (n,_) => (n-1)
                                                               | NONE => n-1
                                           val vs = IntArray.foldri
                                                        (fn (n,ii,ax) =>  if i'=ii then (Tensor.Array.sub(data,n),findrow n)::ax else ax)
                                                        [] indices
                                           val len = List.length vs
                                       in
                                           if len > 0 
                                           then SOME (SLSPARSE {data=Tensor.fromList ([1,len],map #1 vs),
                                                                indices=IntArray.fromList (map #2 vs),
                                                                offset=offset})
                                           else NONE
                                       end)
                   | (_,_) => raise Index)
                end
            | (DENSE {offset=offset, data})  =>
              let 
                    val (u,v) = dimVals offset
                    val (m,n) = dimVals (Tensor.shape data)

                    val i'  = case axis of 1 => i-v | 0 => i-u | _ => raise Match

                    val sl  = case axis of 
                                  1 => RTensorSlice.fromto' ([0,i'],[m,i'],data)
                                | 0 => RTensorSlice.fromto' ([i',0],[i',n],data)
                                | _ => raise Match
              in
                  SOME (SLDENSE {data=sl, offset=offset})
              end
            )
            (findBlocks (i,axis,blocks) )
            
                                    
        end


    (* --- MAPPING --- *)

    fun map f {shape, blocks} =
        {shape=shape,
         blocks=(List.map 
                     (fn (SPARSE {offset, shape, nz, data}) =>
                         (SPARSE {offset=offset, shape=shape, nz=nz, data=array_map f data})
                       |  (DENSE {offset, data}) =>
                          (DENSE {data=(Tensor.map f data), offset=offset}))
                     blocks)}


    fun app f {shape, blocks} = 
        List.app (fn (SPARSE {offset, shape, nz, data}) => 
                     Tensor.Array.app f data
                   | (DENSE {offset, data}) => 
                     Tensor.app f data)
                 blocks


    fun appi f {shape, blocks} = 
        List.app (fn (SPARSE {offset, shape, nz, data}) => 
                     Index.iteri shape nz (fn (idx,i) => 
                                              f (ListPair.map (op +) (offset, idx),
                                                 Tensor.Array.sub (data,i)))
                   | (DENSE {offset, data}) => 
                     Tensor.appi (fn (i,v) => f (ListPair.map (op +) (offset, i),v)) data)
                 blocks


    fun *> n a = map (fn x => Number.*(n,x)) a


    (* --- BINOPS --- *)


    fun map2 f ({shape', blocks'}) ({shape'', blocks''}) =
        if Index.eq(shape',shape') 
        then {shape=shape',
              blocks=(ListPair.map 
                          (fn (SPARSE {offset=offset', shape=shape', nz=nz', data=data'},
                               SPARSE {offset=offset'', shape=shape'', nz=nz'', data=data''}) =>
                              (if Index.eq (offset',offset'') andalso 
                                  Index.eq (shape',shape'')
                               then (let 
                                         val t = Tensor.new (shape', zero)
                                         val _ = Index.iteri shape' nz' 
                                                             (fn (idx,i) => Tensor.update (t,idx,Tensor.Array.sub (data',i)))
                                         val _ = Index.iteri shape' nz'' 
                                                             (fn (idx,i) => 
                                                                 let val v' = Tensor.sub (t,idx)
                                                                     val v'' = f(v',Tensor.Array.sub(data'',i))
                                                                 in
                                                                     Tensor.update (t,idx,v'')
                                                                 end)
                                     in
                                         hd (#blocks (fromTensor shape' (t, SOME offset')))
                                     end)
                               else raise Match)
                            |  (DENSE {offset=offset', data=data'},
                                DENSE {offset=offset'', data=data''}) =>
                              (if Index.eq (offset',offset'') 
                               then
                                   DENSE {offset=offset',data=Tensor.map2 f data' data''}
                               else raise Match)
                            | (_, _) => raise Match)
                          (blocks',blocks''))}
        else raise Index


    (* --- SLICE OPERATIONS --- *)

    fun sliceAppi f sl =
        List.app
            (fn (SLSPARSE {data=sl,indices=si,offset}) => 
                let val (m,n) = dimVals offset
                in
                    (RTensor.foldl
                         (fn (x,i) => 
                             let
                                 val i' = Index.sub (si,i)+m
                             in
                                 (f (i',x); i+1)
                             end) 0 sl; ())
                end
            | (SLDENSE {data=sl,offset}) => 
              let val (m,n) = dimVals offset
              in
                  (RTensorSlice.foldl
                       (fn (x,i) => 
                           let
                               val i' = i+m
                           in
                               (f (i',x); i+1)
                           end) 0 sl; ())
              end)
            sl  

    fun sliceFoldi f init sl =
        List.foldl
            (fn (SLSPARSE {data=sl,indices=si,offset},ax) => 
                let val (m,n) = dimVals offset
                in
                    #2 (RTensor.foldl
                            (fn (x,(i,ax)) => 
                                let
                                    val i' = Index.sub (si,i)+m
                                in
                                    (i+1, f (i',x,ax))
                                end) (0,ax) sl)
                end
            | (SLDENSE {data=sl,offset},ax) => 
              let val (m,n) = dimVals offset
              in
                  #2 (RTensorSlice.foldl
                          (fn (x,(i,ax)) => 
                              let
                                  val i' = i+m
                              in
                                  (i+1, f (i',x,ax))
                              end) (0,ax) sl)
              end)
            init sl  

    fun a + b = map2 Number.+ a b
    fun a * b = map2 Number.* a b
    fun a - b = map2 Number.- a b
    fun a / b = map2 Number./ a b

end


