
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



fun putStrLn out str = 
    (TextIO.output (out, str);
     TextIO.output (out, "\n"))


signature SPARSE_INDEX =
    sig
        type t
        type iarray = IntArray.array
        type nonzero = { indptr: iarray, indices: iarray }
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

signature MONO_SPARSE =
    sig
	structure Tensor : MONO_TENSOR
	structure Number : NUMBER
	structure Index : SPARSE_INDEX

        type index = Index.t
	type elem = Number.t
	type matrix

	exception Data and Shape

	val fromTensor : Tensor.tensor -> matrix

	val shape : matrix -> index

	val sub : matrix * index -> elem
	val update : matrix * index * elem -> unit

	val slice : (matrix * int * int) -> Tensor.tensor

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
        type iarray = IntArray.array
        type nonzero = { indptr: iarray, indices: iarray }
	type indexer = t -> int option
        datatype storage = CSR | CSC
                            
	exception Index
	exception Shape

	val order = CSC

	fun validShape shape = List.all (fn x => x > 0) shape
	fun validIndex index = List.all (fn x => x >= 0) index

        val sub = Unsafe.IntArray.sub

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
            let val nptr = IntArray.length indptr
                val nind = IntArray.length indices
            in
                case order of 
                    CSC => 
                    (case (index, shape) of
                         ([i,j],[s,rs]) => 
                         if (i >= 0) andalso (i < s) 
                         then
                             (let
                                  val s = sub (indptr,j)
                                  val e = if (i < nptr) then sub (indptr,i+1) else nind
                                  val n = findFromTo (i, indices, s, e)
                              in 
                                  case n of 
                                      SOME n' => SOME (s + n')
                                    | NONE => NONE
                          end) 
                         else raise Index
                       | ([],[]) => SOME 0
                       | (_,_)   => raise Index)
                  | CSR => 
                    (case (index, shape) of
                         ([i,j],[s,rs]) => 
                         if (i >= 0) andalso (i < s) 
                         then
                             (let
                                  val s = sub (indptr,i)
                                  val e = if (i < nptr) then sub (indptr,i+1) else nind
                                  val n = findFromTo (j, indices, s, e)
                              in
                                  case n of
                                      SOME n' => SOME (s + n')
                                    | NONE    => NONE
                              end)
                         else raise Index
                       | ([],[]) => SOME 0
                       | (_,_)   => raise Index)
                    
            end

        fun app shape {indptr, indices}   =
            (case order of 
                 CSC => 
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
               | CSR =>
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



structure SparseMatrix : MONO_SPARSE =

struct
    structure Tensor : MONO_TENSOR = RTensor
    structure Number = RTensor.Number
    structure Index = SparseIndex

    type index   = Index.t
    type nonzero = Index.nonzero
    type elem    = Number.t
    type matrix  = {shape: index, nz: nonzero, data: elem array}

    exception Data
    exception Shape
    exception Index

    (* --- LOCALS --- *)

    fun array_map f a =
	let fun apply index = f(Array.sub(a,index)) in
	    Array.tabulate(Array.length a, apply)
	end

    fun array_mapi f a =
	let fun apply index = f(index,Array.sub(a,index)) in
	    Array.tabulate(Array.length a, apply)
	end

    (* --- CONSTRUCTORS --- *)

    fun fromTensor (a: Tensor.tensor) = 
        (let 
            val shape as [rows,cols] = Tensor.shape a
        in
            case Index.order of
                Index.CSR => 
                let 
                    val v0: (int * elem) list = []
	            val data: ((int * elem) list) Array.array  = Array.array(rows,v0)
                    val nzcount = ref 0
                    val _ = Tensor.Index.app shape
                                              (fn (i) => 
                                                  let 
                                                      val v = Tensor.sub (a, i)
                                                  in
                                                      if not (Number.== (v, Number.zero))
                                                      then
                                                          let val [irow,icol] = i
                                                              val row  = Array.sub (data, irow)
                                                              val row' = (icol,v) :: row
                                                          in
                                                              Array.update(data,irow,row');
                                                              nzcount := (!nzcount) + 1
                                                          end
                                                      else ()
                                                  end)
                    val data'   = Array.array (!nzcount, Number.zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (rows, 0)
                    val update  = Unsafe.IntArray.update
                in
                    (Array.foldli (fn (n,rowlist,i) => 
                                      let 
                                          val i' = List.foldl (fn ((colind,v),i) => 
                                                                  (Array.update (data',i,v); 
                                                                   update (indices,i,colind); 
                                                                   i+1))
                                                              i rowlist
                                      in
                                          (update (indptr,n,i); i')
                                      end)
                                  0 data;
                     {shape=shape, nz={ indptr= indptr, indices=indices }, data=data'}
                     )
                end
              | Index.CSC =>
                let 
                    val v0: (int * elem) list = []
	            val data: ((int * elem) list) Array.array  = Array.array(cols,v0)
                    val nzcount = ref 0
                    val _ = Tensor.Index.app shape
                                      (fn (i) => 
                                          let 
                                              val v = Tensor.sub (a, i)
                                          in
                                              if not (Number.== (v, Number.zero))
                                              then
                                                  let val [irow,icol] = i
                                                      val col  = Array.sub (data, icol)
                                                      val col' = (irow,v) :: col
                                                  in
                                                      Array.update(data,icol,col');
                                                      nzcount := (!nzcount) + 1
                                                  end
                                              else ()
                                          end)
                    val data'   = Array.array (!nzcount, Number.zero)
                    val indices = IntArray.array (!nzcount, 0)
                    val indptr  = IntArray.array (cols, 0)
                    val update  = Unsafe.IntArray.update
                in
                    (Array.foldli (fn (n,collist,i) => 
                                      let 
                                          val i' = List.foldl (fn ((rowind,v),i) => 
                                                                  (Array.update (data',i,v); 
                                                                   update (indices,i,rowind); 
                                                                   i+1))
                                                              i collist
                                      in
                                          (update (indptr,n,i); i')
                                      end)
                                  0 data;
                     {shape=shape, nz={ indptr= indptr, indices=indices }, data=data'}
                     )
                end
        end)
                  

    (* --- ACCESSORS --- *)

    fun shape {shape, nz, data} = shape

    fun sub ({shape, nz, data},index as [i,j]) =
        (case Index.toInt shape nz [i,j] of
             SOME n => Array.sub (data, n)
           | NONE => Number.zero)


    fun update ({shape, nz, data},index as [i,j],new) =
        (case Index.toInt shape nz [i,j] of
             SOME n => Array.update (data, n, new)
           | NONE => raise Data)

    fun slice ({shape as [m,n], nz={indptr, indices}, data},axis,i) =
        (case (Index.order,axis) of
             (Index.CSC,1) => (let val s   = IntArray.sub (indptr, i)
                                   val e   = (if i<(m-1) then IntArray.sub (indptr, i+1) else Array.length data)
                                   val len = e-s
                                   val res = RNumberArray.array (len, Number.zero)
                                   fun loop (i,n) = if i < e 
                                                    then (RNumberArray.update (res,n,Array.sub (data,i));
                                                          loop (i+1,n+1))
                                                    else ()
                               in
                                   loop (s,0);
                                   Tensor.fromArray ([len,1],res)
                               end)
           | (Index.CSR,0) => (let val s   = IntArray.sub (indptr, i)
                                   val e   = (if i<(m-1) then IntArray.sub (indptr, i+1) else Array.length data)
                                   val len = e-s
                                   val res = RNumberArray.array (len, Number.zero)
                                   fun loop (i,n) = if i < e 
                                                    then (RNumberArray.update (res,n,Array.sub (data,i));
                                                          loop (i+1,n+1))
                                                    else ()
                               in
                                   loop (s,0);
                                   Tensor.fromArray ([1,len],res)
                               end)
           | (Index.CSC,0) => (let val vs = IntArray.foldri
                                                (fn (n,ii,ax) =>  if ii=i then Array.sub(data,n)::ax else ax)
                                                [] indices
                                   val len = List.length vs
                               in
                                   Tensor.fromList ([1,len],vs)
                               end)
           | (Index.CSR,1) => (let val vs = IntArray.foldri
                                                (fn (n,ii,ax) =>  if ii=i then Array.sub(data,n)::ax else ax)
                                                [] indices
                                   val len = List.length vs
                               in
                                   Tensor.fromList ([len,1],vs)
                               end)
           | (_,_) => raise Index
        )


    (* --- MAPPING --- *)

    fun map f {shape, nz, data} =
        {shape=shape, nz=nz, data=array_map f data}

    fun app f {shape, nz, data} = Array.app f data


    (* --- BINOPS --- *)
(*
    fun a + b = map2 Number.+ a b
    fun a * b = map2 Number.* a b
    fun a - b = map2 Number.- a b
    fun a / b = map2 Number./ a b
    fun ~ a = map Number.~ a
*)
end


