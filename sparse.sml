
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




signature SPARSE_INDEX =
    sig
        type t
        type nonzero = { indptr: int Vector.vector, indices: int Vector.vector }
	type indexer = t -> int option
        datatype storage = CRS | CCS

        exception Index
        exception Shape

        val order : storage
        val toInt : t -> t -> int

        val inBounds : t -> t -> bool

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

 fromList [[number,number,...]*]
	Builds a sparse matrix up from a lists of lists of numbers
	(i.e. a matrix in list format and in column major order)

 rows matrix
 cols matrix
	Return the dimensions of the matrix

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
	structure Tensor : NUMBER_TENSOR
	structure Number : NUMBER
	structure Index : SPARSE_INDEX

        type index = Index.index
	type elem = Number.t
	type matrix

	exception Data
	and Shape

	val fromArray : (int array, int array, elem array) -> matrix
	val full : matrix -> Tensor.tensor

	val sub : matrix * index -> elem
	val update : matrix * index * elem -> unit

	val map : (elem -> elem) -> matrix -> matrix
	val map2 : (elem * elem -> elem) -> matrix -> matrix -> matrix
	val mapi : (index * elem -> elem) -> matrix -> matrix
	val app : (elem -> unit) -> matrix -> unit
	val appi : (index * elem -> unit) -> matrix -> unit

	val + : matrix * matrix -> matrix
	val - : matrix * matrix -> matrix
	val * : matrix * matrix -> matrix
	val / : matrix * matrix -> matrix
	val ~ : matrix -> matrix

	val print : matrix -> unit
    end



structure SparseIndex =
    struct

        structure Vector = Vector

	type t = int list
        type nonzero = { indptr: int Vector.vector, indices: int Vector.vector }
	type indexer = t -> int option
        datatype storage = CRS | CCS
                            
	exception Index
	exception Shape

	val order = CCS

	fun validShape shape = List.all (fn x => x > 0) shape
	fun validIndex index = List.all (fn x => x >= 0) index

        val sub = Unsafe.IntArray.sub


        fun findFromTo (i,v,s,e) =
            let fun loop (j) = 
                    if ((j >= s) andalso (j < e)) 
                    then (if (Vector.sub (v,j) = i) then SOME j else loop (j+1))
                    else NONE
            in
                loop s
            end


	fun toInt shape {indptr, indices} index  =
            let val nptr = Vector.length indptr
                val nind = Vector.length indices
            in
                case order of 
                    CCS => 
                    (case (index, shape) of
                         ([i,j],[s,rs]) => 
                         if (i >= 0) andalso (i < s) 
                         then
                             (let
                                  val s = Vector.sub (indptr,j)
                                  val e = if (i < nptr) then Vector.sub (indptr,i+1) else nind
                                  val n = findFromTo (i, indices, s, e)
                              in 
                                  case n of 
                                      SOME n' => SOME (s + n')
                                    | NONE => NONE
                          end) 
                         else raise Index
                       | ([],[]) => SOME 0
                       | (_,_)   => raise Index)
                  | CRS => 
                    (case (index, shape) of
                         ([i,j],[s,rs]) => 
                         if (i >= 0) andalso (i < s) 
                         then
                             (let
                                  val s = Vector.sub (indptr,i)
                                  val e = if (i < nptr) then Vector.sub (indptr,i+1) else nind
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
            
        fun inBounds shape index =
	    ListPair.all (fn (x,y) => (x >= 0) andalso (x < y))
	    (index, shape)


    end



structure SparseMatrix :> MONO_SPARSE
    where type Tensor.Number.t = RealTensor.Number.t
	  and type Tensor.tensor = RealTensor.tensor =
struct
    structure Tensor : NUMBER_TENSOR = RealTensor
    structure Number = RealTensor.Number

    structure Index = SparseIndex

    type elem = Number.t
    type matrix = {nz: Index.nonzero, data: elem array}

    exception Data
    exception Shape
    exception Index

    (* --- LOCALS --- *)

    fun valid_dim d = (d > 0)

    fun array_map f a =
	let fun apply index = f(Array.sub(a,index)) in
	    Array.tabulate(Array.length a, apply)
	end

    fun array_mapi f a =
	let fun apply index = f(index,Array.sub(a,index)) in
	    Array.tabulate(Array.length a, apply)
	end

    (* --- CONSTRUCTORS --- *)

    fun fromArray (i,j,a) = 
        if (not ( ((Array.length a) = (Array.length i)) andalso
                  ((Array.length a) = (Array.length j))))
        then raise Data
        else (case Index.order of
                  Index.CRS =>

    (* --- ACCESSORS --- *)


    fun sub ({shape, nz, data},[i,j] as index) =
        case Index.toInt shape nz [i,j] of
                 SOME n => Array.sub (data, n)
               | NONE => Number.zero


    fun update ({shape, nz, data},[i,j] as index,new) =
        case Index.toInt shape nz [i,j] of
                 SOME n => Array.update (data, n, new)
               | NONE => raise Data


    (* --- MAPPING --- *)

    fun map f {shape, nz, data} =
        {shape=shape, nz=nz, data=array_map f data}


    fun mapi f {rows,cols,data} =
	let fun for_row (row_ndx,row) =
	    let fun for_item (col_ndx,x) =
		(col_ndx, f(row_ndx,col_ndx,x))
	    in
		List.map for_item row
	    end
	in
	    {rows=rows,cols=cols,data=array_mapi for_row data}
	end

    fun map2 f {rows=rows1, cols=cols1, data=data1}
	       {rows=rows2, cols=cols2, data=data2} =
	let fun fcons (col,x,rest) =
	        if Number.==(x,Number.zero) then rest else (col,x)::rest
	    fun pair_map f [] [] = []
	      | pair_map f ((col,x)::rest) [] =
		fcons(col, f(x,Number.zero), pair_map f rest [])
	      | pair_map f [] ((col,x)::rest) =
		fcons(col, f(Number.zero,x), pair_map f [] rest)
	      | pair_map f l1 l2 =
		let val (c1,x1)::rest1 = l1
		    val (c2,x2)::rest2 = l2
		in
		    if (c1 < c2) then
			fcons(c1, f(x1,Number.zero), pair_map f rest1 l2)
		    else if (c1 > c2) then
			fcons(c2, f(Number.zero,x2), pair_map f l1 rest2)
		    else
			fcons(c1, f(x1,x2), pair_map f rest1 rest2)
		end
	    fun loop_rows row =
		pair_map f (Array.sub(data1,row)) (Array.sub(data2,row))
	in
	    if (rows1 = rows2) andalso (rows2 = cols2) then
		{rows=rows1, cols=cols1, data=Array.tabulate(rows1,loop_rows)}
	    else
		raise Match
	end (* map2 *)
	

    fun app f {rows,cols,data} =
	let fun for_item (_,value) = f value
	    fun for_row row = List.app for_item row in
	    Array.app for_row data
	end

    fun appi f {rows,cols,data} =
	let fun for_row (row_ndx,row) =
	    let fun for_item (col_ndx,x) =
		f(row_ndx,col_ndx,x)
	    in
		List.app for_item row
	    end
	in
	    Array.appi for_row (data,0,NONE)
	end

    fun full matrix =
	let val {rows,cols,data} = matrix
	    val c = Tensor.new([rows,cols],Number.zero)
	    fun copy (row,col,value) = Tensor.update(c,[row,col],value)
	in
	    appi copy matrix;
	    c
	end

    (* --- PRINTING --- *)
    fun print m =
	let val print = TextIO.print
	    fun print_item (row,col,value) =
	    (print "("; print (Int.toString row); print ",";
	     print (Int.toString col); print ") = ";
	     print (Number.toString value);
	     print "\n")
	in
	    print "("; print (Int.toString (rows m));
	    print ","; print (Int.toString (cols m)); print ")\n";
	    appi print_item m
	end

    (* --- BINOPS --- *)

    fun a + b = map2 Number.+ a b
    fun a * b = map2 Number.* a b
    fun a - b = map2 Number.- a b
    fun a / b = map2 Number./ a b
    fun ~ a = map Number.~ a

end
