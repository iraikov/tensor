
(*
 Copyright (c) Ivan Raikov.
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
        This product includes software developed by Ivan Raikov.

*)

fun putStrLn out str = 
    (TextIO.output (out, str);
     TextIO.output (out, "\n"))

val N = 10

fun realRandomTensor (xseed,yseed) shape =
    let 
        val length = Index.length shape
        val seed   = Random.rand (xseed,yseed)
        val a      = RTensor.Array.array(length, Random.randReal seed)
        fun loop 0 = RTensor.fromArray(shape, a)
          | loop j = (RTensor.Array.update(a, length-j, Random.randReal seed);
                      loop (j-1))
    in 
        loop (length - 1)
    end


val epsilon = 0.3
val T  = realRandomTensor (13,17) [N,N]
val T' = RTensor.map (fn (x) => (if Real.> (epsilon,x) then 1.0 else 0.0)) T
val _  = TensorFile.realTensorWrite (TextIO.stdOut) T'

val ST  = SparseMatrix.fromTensor T'

val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString 0) ^ ": ")
val _  = TensorFile.realTensorWrite (TextIO.stdOut) (SparseMatrix.slice (ST,1,0))

val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString 1) ^ ": ")
val _  = TensorFile.realTensorWrite (TextIO.stdOut) (SparseMatrix.slice (ST,1,1))

val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString 9) ^ ": ")
val _  = TensorFile.realTensorWrite (TextIO.stdOut) (SparseMatrix.slice (ST,1,9))


val _ = putStrLn TextIO.stdOut "SparseTensor insert:"
val S    = SparseTensor.new ([4,4],0.0)
val ones = RTensor.new ([2,2],1.0)
val twos = RTensor.new ([2,2],2.0)
val threes = RTensor.new ([1,2],3.0)
val S    = SparseTensor.insert (S, threes, [2,0])
val S    = SparseTensor.insert (S, ones, [0,0])
val S    = SparseTensor.insert (S, twos, [2,2])
val v = SparseTensor.sub (S,[0,0])
val _ = (print "S(0,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[0,1])
val _ = (print "S(0,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[0,2])
val _ = (print "S(0,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[0,3])
val _ = (print "S(0,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[1,0])
val _ = (print "S(1,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[1,1])
val _ = (print "S(1,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[1,2])
val _ = (print "S(1,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[1,3])
val _ = (print "S(1,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[2,0])
val _ = (print "S(2,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[2,1])
val _ = (print "S(2,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[2,2])
val _ = (print "S(2,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[2,3])
val _ = (print "S(2,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[3,0])
val _ = (print "S(3,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[3,1])
val _ = (print "S(3,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[3,2])
val _ = (print "S(3,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = SparseTensor.sub (S,[3,3])
val _ = (print "S(3,3) = "; TensorFile.realWrite (TextIO.stdOut) v)

