
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


structure SparseTest =
struct

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

val _ = putStrLn TextIO.stdOut ("sparse slice column " ^ (Int.toString 0) ^ ": ")
val _  = TensorFile.realTensorWrite (TextIO.stdOut) (SparseMatrix.slice (ST,1,0))

val _ = putStrLn TextIO.stdOut ("sparse slice column " ^ (Int.toString 1) ^ ": ")
val _  = TensorFile.realTensorWrite (TextIO.stdOut) (SparseMatrix.slice (ST,1,1))

val _ = putStrLn TextIO.stdOut ("sparse slice column " ^ (Int.toString 9) ^ ": ")
val _  = TensorFile.realTensorWrite (TextIO.stdOut) (SparseMatrix.slice (ST,1,9))

end
