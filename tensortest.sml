
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


structure TensorTest =
struct

val N1 = 8
val N2 = 2
val N3 = 10
val N =  N1+N2

fun putStrLn (file, str) = 
    (TextIO.output (file, str);
     TextIO.output (file, "\n"))
    
fun putStr (file, str) = 
    (TextIO.output (file, str))




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



val SN1 = (RTensor.*> 0.5 (realRandomTensor (13,17) [N,N1]) )
val SN2 = (RTensor.~ (realRandomTensor (19,23) [N,N2]))

val _ = TensorFile.realTensorWrite (TextIO.stdOut) SN1
val _ = TensorFile.realTensorWrite (TextIO.stdOut) SN2

val SN  = RTensor.cat (SN1, SN2, 1)
val _ = TensorFile.realTensorWrite (TextIO.stdOut) SN

val SN3 = RTensor.new ([N,N3],10.0)
val _ = TensorFile.realTensorWrite (TextIO.stdOut) SN3

val SN'  = RTensor.cat (SN, SN3, 1)
val _ = TensorFile.realTensorWrite (TextIO.stdOut) SN'

val S0  = RTensor.fromList ([2,2],[1.0,2.0,3.0,4.0])
val _ = (print "S0 = "; TensorFile.realTensorWrite (TextIO.stdOut) S0)
val v = RTensor.sub (S0,[0,0])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S0,[0,1])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S0,[1,0])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S0,[1,1])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)

val v = RTensor.sub (SN1,[0,0])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (SN1,[0,1])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (SN1,[1,0])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (SN1,[1,1])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (SN1,[2,0])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (SN1,[2,1])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)

val _ = putStrLn (TextIO.stdOut, "prepad:")
val ones = RTensor.new ([2,2],1.0)
val S  = RTensor.prepad (ones, 2, 0.0, 1)
val _ = TensorFile.realTensorWrite (TextIO.stdOut) S
val v = RTensor.sub (S,[0,0])
val _ = (print "S(0,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,1])
val _ = (print "S(0,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,2])
val _ = (print "S(0,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,3])
val _ = (print "S(0,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,0])
val _ = (print "S(1,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,1])
val _ = (print "S(1,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,2])
val _ = (print "S(1,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,3])
val _ = (print "S(1,3) = "; TensorFile.realWrite (TextIO.stdOut) v)

val _ = putStrLn (TextIO.stdOut, "prepad:")
val ones = RTensor.new ([2,2],1.0)
val S  = RTensor.prepad (ones, 2, 0.0, 0)
val _ = TensorFile.realTensorWrite (TextIO.stdOut) S
val v = RTensor.sub (S,[0,0])
val _ = (print "S(0,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,0])
val _ = (print "S(1,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,0])
val _ = (print "S(2,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,0])
val _ = (print "S(3,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,1])
val _ = (print "S(0,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,1])
val _ = (print "S(1,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,1])
val _ = (print "S(2,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,1])
val _ = (print "S(3,1) = "; TensorFile.realWrite (TextIO.stdOut) v)


val S  = RTensor.cat (SN1, SN2, 1)
val _ = TensorFile.realTensorWrite (TextIO.stdOut) S

val v = RTensor.sub (S,[0,0])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,1])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,2])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,0])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,1])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,2])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,0])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,1])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,2])
val _ = (print "v = "; TensorFile.realWrite (TextIO.stdOut) v)

val S1 = RTensorSlice.slice ([([0,0],[N-1,0])],S)
val S2 = RTensorSlice.slice ([([0,1],[N-1,1])],S)
val S3 = RTensorSlice.slice ([([0,2],[N-1,2])],S)

val _ = TensorFile.realTensorSliceWrite (TextIO.stdOut) S1
val _ = TensorFile.realTensorSliceWrite (TextIO.stdOut) S2
val _ = TensorFile.realTensorSliceWrite (TextIO.stdOut) S3

val _ = putStrLn (TextIO.stdOut, "insert:")
val S    = RTensor.new ([4,4],0.0)
val ones = RTensor.new ([2,2],1.0)
val twos = RTensor.new ([2,2],2.0)
val threes = RTensor.new ([1,2],3.0)
val _    = TensorFile.realTensorWrite (TextIO.stdOut) threes
val _    = RTensor.insert (S, threes, [2,0])
val _    = RTensor.insert (S, ones, [0,0])
val _    = RTensor.insert (S, twos, [2,2])
val _    = TensorFile.realTensorWrite (TextIO.stdOut) S
val v = RTensor.sub (S,[0,0])
val _ = (print "S(0,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,1])
val _ = (print "S(0,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,2])
val _ = (print "S(0,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,3])
val _ = (print "S(0,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,0])
val _ = (print "S(1,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,1])
val _ = (print "S(1,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,2])
val _ = (print "S(1,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,3])
val _ = (print "S(1,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,0])
val _ = (print "S(2,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,1])
val _ = (print "S(2,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,2])
val _ = (print "S(2,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,3])
val _ = (print "S(2,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,0])
val _ = (print "S(3,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,1])
val _ = (print "S(3,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,2])
val _ = (print "S(3,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,3])
val _ = (print "S(3,3) = "; TensorFile.realWrite (TextIO.stdOut) v)

val r  = Range.fromto (RTensor.shape SN) ([0,0],[1,4])
val xs = Range.foldi_range
             (fn ((i,j),ax) => (Index.toInt (RTensor.shape SN) i,
                                Index.toInt (RTensor.shape SN) j) :: ax)
             [] r

val _ = putStrLn (TextIO.stdOut, "insert:")
val S    = RTensor.new ([4,4],0.0)
val ones = RTensor.new ([4,2],1.0)
val twos = RTensor.new ([4,2],2.0)
val _    = RTensor.insert (S, ones, [0,0])
val _    = RTensor.insert (S, twos, [0,2])
val v = RTensor.sub (S,[0,0])
val _ = (print "S(0,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,1])
val _ = (print "S(0,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,2])
val _ = (print "S(0,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[0,3])
val _ = (print "S(0,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,0])
val _ = (print "S(1,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,1])
val _ = (print "S(1,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,2])
val _ = (print "S(1,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[1,3])
val _ = (print "S(1,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,0])
val _ = (print "S(2,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,1])
val _ = (print "S(2,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,2])
val _ = (print "S(2,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[2,3])
val _ = (print "S(2,3) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,0])
val _ = (print "S(3,0) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,1])
val _ = (print "S(3,1) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,2])
val _ = (print "S(3,2) = "; TensorFile.realWrite (TextIO.stdOut) v)
val v = RTensor.sub (S,[3,3])
val _ = (print "S(3,3) = "; TensorFile.realWrite (TextIO.stdOut) v)


end
