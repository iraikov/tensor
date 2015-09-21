
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


fun realRandomTensor (xseed,yseed) shape =
    let 
        val length = Index.length shape
        val seed   = Random.rand (xseed,yseed)
        val a      = RTensor.Array.array(length, 0.0)
        fun loop 0 = RTensor.fromArray(shape, a)
          | loop j = (RTensor.Array.update(a, length-j, Random.randReal seed);
                      loop (j-1))
    in 
        loop (length - 1)
    end

val _ = putStrLn TextIO.stdOut "SparseMatrix fromMapGenerator (CSC):"

val seed   = Random.rand (13,17)
val prob   = 0.1
val SA     = SparseMatrix.fromMapGenerator
                 [10,10] 
                 (fn (i) => foldl (fn((k,v),m) => IntMap.insert'((k,v),m))
                                  IntMap.empty
                                  (List.filter 
                                       (fn(j,r) => Real.>= (r, prob)) 
                                       (List.tabulate (4, (fn(j) => (j,Random.randReal seed))))),
                  SparseMatrix.Index.CSC,
                  [4,4], 
                  SOME [5,5])

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,1,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice row " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,0,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)

val _ = SparseMatrix.appi 
            (fn (i,v) => (putStr TextIO.stdOut "SA[";
                          TensorFile.listLineWrite Int.toString TextIO.stdOut i;
                          putStrLn TextIO.stdOut ("]: " ^ (Real.toString v))))
            SA


val _ = putStrLn TextIO.stdOut "SparseMatrix fromMapGenerator (CSR):"

val seed   = Random.rand (13,17)
val prob   = 0.1
val SA     = SparseMatrix.fromMapGenerator
                 [10,10] 
                 (fn (i) => foldl (fn((k,v),m) => IntMap.insert'((k,v),m))
                                  IntMap.empty
                                  (List.filter 
                                       (fn(j,r) => Real.>= (r, prob)) 
                                       (List.tabulate (4, (fn(j) => (j,Random.randReal seed))))),
                  SparseMatrix.Index.CSR,
                  [4,4], 
                  SOME [5,5])

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,1,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice row " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,0,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)

val _ = SparseMatrix.appi 
            (fn (i,v) => (putStr TextIO.stdOut "SA[";
                          TensorFile.listLineWrite Int.toString TextIO.stdOut i;
                          putStrLn TextIO.stdOut ("]: " ^ (Real.toString v))))
            SA

val _ = putStrLn TextIO.stdOut "SparseMatrix fromGenerator:"

val seed   = Random.rand (13,17)
val prob   = 0.1
val SA     = SparseMatrix.fromGenerator
                 [10,10] 
                 (fn (i) => (if Real.>= (Random.randReal seed, prob) then 1.0 else 0.0),
                  [4,4], SOME [5,5])

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,1,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice row " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,0,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)

val _ = SparseMatrix.appi 
            (fn (i,v) => (putStr TextIO.stdOut "SA[";
                          TensorFile.listLineWrite Int.toString TextIO.stdOut i;
                          putStrLn TextIO.stdOut ("]: " ^ (Real.toString v))))
            SA

val _ = putStrLn TextIO.stdOut "SparseMatrix fromTensor:"

val SA  = SparseMatrix.fromTensor
              [6,6] 
              (RTensor.fromList 
                   ([6,6], 
                    List.concat
                    [
                     [10.0,3.0,0.0,3.0,0.0,0.0],
                     [0.0,9.0,7.0,0.0,8.0,4.0],
                     [0.0,0.0,8.0,8.0,0.0,0.0],
                     [0.0,0.0,7.0,7.0,9.0,0.0],
                     [~2.0,0.0,0.0,5.0,9.0,2.0],
                     [0.0,3.0,0.0,0.0,13.0,~1.0]
                     ]), NONE)


val _ = Loop.app
            (0,6,fn (i) => 
                    Loop.app (0,6,fn (j) => 
                                     (
                                      print ("SA(" ^ (Int.toString i) ^ "," ^ (Int.toString j) ^ ") = "); 
                                      TensorFile.realWrite 
                                          (TextIO.stdOut) 
                                          (SparseMatrix.sub (SA,[i,j]))
                                     )
            ))

val _ = Loop.app
            (0,6,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,1,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)

val _ = Loop.app
            (0,6,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice row " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SA,0,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)


val _ = putStrLn TextIO.stdOut "SparseMatrix fromTensorList:"

val SB = SparseMatrix.fromTensorList 
             [10,10]
             [
              {tensor=(RTensor.*> 0.2
                               (RTensor.new ([3,8],1.0))),
               offset=[7,0],
               sparse=true},
              
              {tensor=(RTensor.*> 0.1 
                               (RTensor.new ([7,8],1.0))),
               offset=[0,0],
               sparse=true}
              
             ]

val _ = Loop.app
            (0,10,fn (j) => 
                    Loop.app (0,10,fn (i) => 
                                     (
                                      print ("SB(" ^ (Int.toString i) ^ "," ^ (Int.toString j) ^ ") = "); 
                                      TensorFile.realWrite 
                                          (TextIO.stdOut) 
                                          (SparseMatrix.sub (SB,[i,j]))
                                     )
            ))

val _ = Loop.app
            (0,10,fn (i) => 
                    let
                        val _ = putStrLn TextIO.stdOut ("SparseMatrix slice column " ^ (Int.toString i) ^ ": ")
                        val sl = SparseMatrix.slice (SB,1,i) 
                    in
                        SparseMatrix.sliceAppi 
                            (fn (i,x) => putStrLn TextIO.stdOut ("[" ^ (Int.toString i) ^ "]: " ^ (Real.toString x)))
                            sl
                    end)

val SC = SparseMatrix.*> 2.0 SB

val _ = Loop.app
            (0,10,fn (j) => 
                    Loop.app (0,10,fn (i) => 
                                     (
                                      print ("SC(" ^ (Int.toString i) ^ "," ^ (Int.toString j) ^ ") = "); 
                                      TensorFile.realWrite 
                                          (TextIO.stdOut) 
                                          (SparseMatrix.sub (SC,[i,j]))
                                     )
            ))


val SAL  = SparseMatrix.fromList
              [6,6] 
              ([
                  (0, [(0,10.0),(1,3.0),(3,3.0)]),
                  (1, [(1,9.0),(2,7.0),(4,8.0),(5,4.0)]),
                  (2, [(2,8.0),(3,8.0)]),
                  (3, [(2,7.0),(3,7.0),(4,9.0)]),
                  (4, [(0,~2.0),(3,5.0),(4,9.0),(5,2.0)]),
                  (5, [(1,3.0),(4,13.0),(5,~1.0)])
              ], [6,6], NONE)


val _ = Loop.app
            (0,6,fn (j) => 
                    Loop.app (0,6,fn (i) => 
                                     (
                                      print ("SAL(" ^ (Int.toString i) ^ "," ^ (Int.toString j) ^ ") = "); 
                                      TensorFile.realWrite 
                                          (TextIO.stdOut) 
                                          (SparseMatrix.sub (SAL,[i,j]))
                                     )
            ))

val SBL  = SparseMatrix.fromList
               [10,10] 
               ([
                   (0, [(0,10.0),(1,2.0),(3,3.0)]),
                   (1, [(1,9.0),(2,7.0)]),
                   (2, [(2,6.0),(3,8.0)]),
                   (3, [])
               ], [4,4], NONE)

val _ = Loop.app
            (0,10,fn (j) => 
                    Loop.app (0,10,fn (i) => 
                                     (
                                      print ("SBL(" ^ (Int.toString i) ^ "," ^ (Int.toString j) ^ ") = "); 
                                      TensorFile.realWrite 
                                          (TextIO.stdOut) 
                                          (SparseMatrix.sub (SBL,[i,j]))
                                     )
            ))

val SBLL = SparseMatrix.fromLists 
               [10,10]
               [
                 {l=[(0, [(0,10.0),(1,2.0),(3,3.0)]),
                     (1, [(1,9.0),(2,7.0)]),
                     (2, [(2,6.0),(3,8.0)])],
                  shape_l=[4,4],
                  offset=[4,0]}
               ]

val _ = Loop.app
            (0,10,fn (j) => 
                    Loop.app (0,10,fn (i) => 
                                     (
                                      print ("SBLL(" ^ (Int.toString i) ^ "," ^ (Int.toString j) ^ ") = "); 
                                      TensorFile.realWrite 
                                          (TextIO.stdOut) 
                                          (SparseMatrix.sub (SBLL,[i,j]))
                                     )
            ))

