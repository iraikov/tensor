
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


structure DNF =
struct


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


fun fromDiag (m, n, a, dflt) =
    if Index.validShape [m,n]
    then 
        (let 
             val na   = RTensor.Array.length a
             val na'  = na-1
             val te = RTensor.new ([m,n], dflt)
             fun diag (i, j, ia) =
                 let
                     val ia' = 
                         (RTensor.update (te, [i,j], RTensor.Array.sub (a, ia));
                          if ia = na' then 0 else ia+1)
                 in
                     if (i=0) orelse (j=0) 
                     then te
                     else diag (i-1, j-1, ia)
                 end
         in
             diag (m-1, n-1, 0)
         end)
    else 
        raise RTensor.Shape

exception Shape
exception Index

(* Generate a tensor containing a disc. *)
fun disc2D (shape, center, radius) =
    let
        val (M,N) = case shape of
                        [M,N] => (M,N)
                      | _     => raise Shape

        val (cx,cy) = case center of 
                          NONE => (Real.fromInt (M div 2), Real.fromInt (N div 2))
                        | SOME [cx,cy] => (cx,cy)
                        | _ => raise Index

        fun sqr (x) = Real.*(x,x)

        fun dist2D (x,y) =
            let 
                open Real
            in
                Math.sqrt(sqr(x-cx) + sqr(y-cy))
            end

        val D = RTensor.tabulate (shape,fn (idx) => 
                                           case idx of 
                                               [x,y] => dist2D (Real.fromInt x, Real.fromInt y) 
                                             | _ => raise Index)
    in
        RTensor.map (fn (x) => if Real.<= (x, radius) then x else 0.0) D
    end


end
