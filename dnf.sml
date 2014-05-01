
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

fun sqr (x) = Real.*(x,x)


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

fun dist2D (dx,dy) =
    let 
        open Real
    in
        Math.sqrt(sqr(dx) + sqr(dy))
    end

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



        val D = RTensor.tabulate
                    (shape,
                     fn (idx) => 
                        case idx of 
                            [x,y] => dist2D ((Real.fromInt x)-cx, (Real.fromInt y)-cy) 
                          | _ => raise Index)
    in
        RTensor.map (fn (x) => if Real.<= (x,radius) then 1.0 else 0.0) D
    end

exception NegativeRadius 
exception InvalidCenter 

(* Peels a 2D array Z into several 'onion rings' of width r. *)
fun peel2D (Z, center, r) =
    let
        val (M,N) = case RTensor.shape Z of
                        [M,N] => (M,N)
                      | _     => raise Shape
        val _ = if Real.<= (r,0.0) then raise NegativeRadius else ()
        val (cx,cy) = case center of 
                          NONE => (Real.fromInt (M div 2), Real.fromInt (N div 2))
                        | SOME [cx,cy] => (cx,cy)
                        | _ => raise Index

        val _ = if Real.>= (cx,Real.fromInt M) orelse Real.>= (cy,Real.fromInt N) 
                   orelse Real.< (cx,0.0) orelse Real.< (cy,0.0) 
                then raise InvalidCenter else ()

        (* Computes the maximum diameter to get number of rings *)
        val dx = Real.max ((Real.fromInt M)-cx,cx)
        val dy = Real.max ((Real.fromInt N)-cy,cy)

        val radius = dist2D (dx,dy)

        val rhalf = Real./(r,2.0)
    in
        Loop.foldi
            (0, 1 + Real.round(Real./ (radius,r)),
             (fn (i, ax) =>
                 let
                     val r1 = Real.* (Real.fromInt i,rhalf)
                     val r2 = Real.* (Real.fromInt (i+1),rhalf)
                     val K  = RTensor.* (RTensor.- (disc2D(RTensor.shape Z,center,Real.* (2.0,r2)),
                                                    disc2D(RTensor.shape Z,center,Real.* (2.0,r1))), Z)
                 in
                     K :: ax
                 end),
             [])
    end
         
end

(*
val d = DNF.disc2D ([10,10], NONE, 1.0)

val _  = TensorFile.realTensorWrite (TextIO.stdOut) d

*)
