
structure BitTensor : MONO_TENSOR =
struct
    
        structure Array = BitArray
        structure Index  = Index
        type elem = Array.elem
        type index = Index.t
        type tensor = {shape : index, indexer : Index.indexer, data : Array.array}
        type t = tensor
        exception Shape
        exception Match
        exception Index
        exception NotImplemented

    local
    (*----- LOCALS -----*)
        fun make' (shape, data) =
            {shape = shape, indexer = Index.indexer shape, data = data}
        fun toInt {shape, indexer, data} index = indexer index
        fun splitList (l as (a::rest), place) =
            let fun loop (left,here,right) 0 =  (List.rev left,here,right)
                  | loop (_,_,[]) place = raise Index
                  | loop (left,here,a::right) place = 
                loop (here::left,a,right) (place-1)
            in
                if place <= 0 then
                    loop ([],a,rest) (List.length rest - place)
                else
                    loop ([],a,rest) (place - 1)
            end
          | splitList ([], _) = ([],0,[])
    in
    (*----- STRUCTURAL OPERATIONS & QUERIES ------*)

        fun new (shape, init) =
            if not (Index.validShape shape) then
                raise Shape
            else
                let val length = Index.length shape in
                    {shape = shape,
                     indexer = Index.indexer shape,
                     data = Array.array(length,init)}
                end
        fun toArray {shape, indexer, data} = data
        fun length {shape, indexer, data} =  Array.length data
        fun shape {shape, indexer, data} = shape
        fun rank t = List.length (shape t)
        fun reshape new_shape tensor =
            if Index.validShape new_shape then
                case (Index.length new_shape) = length tensor of
                    true => make'(new_shape, toArray tensor)
                  | false => raise Match
            else
                raise Shape
        fun fromArray (s, a) =
            case Index.validShape s andalso 
                 ((Index.length s) = (Array.length a)) of
                 true => make'(s, a)
               | false => raise Shape
        fun fromList (s, a) = fromArray (s, Array.fromList a)
        fun tabulate (shape,f) =
            if Index.validShape shape then
                let val last = Index.last shape
                    val length = Index.length shape
                    val c = Array.array(length, f last)
                    fun dotable (c, indices, i) =
                        (Array.update(c, i, f indices);
                         if i <= 1
                         then c
                         else dotable(c, Index.prev' shape indices, i-1))
                in make'(shape,dotable(c, Index.prev' shape last, length-2))
                end
            else
                raise Shape



      fun cat (x: tensor, y: tensor, dim) =
        (let val xshape = (#shape x)
             val yshape = (#shape y)
             val xdata  = (#data x)
             val ydata  = (#data y)
        in
           if  not (rank x  = rank y) then
           raise Shape
           else
                let 
                   val (_,newshape)   = ListPair.foldr
                                      (fn (x,y,(i,ax)) => if (dim = i) then (i-1,(x+y) :: ax) 
                                                                       else if not (x=y) then raise Shape else (i-1,x :: ax))
                                       ((rank x)-1,[]) (xshape, yshape)
                   val newlength  = Index.length newshape 
                   val newdata    = Array.array(newlength,Array.sub(xdata,0))
                in
                    Array.copy {src=xdata,dst=newdata,di=0};
                    Array.copy {src=ydata,dst=newdata,di=(Index.length xshape)};
                    {shape = newshape,
                     indexer = Index.indexer newshape,
                     data = newdata}
                end
        end)


      fun prepad (x: tensor, len, c, dim) =
          (let val xshape = (#shape x)
               val xdata  = (#data x)
           in
               if (rank x) <= dim then
                   raise Shape
               else
                   let 
                       val (_,newshape)   = List.foldr
                                                (fn (x,(i,ax)) => 
                                                    if (dim = i) then (i-1,len :: ax)
                                                    else (i-1,x :: ax))
                                                ((rank x)-1,[]) xshape
                   in
                       cat (new (newshape, c), x, dim)
                   end
           end)

      fun postpad (x: tensor, len, c, dim) =
          (let val xshape = (#shape x)
               val xdata  = (#data x)
           in
               if (rank x) <= dim then
                   raise Shape
               else
                   let 
                       val (_,newshape)   = List.foldr
                                                (fn (x,(i,ax)) => 
                                                    if (dim = i) then (i-1,len :: ax)
                                                    else (i-1,x :: ax))
                                                ((rank x)-1,[]) xshape
                   in
                       cat (x, new (newshape, c), dim)
                   end
           end)



        (*----- ELEMENTWISE OPERATIONS -----*)
        fun sub (t, index) = Array.sub(#data t, toInt t index)
        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)
        fun map f {shape, indexer, data} =
            let
                val data' = Array.array (Array.length data, Array.sub(data, 0))
                val _ = Array.copy {src=data,dst=data',di=0}
                val _ = Array.modify f data'
            in
                {shape = shape, indexer = indexer, data = data'}
            end
            
        fun map2 f t1 t2= raise NotImplemented

        fun appi f tensor = 
            let 
                val shape = shape tensor
                val next = Index.next shape
            in
                (Array.foldl 
                     (fn (v,i) => (f (i,v); valOf (next i)))
                     (Index.first shape) 
                     (toArray tensor); ())
            end
        fun app f tensor = Array.app f (toArray tensor)
        fun all f tensor =
            let val a = toArray tensor
            in Loop.all(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun any f tensor =
            let val a = toArray tensor
            in Loop.any(0, length tensor - 1, fn i =>
                        f (Array.sub(a, i)))
            end
        fun foldl f init tensor = Array.foldl f init (toArray tensor)

        fun foldln f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Array.update(c, ic, f(Array.sub(c,ic), Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
        (* --- POLYMORPHIC ELEMENTWISE OPERATIONS --- *)
        fun array_map' f a =
            let fun apply index = f(Array.sub(a,index)) in
                Tensor.Array.tabulate(Array.length a, apply)
            end
        fun map' f t = Tensor.fromArray(shape t, array_map' f (toArray t))
        fun map2' f t1 t2 =
            let val d1 = toArray t1
                val d2 = toArray t2
                fun apply i = f (Array.sub(d1,i), Array.sub(d2,i))
                val len = Array.length d1
            in
                if Index.eq(shape t1, shape t2) then
                    Tensor.fromArray(shape t1, Tensor.Array.tabulate(len,apply))
                else
                    raise Match
            end
        fun foldl' f init {shape, indexer, data=a} index =
            let val (head,lk,tail) = splitList(shape, index)
                val li = Index.length head
                val lj = Index.length tail
                val c = Tensor.Array.array(li * lj,init)
                fun loopi (0, _,  _)  = ()
                  | loopi (i, ia, ic) =
                    (Tensor.Array.update(c,ic,f(Tensor.Array.sub(c,ic),Array.sub(a,ia)));
                     loopi (i-1, ia+1, ic+1))
                fun loopk (0, ia, _)  = ia
                  | loopk (k, ia, ic) = (loopi (li, ia, ic);
                                         loopk (k-1, ia+li, ic))
                fun loopj (0, _,  _)  = ()
                  | loopj (j, ia, ic) = loopj (j-1, loopk(lk,ia,ic), ic+li)
            in
                loopj (lj, 0, 0);
                make'(head @ tail, c)
            end
    end
end

structure BitTensorSlice =
    struct
        structure Tensor = BitTensor
        structure Index  = Tensor.Index
        structure Array  = Tensor.Array
        structure Range  = Range

        type elem   = Array.elem
        type index  = Tensor.Index.t
        type range  = Range.t
        type tensor = BitTensor.tensor

        type slice = {range : range, shapes: index list, tensor : tensor}

        exception EmptySliceRange

        fun fromto (lo,up,tensor) =
            let val r = Range.fromto (Tensor.shape tensor) (lo,up)
            in
                if (Range.length r) = 0
                then raise EmptySliceRange
                else {range=r,
                      shapes=(Range.shapes r),
                      tensor=tensor}
            end

        fun fromto' (lo,up,tensor) =
            let val r = Range.fromto' (Tensor.shape tensor) (lo,up)
            in
                if (Range.length r) = 0
                then raise EmptySliceRange
                else {range=r,
                      shapes=(Range.shapes r),
                      tensor=tensor}
            end


        fun slice (rs,tensor) =
            let 
                val r = (Range.ranges (Tensor.shape tensor) rs)
            in
                {range=r,
                 shapes=(Range.shapes r),
                 tensor=tensor}
            end

        fun length ({range, shapes, tensor})  = Range.length range
        fun base ({range, shapes, tensor})    = tensor
        fun shapes ({range, shapes, tensor}) = shapes
        fun range ({range, shapes, tensor})   = range

        fun map f slice = 
        let
           val te    = base slice
           val ra    = range slice
           val fndx  = Range.first ra
           val len   = length (slice)
           val arr   = Array.array(len, f (Tensor.sub(te,fndx)))
           val i     = ref 0
        in 
           
           Range.iteri (fn (ndx) => 
                         let val v = f (Tensor.sub (te,ndx)) in (Array.update (arr, !i, v); i := (!i + 1); true) end) ra;
           Tensor.fromArray ([1,len], arr)
        end

        fun app f (slice: slice) = 
        let
           val te    = base slice
           val ra    = range slice
           val fndx  = Range.first ra
        in 
           Range.iteri (fn (ndx) => (f (Tensor.sub (te,ndx)); true)) ra; ()
        end

        fun map2 f (sl1: slice) (sl2: slice) = 
        let
           val _      = if not ((shapes sl1) = (shapes sl2)) then raise Index.Shape else ()
           val te1    = base sl1
           val te2    = base sl2
           val ra1    = range sl1
           val ra2    = range sl2
           val len    = length sl1
           val fndx1  = Range.first ra1
           val fndx2  = Range.first ra2
           val arr    = Array.array (length(sl1), f (Tensor.sub(te1,fndx1), Tensor.sub(te2,fndx2)))
           val i      = ref 0
        in 
           Range.iteri2 (fn (ndx,ndx') => 
                            let 
                                val v = f (Tensor.sub (te1,ndx),Tensor.sub (te2,ndx')) 
                            in 
                                (Array.update (arr, !i, v); i := (!i + 1); true) 
                            end)
                        (ra1,ra2);
           Tensor.fromArray ([1,len], arr)
        end

        fun foldl f init (slice: slice) = 
        let

           val te     = base slice
           val sh     = Tensor.shape te
           val arr    = Tensor.toArray te
           val ra     = range slice
                                                           
        in 
            Range.foldi_range
                (fn ((i,j),ax) => 
                    (Loop.foldi (Index.toInt sh i, (Index.toInt sh j)+1,
                                 fn (n,ax) => f (Array.sub (arr,n),ax), 
                                 ax)))
                init ra
        end


    end                                

