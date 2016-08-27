
structure RawTensor =
struct
    
        structure Array = RawArray
        structure Index  = Index
        type index = Index.t
        type 'a tensor = {shape : index, indexer : Index.indexer, data : 'a Array.array}
        type 'a t = 'a tensor
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


    (*----- ELEMENTWISE OPERATIONS -----*)
        fun sub (t, index) = Array.sub(#data t, toInt t index)
        fun update (t, index, value) =
            Array.update(toArray t, toInt t index, value)

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

    end
end
