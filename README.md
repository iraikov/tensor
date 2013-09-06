tensor
======

Standard ML tensor/multidimensional array library.

INDEX         -Signature-

Indices are a enumerable finite set of data with an order and a map to
a continous nonnegative interval of integers.  In this implementation
each index is a list of integers,

        [i1,...,in]

and each set of indices is defined by a shape, which has the same
shape of an index but with each integer incremented by one

        shape = [k1,...,kn]
        0 < i1 < k1

type storage = RowMajor | ColumnMajor
order : storage

        Identifies:
                1) the underlying algorithms for this structure
                2) the most significant index
                3) the index that varies more slowly
                4) the total order
        RowMajor means that first index is most significant and varies
        more slowly, while ColumnMajor means that last index is the most
        significant and varies more slowly. For instance
                RowMajor => [0,0]<[0,1]<[1,0]<[1,1] (C, C++, Pascal)
                ColumnMajor => [0,0]>[1,0]>[0,1]>[1,1] (Fortran)

last shape
first shape
        Returns the last/first index that belongs to the sed defined by
        'shape'.

inBounds shape index
        Checkes whether 'index' belongs to the set defined by 'shape'.

toInt shape index
        As we said, indices can be sorted and mapped to a finite set of
        integers. 'toInt' obtaines the integer number that corresponds to
        a certain index.

indexer shape
        It is equivalent to the partial evaluation 'toInt shape' but
        optimized for 'shape'.

next shape index
prev shape index
next' shape index
prev' shape index
        Obtain the following or previous index to the one we supply.
        next and prev return an object of type 'index option' so that
        if there is no such following/previous, the output is NONE.
        On the other hand, next'/prev' raise an exception when the
        output is not well defined and their output is always of type
        index. next/prev/next'/prev' raise an exception if 'index'
        does not belong to the set of 'shape'.

all shape f
any shape f
app shape f
        Iterates 'f' over every index of the set defined by 'shape'.
        'all' stops when 'f' first returns false, 'any' stops when
        'f' first returns true and 'app' does not stop and discards the
        output of 'f'.

compare(a,b)
        Returns LESS/GREATER/EQUAL according to the total order which
        is defined in the set of all indices.
  <,>,eq,<=,>=,<>
        Reduced comparisons which are defined in terms of 'compare'.

validShape t
validIndex t
        Checks whether 't' conforms a valid shape or index.

iteri shape f


TENSOR

Polymorphic tensors of any type. With 'tensor' we denote a (mutable)
array of any rank, with as many indices as one wishes, and that may
be traversed (map, fold, etc) according to any of those indices.

type 'a tensor
        Polymorphic tensor whose elements are all of type 'a.

val storage = RowMajor | ColumnMajor
        RowMajor = data is stored in consecutive cells, first index
        varying fastest (FORTRAN convention)
        ColumnMajor = data is stored in consecutive cells, last
        index varying fastest (C,C++,Pascal,CommonLisp convention)

new ([i1,...,in],init)
        Build a new tensor with n indices, each of sizes i1...in,
        filled with 'init'.

fromArray (shape,data)
fromList (shape,data)
        Use 'data' to fill a tensor of that shape. An exception is
        raised if 'data' is too large or too small to properly
        fill the vector. Later use of a 'data' array is disregarded
        -- one must think that the tensor now owns the array.

length tensor
rank tensor
shape tensor
        Return the number of elements, the number of indices and
        the shape (size of each index) of the tensor.

toArray tensor
        Return the data of the tensor in the form of an array.
        Mutation of this array may lead to unexpected behavior.

sub (tensor,[i1,...,in])
update (tensor,[i1,...,in],new_value)
        Access the element that is indexed by the numbers [i1,..,in]

app f a
appi f a
        The same as 'map' and 'mapi' but the function 'f' outputs
        nothing and no new array is produced, i.e. one only seeks
        the side effect that 'f' may produce.

map2 operation a b
        Apply function 'f' to pairs of elements of 'a' and 'b'
        and build a new tensor with the output. Both operands
        must have the same shape or an exception is raised.
        The procedure is sequential, as specified by 'storage'.

foldl operation a n
        Fold-left the elements of tensor 'a' along the n-th
        index.

all test a
any test a
        Folded boolean tests on the elements of the tensor.

insert a b index
        Inserts b into a starting at the given index
        a and b must be of the same rank, with b smaller than a

cat a b int
        Concatenates a and b along the given axis


