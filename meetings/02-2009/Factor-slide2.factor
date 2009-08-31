Binary tree example:

USING: random ;

TUPLE: node left right data ;

: <node> node boa ;

: make-random-tree ( depth -- tree )
    dup 0 >
    [ 1 - dup [ make-random-tree ] bi@ 1000 random <node> ]
    [ drop f ]
        if ;

How does it work?

: make-random-tree ( depth -- tree )
    dup 0 >
    [ true-branch.. ]
    [ false-branch.. ]
        if ;

What does the true-branch do?

[ 1 - dup [ make-random-tree ] bi@ 1000 random <node> ]

1 -                               Subtract 1 from depth.
dup [ make-random-tree ] bi@      Push onto the stack two random trees with this depth.
1000 random                       Push onto the stack a random number from 0 to 999.
<node>                            Construct a node, setting its slots from the 3 values on the stack.

What does the false-branch do?

drop                              Throw away the depth parameter.
f                                 Push onto the stack the empty tree.


