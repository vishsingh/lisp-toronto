USING: random locals ;

TUPLE: node left right data ;

: <node> node boa ;

: if2* ( ? x true false -- )
    [ [ swap ] prepose ] dip
    [ swap ] 2dip
    if* ;

: when2* ( ? x true -- )
    [ drop ] if2* ;

: if-positive ( n true false -- )
    [ dup 0 > ] 2dip if ;

: split-node ( node -- left right data )
    [ left>> ] [ right>> ] [ data>> ] tri ;

: make-random-tree ( depth -- tree )
    [ 1 - dup [ make-random-tree ] bi@ 1000 random <node> ]
    [ drop f ]
        if-positive ;

: walk-tree-ordered ( tree quot -- )
    [ [ [  left>> ] dip walk-tree-ordered ]
      [ [  data>> ] dip call ]
      [ [ right>> ] dip walk-tree-ordered ]
      2tri ]
    when2* ;

: collector ( -- reader writer )
    [let | seq! [ { } ] |
        [ seq ]
        [ seq swap suffix seq! ] ] ;

: flatten-tree-ordered ( tree -- seq )
    collector swapd walk-tree-ordered call ;

: add-to-binary-tree ( tree x -- newtree )
    [ [ split-node ] dip 2dup < rot
      [ [ add-to-binary-tree ] [ swapd add-to-binary-tree swap ] if ] dip ]
    [ [ f f ] dip ]
        if2* <node> ;

: seq-to-binary-tree ( seq -- tree )
    f swap [ add-to-binary-tree ] each ;

: binary-sort ( seq -- sorted-seq )
    seq-to-binary-tree flatten-tree-ordered ;
