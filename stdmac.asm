[macro scratch-register [] [addr $ff00]]
[macro declare [name address] [merge {[macro} [sym name] {[] [addr} [addr address] {]} {]}]]
[macro set [name value] [merge {lda} value {sta [} [sym name] {]}]]
[macro for-x [from to code] [merge {ldx} from [label [merge "forx" [id]]] [expand code] {inx cpx} to {bne} [sym [merge "forx" [id]]]]] ;for loop on x;
[macro for-y [from to code] [merge {ldy} from [label [merge "fory" [id]]] [expand code] {iny cpy} to {bne} [sym [merge "fory" [id]]]]] ;for loop on y;
[macro with-x [f] [merge {stx} [scratch-register] f [scratch-register]]]
[macro with-y [f] [merge {sty} [scratch-register] f [scratch-register]]]
[macro with-a [f] [merge {sta} [scratch-register] f [scratch-register]]]
[macro == [a b] [equal a b]]
[macro > [a b] [greater a b]]
[macro < [a b] [lesser a b]]
[macro >= [a b] [or [greater a b] [equal a b]]]
[macro <= [a b] [or [lesser a b] [equal a b]]]

; Predefined lambdas for use with higher-order macros like 'for' ;
[macro x== [y] [lambda [x] [== x y]]]
[macro x> [y] [lambda [x] [> x y]]]
[macro x< [y] [lambda [x] [< x y]]]
[macro x>= [y] [lambda [x] [>= x y]]]
[macro x<= [y] [lambda [x] [<= x y]]]
[macro increment [n] [lambda [x] [+ x n]]]
[macro decrement [n] [lambda [x] [- x n]]]
[macro ->merge<- [] [lambda [a b] [merge a b]]]
[macro ->sum<- [] [lambda [a b] [+ a b]]]
[macro ->discard<- [] [lambda [a b] b]]

; Complex static loop macro ;
; index: the starting index of the loop ;
; update: how to update the loop ;
; terminate: the condition the loop should terminate under ;
; act: The operation to apply at each index ;
[macro for [index update terminate combine act]
  [if [terminate index]
    [act index]
    [combine [act index] [for [update index] update terminate combine act]]]]

