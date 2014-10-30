[macro scratch-register [] [addr $ff00]]
[macro declare [name address] [merge {[macro} [sym name] {[] [addr} [addr address] {]} {]}]]
[macro set [name value] [merge {lda} value {sta [} [sym name] {]}]]
[macro for-x [from to code] [merge {ldx} from [label [merge "forx" [id]]] [expand code] {inx cpx} to {bne} [sym [merge "forx" [id]]]]] ;for loop on x;
[macro for-y [from to code] [merge {ldy} from [label [merge "fory" [id]]] [expand code] {iny cpy} to {bne} [sym [merge "fory" [id]]]]] ;for loop on y;
[macro with-x [f] [merge {stx} [scratch-register] f [scratch-register]]]
[macro with-y [f] [merge {sty} [scratch-register] f [scratch-register]]]
[macro with-a [f] [merge {sta} [scratch-register] f [scratch-register]]]
[macro repeat [n f] [if [equal n 0] {} [merge f [repeat [- n 1] f]]]]