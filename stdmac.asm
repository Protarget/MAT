[macro declare [name address] [merge {[macro} [sym name] {[] [addr} [addr address] {]} {]}]]
[macro set [name value] [merge {lda} value {sta [} [sym name] {]}]]
[macro for-x [lab from to code] [merge {ldx} from [label lab] [expand code] {inx cpx} to {bne} [sym lab]]] ;for loop on x;
[macro for-y [lab from to code] [merge {ldy} from [label lab] [expand code] {iny cpy} to {bne} [sym lab]]] ;for loop on y;