[macro declare [name address] [merge {[macro} [sym name] {[] [addr} [addr address] {]} {]}]]
[macro set [name value] [merge {lda} value {sta [} [sym name] {]}]]
[macro for-x [from to code] [merge {ldx} from [label [merge "forx" [id]]] [expand code] {inx cpx} to {bne} [sym [merge "forx" [id]]]]] ;for loop on x;
[macro for-y [from to code] [merge {ldy} from [label [merge "fory" [id]]] [expand code] {iny cpy} to {bne} [sym [merge "fory" [id]]]]] ;for loop on y;

[macro move16 [from to] 
  [merge 
    {lda} [addr from] 
    {sta} [addr to] 
    {lda} [addr [+ from 1]] 
    {sta} [addr [+ to 1]]]]

[macro add16 [from to result]
  [let i [id]
    [merge
      {lda} [addr from] 
      {adc} [addr to]
      {sta} [addr result]
      {bcc} [sym [merge "adder" i]]
      {clc}
      {lda} [addr [+ from 1]]
      {adc} [addr [+ to 1]]
      {sta} [addr [+ 1 result]]
      [label [merge "adder" i]]]]]