[macro declare [name address] [merge {[macro} [sym name] {[] [addr} [addr address] {]} {]}]]
[macro set [name value] [merge {lda} value {sta [} [sym name] {]}]]


[declare "x" $215]
[declare "y" $216]
[set "y" {#32}]
[set "x" {#16}]
lda [x]
adc [y]