; Adds global variable support using recursive macros
; declare "name" address will designate a RAM address as a global variable. 
; declare creates the macro [name] which returns the address of the variable
; set "name" value where value is a token literal will allow you to set the value
; e.g: set "var" {$100} will transfer the value at address $100 to var
;      set "var" {#100} will transfer a literal value to var
[macro declare [name address] [merge {[macro} [sym name] {[] [addr} [addr address] {]} {]}]]
[macro set [name value] [merge {lda} value {sta [} [sym name] {]}]]

; An easy way to declare named sections in the assembled file
; [define-section name] will create a macro called [section-name] that
; is a thin wrapper over ".org o .map m"
[macro define-section [name o m]
  [merge
    {[macro} 
    [sym [merge "section-" name]]
    {[] [merge {.org} [addr}
    [addr o]
    {] {.map} [addr}
    [addr m]
    {]]]}]]

[macro data-string [n] 
  [if [empty n]
    {}
    [merge [byte [ord [head n]]] [data-string [tail n]]]]]

; Macros making use of a scratch register in order to allow registers to be
; used as operands that don't normally support them
; e.g: [with-x {adc}] will add the accumulator to X
[macro scratch-register [] [addr $ff00]] 
[macro with-x [f] [merge {stx} [scratch-register] f [scratch-register]]]
[macro with-y [f] [merge {sty} [scratch-register] f [scratch-register]]]
[macro with-a [f] [merge {sta} [scratch-register] f [scratch-register]]]

; Aliases on comparison functions
[macro == [a b] [equal a b]]
[macro > [a b] [greater a b]]
[macro < [a b] [lesser a b]]
[macro >= [a b] [or [greater a b] [equal a b]]]
[macro <= [a b] [or [lesser a b] [equal a b]]]

; Predefined lambdas for use with higher-order macros like 'for'
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

; Complex static loop macro
; index: the starting index of the loop
; update: how to update the loop
; terminate: the condition the loop should terminate under
; act: The operation to apply at each index
[macro for [index update terminate combine act]
  [if [terminate index]
    [act index]
    [combine [act index] [for [update index] update terminate combine act]]]]

