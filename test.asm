.include "stdmac.asm"
[define-section "code" $100 $100]
[define-section "data" $D000 $D000]
[macro terminate [] {lda #1 sta $fff0}]

; Static print "wraps" the text data to be printed
; inside of the code printing it. This allows simplified
; static output of text
[macro static-print [text]
  [let v [id]
    [merge 
      {ldx #0}
      [label [merge "sprint" v]]
      [merge {lda} [labelx [merge "data" v]]]
      {sta $fff1 inx}
      [merge {lda} [labelx [merge "data" v]]]
      {cmp #0 beq} [sym [merge "terminate" v]]
      {jmp} [sym [merge "sprint" v]]
      [label [merge "data" v]]
      [data-string text]
      [byte 10]
      [byte 0]
      [label [merge "terminate" v]]]]]

[section-code]
LDA 32
[terminate]