.include "stdmac.asm"

lda #0 sta 0
lda #10 sta 1
[for-x "x-loop" {0} {1} {
}]