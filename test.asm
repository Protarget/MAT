.include "stdmac.asm"

[declare "x" $ff00]

[for-x {#0} {#1} {
  [for-y {#2} {#4} {
    stx $ff01
    [set "x" {$ff01}]
  }]
}]