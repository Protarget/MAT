.import "stdmac.asm"

; Macros for use with the Nintender Entertainment System
; Create an INES header at binary location 0
; Then resets the logical address to 0 
[macro INES [] {
  .org $0
  [data-string "NES"] `$1A
  `$01 `$01 `$00 `$00
  `$00 `$00 `$00 `$00 `$00 `$00 `$00 `$00
  .map $0
}]