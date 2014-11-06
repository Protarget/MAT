.import "stdmac.asm"

; Macros for use with the Nintender Entertainment System
; Generate the INES header data
[macro INES [prgrom chrrom prgram flgs pal]
  [merge
    { [data-string "NES"] `$1A } 
    [byte prgrom] [byte chrrom] 
    {.addr} [addr flgs]
    [byte prgram]
    [byte [if pal 1 0]]
    { `$00 `$00 `$00 `$00 `$00 `$00 }]]

[macro flags [h g f e d c b a]
  [bor
    [if a 1 0]
    [if b 2 0]
    [if c 4 0]
    [if d 8 0]
    [if e 16 0]
    [if f 32 0]
    [if g 64 0]
    [if h 128 0]]]

[macro flags16 [p o n m l k j i h g f e d c b a]
  [bor
    [flags h g f e d c b a]
    [if i 256 0]
    [if j 512 0]
    [if k 1024 0]
    [if l 2048 0]
    [if m 4096 0]
    [if n 8192 0]
    [if o 16384 0]
    [if p 32768 0]]]

[macro includes [l v]
  [if [empty l] 
    false 
    [if [== [head l] v]
      true
      [includes [tail l] v]]]]

[macro ppu-options [_] 
  [flags 
    [includes args "blue"] 
    [includes args "green"] 
    [includes args "red"] 
    [includes args "sprites"] 
    [includes args "bg"] 
    [includes args "nospriteclip"] 
    [includes args "nobgclip"] 
    [includes args "grayscale"]]]

[macro hflags [mapperNumber _]
  [flags16
    [> [band mapperNumber 128] 0]
    [> [band mapperNumber 64] 0]
    [> [band mapperNumber 32] 0]
    [> [band mapperNumber 16] 0]
    false
    false
    [includes args "playchoice"] 
    [includes args "unisystem"]
    [> [band mapperNumber 8] 0]
    [> [band mapperNumber 4] 0]
    [> [band mapperNumber 2] 0]
    [> [band mapperNumber 1] 0]
    [includes args "fourscreen"] 
    [includes args "trainer"] 
    [includes args "battery"] 
    [includes args "hmirror"]]]

; Creates a tight loop waiting for vertical blank
[macro vblank [] {
  [merge [label [merge "vblank_" [id]]] {BIT $2002 BPL} [sym [merge "vblank_" [id]]]]
}]