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

[macro init-stack [] {ldx #$ff txs}]

[macro clear-ram [] [merge
  {ldx #0}
  {lda #0}
  [label [merge "clear_ram" [id]]]
  [for $0000 [increment $100] [x>= $0700] [->merge<-] [%[n] [merge {sta} [addrx n]]]]
  {inx}
  {bne} [sym [merge "clear_ram"[id]]]]]

[macro set-ppu-control [flags]
  [merge
    {lda} [lit flags] {sta $2000}]]

[macro set-ppu-mask [flags]
  [merge
    {lda} [lit flags] {sta $2001}]]

[macro set-apu-frame-counter [flags]
  [merge
    {lda} [lit flags] {sta $4017}]]

[macro set-apu-dmc-control [flags]
  [merge
    {lda} [lit flags] {sta $4010}]]

[macro ppu-control [nametable-addr vram-addr sprite-addr pat-addr large-sprites master-enabled nmi-enabled]
  [flags
    [> nmi-enabled 0]
    [> master-enabled 0]
    [> large-sprites 0]
    [> pat-addr 0]
    [> sprite-addr 0]
    [> vram-addr 0]
    [> [band 2 nametable-addr] 0]
    [> [band 1 nametable-addr] 0]]]

[macro ppu-mask [_] 
  [flags 
    [includes args "blue"] 
    [includes args "green"] 
    [includes args "red"] 
    [includes args "sprites"] 
    [includes args "bg"] 
    [includes args "nospriteclip"] 
    [includes args "nobgclip"] 
    [includes args "grayscale"]]]

[macro apu-frame-counter [upstep irq-enabled]
  [flags
    upstep
    irq-enabled
    false
    false
    false
    false
    false
    false]]

[macro apu-dmc-control [rate-index looping irq-enabled]
  [flags
    irq-enabled
    looping
    false
    false
    [> [band 8 rate-index] 0]
    [> [band 4 rate-index] 0]
    [> [band 2 rate-index] 0]
    [> [band 1 rate-index] 0]]]

[macro header-flags [mapperNumber _]
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

; Creates a tight loop waiting for a number of vertical blank
[macro vblank [blank-count _] 
  [let vblank-label [if [== 1 [length args]] [head args] [merge "vblank_" [id]]]
    [if [> blank-count 1]
    [merge
      {ldx} [lit blank-count]
      [label vblank-label]
      [label [merge "inner_"  vblank-label]]
      {bit $2002 bpl} [sym vblank-label]
      {dex}
      {bne} [sym [merge "inner_"  vblank-label]]]
    [merge
      [label vblank-label]
      {bit $2002 bpl} [sym vblank-label]]]]]


; Set the palette data from a memory address
[macro set-palette [palette-data-address]
  [merge
    {ldx $2002} ; Reset the ppu latch
    {ldx #$3F} ; Load address high
    {stx $2006}
    {ldx #$10} ; Load address low
    {stx $2006}
    {ldx #$00}
    [label [merge "palette_loop_" [id]]] ;Copy 32 bytes of data into memory
    {lda} [addr-or-label-x palette-data-address] ; Emit a string,X token if label, or an addr,X token if number
    {sta $2007}
    {inx}
    {cpx #$20}
    {bne} [sym [merge "palette_loop_" [id]]]]]

[macro dma-sprites [start-address]
  [merge
    {lda} [lit [band start-address $00FF]]
    {sta $2003}
    {lda} [lit [>> [band start-address $FF00] 8]]
    {sta $4014}]]

[macro set-sprite-pos [base n x y]
  [merge
    {lda} y
    {sta} [addr [+ [* 4 n] base 0]]
    {lda} x
    {sta} [addr [+ [* 4 n] base 3]]]]

[macro set-sprite-tile [base n v]
  [merge
    {lda} v
    {sta} [addr [+ [* 4 n] base 1]]]]

[macro set-sprite-palette [base n v]
  [merge
    {lda} v
    {sta} [addr [+ [* 4 n] base 2]]]]
