.import "nes.asm"
.import "stdmac.asm"

.segment $10 $0000 ;Header Segment
[INES 1 1 0 [hflags 0] false]

.segment $4000 $C000 ;Code Segment
RESET:
  sei
  cld
  ldx #$40
  stx $4017
  ldx #$ff
  txs
  inx
  stx $2000
  stx $2001
  stx $4010

  [vblank]

clrmem:
  LDA #$00
  [for $0000 [increment $100] [x>= $0700] [->merge<-] [%[n] [merge {sta} [addrx n]]]]
  INX
  BNE clrmem
   
  [vblank]

  lda [ppu-options "green"]
  sta $2001

Forever:
  JMP Forever     ;jump back to Forever, infinite loop

NMI:
  RTI

.org $FFFA
.addr NMI
.addr RESET
.addr 0


.segment $2000 $0000 ;Data Segment
.incbin "mario.chr"