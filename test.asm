.import "nes.asm"
.import "stdmac.asm"

.segment $10 $0000 ;Header Segment
[INES]

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

vblankwait1:
  bit $2002
  bpl vblankwait1

clrmem:
  LDA #$00
  [for $0000 [increment $100] [x>= $0700] [->merge<-] [%[n] [merge {sta} [addrx n]]]]
  INX
  BNE clrmem

  lda #64
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2

  asl a
  cmp #0
  bne colorloop
  lda #32
  colorloop:

  sta $2001

  ; LDA #128   ;intensify blues
  ; STA $2001
  jmp vblankwait2

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