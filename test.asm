.import "nes.asm"
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
  STA $0000,X
  STA $0100,X
  STA $0200,X
  STA $0400,X
  STA $0500,X
  STA $0600,X
  STA $0700,X
  LDA #$FE
  STA $0300,X
  INX
  BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
  BIT $2002
  BPL vblankwait2


  LDA #128   ;intensify blues
  STA $2001

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