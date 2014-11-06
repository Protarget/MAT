.import "nes.asm"
.import "stdmac.asm"

.segment $10 $0000                                      ;Header Segment
[INES 1 1 0 [header-flags 0] false]                     ; Emit an INES header with 1 bank of prg rom, 1 bank of chr rom, and mapper 0 with no flags set, NTSC

.segment $4000 $C000                                    ;Code Segment
RESET:
  sei
  cld
  [stm {#$40} $4017]
  [stm {#0}   $4010]
  [init-stack]
  [stm {[ppu-control 0 0 0 0 false false false]} $2000] ; Initialize the ppu control register
  [stm {[ppu-mask "green"]} $2001]                      ; Initialize the ppu mask register
  [vblank 1] ;wait for vblank
  [clear-ram]

  [vblank 1 "forever"]                                  ; Acts both as the label "forever" and as a single vblank wait

  [stm {[ppu-mask "red"]} $2001]                        ; Flash the screen red and blue!
  [vblank 3]
  [stm {[ppu-mask "blue"]} $2001]
  [vblank 2]

  jmp forever                                           ; Jump to the vblank label "forever"

NMI:
  rti

.org $FFFA                                              ; Setup the vectors for this program
.addr NMI
.addr RESET
.addr 0

.segment $2000 $0000                                    ;Data Segment
.incbin "mario.chr"