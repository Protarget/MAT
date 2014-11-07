.import "nes.asm"                                           ; Import the standard macro package and the NES macro package
.import "stdmac.asm"

.segment $10 $0000                                          ; Header Segment
[INES 1 1 0 [header-flags 0] false]                         ; Emit an INES header with 1 bank of prg rom, 1 bank of chr rom, and mapper 0 with no flags set, NTSC

.segment $4000 $C000                                        ; Code Segment
RESET:
  sei
  cld
  [set-apu-frame-counter [apu-frame-counter false false]]   ; Initialize the audio registers
  [set-apu-dmc-control [apu-dmc-control 0 true false]]
  [init-stack]
  [set-ppu-control [ppu-control 0 0 0 0 0 0 0]]             ; Initialize the ppu control register
  [set-ppu-mask [ppu-mask]]                                 ; Initialize the ppu mask register
  [vblank 1]                                                ; wait for vblank
  [clear-ram]                                               ; Clear the 2k working ram

  [vblank 1 "forever"]                                      ; Acts both as the label "forever" and as a single vblank wait

  [set-ppu-mask [ppu-mask "red"]]                           ; Flash the screen red, blue and green with 30 vblanks between each!
  [vblank 30]
  [set-ppu-mask [ppu-mask "blue"]]
  [vblank 30]
  [set-ppu-mask [ppu-mask "green"]]
  [vblank 29]
  jmp forever                                               ; Jump to the vblank label "forever"

NMI:
  rti

.org $FFFA                                                  ; Setup the vectors for this program
.addr NMI
.addr RESET
.addr 0

.segment $2000 $0000                                        ; Data Segment
.incbin "mario.chr"