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
  [set-ppu-control [ppu-control 0 0 0 0 0 0 1]]             ; Initialize the ppu control register
  [set-ppu-mask [ppu-mask "sprites"]]                       ; Initialize the ppu mask register
  [vblank 1]                                                ; wait for vblank
  [clear-ram]                                               ; Clear the 2k working ram}
  [set-palette "paletteData"]
  [call "setSpriteTile" {#0} {#2}]
  [call "setSpritePos" {#0} {#$80} {#$80}]
  
  [vblank 1 "forever"]

  jmp forever

  paletteData:
   `$0F `$31 `$32 `$33 `$0F `$35 `$36 
   `$37 `$0F `$39 `$3A `$3B `$0F `$3D 
   `$3E `$0F `$0F `$1C `$15 `$14 `$0F 
   `$02 `$38 `$3C `$0F `$1C `$15 `$14 
   `$0F `$02 `$38 `$3C

NMI:
  [dma-sprites $0200]
  rti

.include "nes_subs.asm"

.org $FFFA                                                  ; Setup the vectors for this program
.addr NMI
.addr RESET
.addr 0

.segment $2000 $0000                                        ; Data Segment
.incbin "mario.chr"
