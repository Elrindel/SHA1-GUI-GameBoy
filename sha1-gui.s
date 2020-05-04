.ROMDMG
.NAME "SHA1-GUI"
.CARTRIDGETYPE 0
.RAMSIZE 0
.COMPUTEGBCHECKSUM
.COMPUTEGBCOMPLEMENTCHECK
.LICENSEECODENEW "00"
.EMPTYFILL $00

.MEMORYMAP
SLOTSIZE $4000
DEFAULTSLOT 0
SLOT 0 $0000
SLOT 1 $4000
.ENDME

.ROMBANKSIZE $4000
.ROMBANKS 2

.BANK 0 SLOT 0

.ENUM $C000
Step         DB         ;C000      : Compteur de rounds
StateA       DS 4       ;C001-C004 : SHA1 A en cours
StateB       DS 4       ;C005-C008 : SHA1 B en cours
StateC       DS 4       ;C009-C00C : SHA1 C en cours
StateD       DS 4       ;C00D-C010 : SHA1 D en cours
StateE       DS 4       ;C011-C014 : SHA1 E en cours
StateT       DS 4       ;C015-C018 : Temporaire pour les opérations sur les State
RollT        DS 4       ;C019-C01C : Temporaire pour certaines opérations 
RoundK       DS 4       ;C01D-C020 : SHA1 K du round en cours
Block        DS 64      ;C021-C060 : SHA1 Block (message à hacher)
Schedule     DS 64      ;C061-C0A0 : Temporaire pour les opérations sur Block
InitA        DS 4       ;C0A1-C0A5 : Sauvegarde SHA1 A à l'initialisation
InitB        DS 4       ;C0A6-C0A9 : Sauvegarde SHA1 B à l'initialisation
InitC        DS 4       ;C0AA-C0AD : Sauvegarde SHA1 C à l'initialisation
InitD        DS 4       ;C0AE-C0B1 : Sauvegarde SHA1 D à l'initialisation
InitE        DS 4       ;C0B2-C0B5 : Sauvegarde SHA1 E à l'initialisation
Reg1         DS 2       ;C0B6-C0B7 : Registre 1
Reg2         DS 2       ;C0B8-C0B9 : Registre 2
VBlankTimer  DB         ;Compteur VBlank pour avoir un timer synchronisé sur l'affichage
KeyLastDir   DB         ;Dernière touche de direction appuyées
KeyLastBut   DB         ;Dernier bouton appuyé
CharTmp      DB         ;Caractère en cours
CharCount    DB         ;Nombre de caractères déjà sélectionnés
CharBuffer   DS 20      ;Enregistrement des caractères sélectionnés
DisplayHash  DB         ;Permet de déclencher l'affichage du hash SHA1
.ENDE

.ORG $0040
call      VBlank
reti

.ORG $0100
nop
jp    start

.ORG $0104
;Nintendo
.db $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C
.db $00,$0D,$00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6
.db $DD,$DD,$D9,$99,$BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC
.db $99,$9F,$BB,$B9,$33,$3E

.ORG $0150
start:
  di
  ld     sp,$FFF4

  xor a
  ldh    ($26),a

waitvbl:
  ldh    a,($44)
  cp     144
  jr     c, waitvbl

  ld     a,%00010001
  ldh    ($40),a

  ld de,104*16                ;de = 1664
  ld hl,$8000+$180            ;hl = $8180
  ld bc,tiles                 ;bc = tiles
loadTile:
  ld a,(bc)                   ;a = (bc)
  ldi (hl),a                  ;(hl) = a, hl += 1
  inc bc                      ;bc += 1
  dec de                      ;de -= 1
  ld a,e                      ;a = e
  or d                        ;a |= b
  jr nz,loadTile              ;Si a != 0 : Jump loadTile

  ld     de,32*32
  ld     hl,$9800
clmap:
  xor    a
  ldi    (hl),a
  dec    de
  ld     a,e
  or     d
  jr     nz,clmap

  ld     hl,$FE00
  ld     b,40*4
clspr:
  ld     (hl),$00
  inc    l
  dec    b
  jr     nz,clspr

  xor    a
  ldh    ($42),a
  ldh    ($43),a


  ;Init var
  xor a
  ld (DisplayHash),a
  ld (CharCount),a
  ld d,20
  ld hl,CharBuffer
initCharBuffer:
  ldi (hl),a
  dec d
  jr nz,initCharBuffer
  ld a,$40
  ld (CharTmp),a

  ld hl,$9840
  ld bc,strText
  ld d,6
loopStrText:
  ld a,(bc)
  ldi (hl),a
  inc bc
  dec d
  jr nz,loopStrText

  ld hl,$98A0
  ld bc,strHash
  ld d,6
loopStrHash:
  ld a,(bc)
  ldi (hl),a
  inc bc
  dec d
  jr nz,loopStrHash

  ld hl,$99C0
  ld bc,strPressUpDown
  ld d,17
loopStrPressUpDown:
  ld a,(bc)
  ldi (hl),a
  inc bc
  dec d
  jr nz,loopStrPressUpDown

  ld hl,$99E0
  ld bc,strPressA
  ld d,17
loopStrPressA:
  ld a,(bc)
  ldi (hl),a
  inc bc
  dec d
  jr nz,loopStrPressA

  ld hl,$9A00
  ld bc,strPressB
  ld d,19
loopStrPressB:
  ld a,(bc)
  ldi (hl),a
  inc bc
  dec d
  jr nz,loopStrPressB

  ld hl,$9A20
  ld bc,strPressStart
  ld d,19
loopStrPressStart:
  ld a,(bc)
  ldi (hl),a
  inc bc
  dec d
  jr nz,loopStrPressStart


  ld     a,%11100100
  ldh    ($47),a
  ldh    ($48),a
  ldh    ($49),a
  ld     a,%10010011
  ldh    ($40),a

  ld     a,%00010000
  ldh    ($41),a
  ld     a,%00000001
  ldh    ($FF),a

  ei

loop:
  call joypadActions

  jr loop

processSha1:
  ld hl,Block               ;hl = Block
  ld bc,CharBuffer          ;bc = CharBuffer
  ld a,(CharCount)          ;a = (CharCount)
  cp 0
  jr z,loopBufferBlockEnd   ;Si a == 0 : Jump loopBufferBlockEnd
  ld e,a                    ;e = a
loopBufferBlock:
  ld a,(bc)                 ;a = (bc)
  ldi (hl),a                ;(hl) = a, hl += 1
  inc bc                    ;bc += 1
  dec e                     ;e -= 1
  jr nz,loopBufferBlock     ;Si e != 0 : Jump loopBufferBlock
loopBufferBlockEnd:
  ld a,$80                  ;a = $80
  ldi (hl),a                ;(hl) = a, hl += 1
  ld a,(CharCount)          ;a = (CharCount)
  ld e,a                    ;e = a
  ld a,62                   ;a = 62
  sub e                     ;a -= e
  ld e,a                    ;e = a
  xor a                     ;a = 0
loopInitBlock:
  ldi (hl),a                ;(hl) = a, hl += 1
  dec e                     ;e -= 1
  jr nz,loopInitBlock       ;Si e != 0 : Jump loopInitBlock
  ld a,(CharCount)          ;a = (CharCount)
  sla a                     ;a <<= 1
  sla a                     ;a <<= 1
  sla a                     ;a <<= 1
  ldi (hl),a                ;(hl) = a, hl += 1
  call sha1

  ld a,1                    ;a = 1
  ld (DisplayHash),a        ;(DisplayHash) = a

  ret

joypadActions:
  ld a,%00100000            ;a = 0b00100000
  ldh ($00),a               ;(FF00) = a
  
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ld b,a                    ;b = a

  and %00001111             ;a &= 0b00001111
  xor %00001111             ;a ^= 0b00001111
  jr nz,notKeyDirRefresh    ;Si a != 0 : Jump notKeyDirRefresh
  xor a                     ;a = 0
  ld (KeyLastDir),a         ;(KeyLastDir) = a
  jr notKeyUp               ;Jump notKeyUp
notKeyDirRefresh:

  bit 3,b                   ;Test bit 3 de b : Down
  jr nz,notKeyDown          ;Si bit 3 != 0 : Jump notKeyDown
  ld a,(KeyLastDir)         ;a = (KeyLastDir)
  cp 4
  jr z,notKeyDown           ;Si a == 4 : Jump notKeyDown
  ld a,4                    ;a = 4
  ld (KeyLastDir),a         ;(KeyLastDir) = a
  ld a,(CharTmp)            ;a = (CharTmp)
  dec a                     ;a -= 1
  cp $20
  jr nc,processKeyDown      ;Si a >= 32 : Jump processKeyDown
  ld a,$7E                  ;a = 126
processKeyDown:
  ld (CharTmp),a            ;(CharTmp) = a
notKeyDown:

  bit 2,b                   ;Test bit 2 de b : Up
  jr nz,notKeyUp            ;Si bit 2 != 0 : Jump notKeyUp
  ld a,(KeyLastDir)         ;a = (KeyLastDir)
  cp 3
  jr z,notKeyUp             ;Si a == 3 : Jump notKeyUp
  ld a,3                    ;a = 3
  ld (KeyLastDir),a         ;(KeyLastDir) = a
  ld a,(CharTmp)            ;a = (CharTmp)
  inc a                     ;a += 1
  cp $7F
  jr c,processKeyUp         ;Si a < 127 : Jump processKeyUp
  ld a,$20                  ;a = 32
processKeyUp:
  ld (CharTmp),a            ;(CharTmp) = a
notKeyUp:

  ld a,%00010000            ;a = 0b00010000
  ldh ($00),a               ;(FF00) = a
  
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ldh a,($00)               ;a = (FF00)
  ld b,a                    ;b = a

  and %00001111             ;a &= 0b00001111
  xor %00001111             ;a ^= 0b00001111
  jr nz,notKeyButRefresh    ;Si a != 0 : Jump notKeyButRefresh
  xor a                     ;a = 0
  ld (KeyLastBut),a         ;(KeyLastBut) = a
  jr notKeyB                ;Jump notKeyB
notKeyButRefresh:

  bit 0,b                   ;Test bit 0 de b : A
  jr nz,notKeyA             ;Si bit 0 != 0 : Jump notKeyA
  ld a,(KeyLastBut)         ;a = (KeyLastBut)
  cp 1
  jr z,notKeyA              ;Si a == 1 : Jump notKeyA
  ld a,1                    ;a = 1
  ld (KeyLastBut),a         ;(KeyLastBut) = a
  ld a,(CharCount)          ;a = (CharCount)
  cp 20
  jr z,notKeyA              ;Si a == 20 : Jump notKeyA
  xor d                     ;d = 0
  ld e,a                    ;e = a
  ld hl,CharBuffer          ;hl = CharBuffer
  add hl,de                 ;hl += de
  inc a                     ;a += 1
  ld (CharCount),a          ;(CharCount) = a
  ld a,(CharTmp)            ;a = (CharTmp)
  ld (hl),a                 ;(hl) = a
notKeyA:

  bit 1,b                   ;Test bit 1 de b : B
  jr nz,notKeyB             ;Si bit 1 != 0 : Jump notKeyB
  ld a,(KeyLastBut)         ;a = (KeyLastBut)
  cp 2
  jr z,notKeyB              ;Si a == 2 : Jump notKeyB
  ld a,2                    ;a = 2
  ld (KeyLastBut),a         ;(KeyLastBut) = a
  ld a,(CharCount)          ;a = (CharCount)
  cp 0
  jr z,notKeyB              ;Si a == 0 : Jump notKeyB
  xor d                     ;d = 0
  ld e,a                    ;e = a
  ld hl,CharBuffer          ;hl = CharBuffer
  add hl,de                 ;hl += de
  dec a                     ;a -= 1
  ld (CharCount),a          ;(CharCount) = a
  ld a,$00                  ;a = 0
  ld (hl),a                 ;(hl) = a
notKeyB:

  bit 3,b                   ;Test bit 3 de b : Start
  jr nz,notKeyStart         ;Si bit 3 != 0 : Jump notKeyStart
  ld a,(KeyLastBut)         ;a = (KeyLastBut)
  cp 4
  jr z,notKeyStart          ;Si a == 4 : Jump notKeyStart
  ld a,4                    ;a = 4
  ld (KeyLastBut),a         ;(KeyLastBut) = a
  ld a,(DisplayHash)        ;a = (DisplayHash)
  cp 0
  jr nz,notKeyStart         ;Si a != 0 : Jump notKeyStart
  ld a,4                    ;a = 4
  ld (KeyLastBut),a         ;(KeyLastBut) = a
  call processSha1
notKeyStart:

  ret


VBlank:
  push af
  push bc
  push de
  push hl

  ld a,(DisplayHash)        ;a = (DisplayHash)
  cp 1
  jr nz,tryDisplayHash      ;Si a != 1 : Jump tryDisplayHash
  inc a                     ;a += 1
  ld (DisplayHash),a        ;(DisplayHash) = a

  ld hl,$9860               ;hl = $9860
  ld d,20                   ;d = 20
  xor a                     ;a = 0
loopCleanText:
  ldi (hl),a                ;(hl) = a
  dec d                     ;d -= 1
  jr nz,loopCleanText

  ld hl,$9860               ;hl = $9860
  ld bc,CharBuffer          ;bc = CharBuffer
  ld a,(CharCount)          ;a = (CharCount)
  cp 0
  jr z,displayHashEnd       ;Si a == 0 : Jump displayHashEnd
  ld d,a                    ;d = a
loopDisplayText:
  ld a,(bc)                 ;a = (bc)
  ldi (hl),a                ;(hl) = a, hl += 1
  inc bc                    ;bc += 1
  dec d                     ;d -= 1
  jr nz,loopDisplayText     ;Si d != 0 : Jump loopDisplayText
  jr displayHashEnd         ;Jump displayHashEnd

tryDisplayHash:
  cp 2
  jr nz,displayHashEnd      ;Si a != 2 : Jump displayHashEnd
  ld hl,$98C0               ;hl = $98C0
  ld bc,StateA              ;bc = StateA
  ld d,4                    ;d = 4
  ld e,5                    ;e = 5
  jr loopDisplayHash        ;Jump loopDisplayHash
loopDisplayHash2:
  push bc                   ;Stack bc
  ld d,4                    ;d = 4
  ld bc,24                  ;bc = 24
  add hl,bc                 ;hl += bc
  pop bc                    ;UnStack bc
loopDisplayHash:
  ld a,(bc)                 ;a = (bc)
  srl a                     ;a >>= 1
  srl a                     ;a >>= 1
  srl a                     ;a >>= 1
  srl a                     ;a >>= 1
  cp 10
  jr c,displayHashHex1      ;Si a < 10 : Jump displayHashHex1
  add 7                     ;a += 7
displayHashHex1:
  add $30                   ;a += 48
  ldi (hl),a                ;(hl) = a, hl += 1
  ld a,(bc)                 ;a = (bc)
  and $0F                   ;a &= 15
  cp 10
  jr c,displayHashHex2      ;Si a < 10 : Jump displayHashHex2
  add 7                     ;a += 7
displayHashHex2:
  add $30                   ;a += 48
  ldi (hl),a                ;(hl) = a, hl += 1
  inc bc                    ;bc += 1
  dec d                     ;d -= 1
  jr nz,loopDisplayHash     ;Si d != 0 : Jump loopDisplayHash
  dec e                     ;e -= 1
  jr nz,loopDisplayHash2    ;Si e != 0 : Jump loopDisplayHash2

  xor a                     ;a = 0
  ld (DisplayHash),a        ;(DisplayHash) = a
  jr endVBlank              ;Jump endVBlank
displayHashEnd:

  ld a,(VBlankTimer)        ;a = (VBlankTimer)
  inc a                     ;a += 1
  ld (VBlankTimer),a        ;(VBlankTimer) = a
  ld e,a                    ;e = a

  ld hl,$9800               ;hl = $9800
  ld d,20                   ;d = 20
  ld bc,CharBuffer          ;bc = CharBuffer
displayCharBuffer:
  ld a,(bc)                 ;a = (bc)
  ldi (hl),a                ;(hl) = a, hl += 1
  inc bc                    ;bc += 1
  dec d                     ;d -= 1
  jr nz,displayCharBuffer   ;Si d != 0 : Jump displayCharBuffer

  ld hl,$9800               ;hl = $9800
  ld a,(CharCount)          ;a = (CharCount)
  ld b,0                    ;b = 0
  ld c,a                    ;c = a
  add hl,bc                 ;hl += bc

  xor a                     ;a = 0
  bit 4,e                   ;Test bit 4 de e
  jr z,endBlink             ;Si bit 4 == 0 : Jump endBlink
  ld a,(CharTmp)            ;a = (CharTmp)
endBlink:
  ld (hl),a                 ;(hl) = a

endVBlank:
  pop hl
  pop de
  pop bc
  pop af
  ret


sha1:
  ;Init StateA = 0x67452301
  ld a,$67
  ld (StateA),a
  ld a,$45
  ld (StateA+1),a
  ld a,$23
  ld (StateA+2),a
  ld a,$01
  ld (StateA+3),a

  ;Init StateB = 0xEFCDAB89
  ld a,$EF
  ld (StateB),a
  ld a,$CD
  ld (StateB+1),a
  ld a,$AB
  ld (StateB+2),a
  ld a,$89
  ld (StateB+3),a

  ;Init StateC = 0x98BADCFE
  ld a,$98
  ld (StateC),a
  ld a,$BA
  ld (StateC+1),a
  ld a,$DC
  ld (StateC+2),a
  ld a,$FE
  ld (StateC+3),a

  ;Init StateD = 0x10325476
  ld a,$10
  ld (StateD),a
  ld a,$32
  ld (StateD+1),a
  ld a,$54
  ld (StateD+2),a
  ld a,$76
  ld (StateD+3),a

  ;Init StateE = 0xC3D2E1F0
  ld a,$C3
  ld (StateE),a
  ld a,$D2
  ld (StateE+1),a
  ld a,$E1
  ld (StateE+2),a
  ld a,$F0
  ld (StateE+3),a

sha1next:
  xor a                 ;a = 0
  ld (Step),a           ;(Step) = 0

  ld bc,StateA
  ld hl,InitA
  call movl             ;InitA = StateA

  ld bc,StateB
  ld hl,InitB
  call movl             ;InitB = StateB

  ld bc,StateC
  ld hl,InitC
  call movl             ;InitC = StateC

  ld bc,StateD
  ld hl,InitD
  call movl             ;InitD = StateD

  ld bc,StateE
  ld hl,InitE
  call movl             ;InitE = StateE

  ;Init Stack StateA-E
  ld hl,StateE
  push hl               ;Stack : StateE => ArgE
  ld hl,StateD
  push hl               ;Stack : StateD => ArgD
  ld hl,StateC
  push hl               ;Stack : StateC => ArgC
  ld hl,StateB
  push hl               ;Stack : StateB => ArgB
  ld hl,StateA
  push hl               ;Stack : StateA => ArgA
  jp initRound0         ;Jump initRound0

loopRound:
  ;SP = 0
  ld hl,Reg1            ;hl = Reg1
  pop de                ;de = sp[0] = ArgA, sp = 2
  ld a,d                ;a = d
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,e                ;a = e
  ldi (hl),a            ;(hl) = a, hl += 1
  pop de                ;de = sp[2] = ArgB, sp = 4
  ld a,d                ;a = d
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,e                ;a = e
  ldi (hl),a            ;(hl) = a, hl += 1
  pop de                ;de = sp[4] = ArgC, sp = 6
  pop hl                ;hl = sp[6] = ArgD, sp = 8
  pop bc                ;bc = sp[8] = ArgE, sp = 10
  push hl               ;sp = 8, sp[8] = hl = D
  push de               ;sp = 6, sp[6] = de = C
  ld hl,Reg2            ;hl = Reg2
  ld d,(hl)             ;d = (hl)
  inc hl                ;hl += 1
  ld e,(hl)             ;e = (hl)
  push de               ;sp = 4, sp[4] = de = Reg2 = ArgB
  ld hl,Reg1            ;hl = Reg1
  ld d,(hl)             ;d = (hl)
  inc hl                ;hl += 1
  ld e,(hl)             ;e = (hl)
  push de               ;sp = 2, sp[2] = de = Reg1 = ArgA
  push bc               ;sp = 0, sp[0] = bc = ArgE

  ld a,(Step)           ;a = (Step)
  inc a                 ;a += 1
  ld (Step),a           ;(Step) = a

  cp a,16
  jr c,loopRound0a      ;Si Step < 16: Jump loopRound0a
  cp a,20
  jr c,loopRound0b      ;Si Step < 20: Jump loopRound0b
  jr z,initRound1       ;Si Step == 20: Jump initRound1
  cp a,40
  jr c,loopRound1       ;Si Step < 40: Jump loopRound1
  jr z,initRound2       ;Si Step == 40: Jump initRound2
  cp a,60
  jr c,loopRound2       ;Si Step < 60: Jump loopRound2
  jr z,initRound3       ;Si Step == 60: Jump initRound3
  cp a,80
  jr c,loopRound3       ;Si Step < 80: Jump loopRound3
  jp loopRoundEnd       ;Sinon Jump loopRoundEnd

initRound0:
  ;Init RoundK ROUND0 0x5A827999
  ld hl,RoundK          ;hl = RoundK
  ld a,$5A              ;a = 0x5A
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$82              ;a = 0x82
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$79              ;a = 0x79
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$99              ;a = 0x99
  ldi (hl),a            ;(hl) = a, hl += 1
loopRound0a:
  call round0a
  jp loopRound
loopRound0b:
  call round0b
  jp loopRound

initRound1:
  ;Init RoundK ROUND1 0x6ED9EBA1
  ld hl,RoundK          ;hl = RoundK
  ld a,$6E              ;a = 0x6E
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$D9              ;a = 0xD9
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$EB              ;a = 0xEB
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$A1              ;a = 0xA1
  ldi (hl),a            ;(hl) = a, hl += 1
loopRound1:
  call round1
  jp loopRound

initRound2:
  ;Init RoundK ROUND2 0x8F1BBCDC
  ld hl,RoundK          ;hl = RoundK
  ld a,$8F              ;a = 0x8F
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$1B              ;a = 0x1B
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$BC              ;a = 0xBC
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$DC              ;a = 0xDC
  ldi (hl),a            ;(hl) = a, hl += 1
loopRound2:
  call round2
  jp loopRound

initRound3:
  ;Init RoundK ROUND3 0xCA62C1D6
  ld hl,RoundK          ;hl = RoundK
  ld a,$CA              ;a = 0xCA
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$62              ;a = 0x62
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$C1              ;a = 0xC1
  ldi (hl),a            ;(hl) = a, hl += 1
  ld a,$D6              ;a = 0xD6
  ldi (hl),a            ;(hl) = a, hl += 1
loopRound3:
  call round3
  jp loopRound

loopRoundEnd:
  add sp,10

  ld bc,InitA
  ld hl,StateA
  call addl             ;StateA += InitA

  ld bc,InitB
  ld hl,StateB
  call addl             ;StateB += InitB

  ld bc,InitC
  ld hl,StateC
  call addl             ;StateC += InitC

  ld bc,InitD
  ld hl,StateD
  call addl             ;StateD += InitD

  ld bc,InitE
  ld hl,StateE
  call addl             ;StateE += InitE

  ret

round0:
  ld hl,sp+6            ;hl = Stack+6 = ArgC
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call movl             ;StateT = ArgC

  ld hl,sp+8            ;hl = Stack+8 = ArgD
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call xorl             ;StateT ^= ArgD

  ld hl,sp+4            ;hl = Stack+4 = ArgB
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call andl             ;StateT &= ArgB

  ld hl,sp+8            ;hl = Stack+6 = ArgD
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call xorl             ;StateT ^= ArgD

  jp roundtail

round0a:
  ld a,(Step)           ;a = (Step)
  sla a                 ;a *= 2
  sla a                 ;a *= 2
  ld b,0                ;b = 0
  ld c,a                ;c = a
  ld hl,Block           ;hl = Block
  add hl,bc             ;hl += bc : Block[Step * 4]
  push hl               ;Stack hl
  ld hl,Schedule        ;hl = Schedule
  add hl,bc             ;hl += bc : Schedule[Step * 4]
  pop bc                ;UnStack bc = Block[Step * 4]
  push hl               ;Stack hl
  call movl             ;Schedule[Step * 4] = Block[Step * 4]

  pop bc                ;UnStack bc = Schedule[Step * 4]
  ld hl,sp+10           ;hl = Stack+10 = ArgE
  ld e,(hl)             ;e = (hl)
  inc hl                ;hl += 1
  ld d,(hl)             ;d = (hl)
  push de               ;Stack de
  pop hl                ;UnStack hl = de
  call addl             ;ArgE += Schedule[Step * 4]

  jp round0

round0b:
  call roundschedule
  jp round0

round1:
round3:
  call roundschedule

  ld hl,sp+4            ;hl = Stack+4 = ArgB
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call movl             ;StateT = ArgB

  ld hl,sp+6            ;hl = Stack+6 = ArgC
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call xorl             ;StateT ^= ArgC

  ld hl,sp+8            ;hl = Stack+8 = ArgD
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call xorl             ;StateT ^= ArgD

  jp roundtail

round2:
  call roundschedule

  ld hl,sp+6            ;hl = Stack+6 = ArgC
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call movl             ;StateT = ArgC

  ld bc,StateT          ;bc = StateT
  ld hl,RollT           ;hl = RollT
  call movl             ;RollT = StateT

  ld hl,sp+8            ;hl = Stack+8 = ArgD
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  push bc               ;Stack bc : ArgD
  ld hl,StateT          ;hl = StateT
  call orl              ;StateT |= ArgD

  pop bc                ;UnStack bc = ArgD
  ld hl,RollT           ;hl = RollT
  call andl             ;RollT &= ArgD

  ld hl,sp+4            ;hl = Stack+4 = ArgB
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,StateT          ;hl = StateT
  call andl             ;StateT &= ArgB
  
  ld bc,RollT           ;bc = RollT
  ld hl,StateT          ;hl = StateT
  call orl              ;StateT |= RollT

  jp roundtail

roundschedule:
  ld a,(Step)           ;a = (Step)
  sub 3                 ;a -= 3
  and 15                ;a &= 15
  sla a                 ;a *= 2
  sla a                 ;a *= 2
  ld b,0                ;b = 0
  ld c,a                ;c = a
  ld hl,Schedule        ;hl = Schedule
  add hl,bc             ;hl += bc : Schedule[(Step - 3) * 4]
  push hl               ;Stack hl
  pop bc                ;UnStack bc = hl
  ld hl,StateT          ;hl = StateT
  call movl             ;StateT = Schedule[(Step - 3) * 4]

  ld a,(Step)           ;a = (Step)
  sub 8                 ;a -= 8
  and 15                ;a &= 15
  sla a                 ;a *= 2
  sla a                 ;a *= 2
  ld b,0                ;b = 0
  ld c,a                ;c = a
  ld hl,Schedule        ;hl = Schedule
  add hl,bc             ;hl += bc : Schedule[(Step - 8) * 4]
  push hl               ;Stack hl
  pop bc                ;UnStack bc = hl
  ld hl,StateT          ;hl = StateT
  call xorl             ;StateT ^= Schedule[(Step - 8) * 4]

  ld a,(Step)           ;a = (Step)
  sub 14                ;a -= 14
  and 15                ;a &= 15
  sla a                 ;a *= 2
  sla a                 ;a *= 2
  ld b,0                ;b = 0
  ld c,a                ;c = a
  ld hl,Schedule        ;hl = Schedule
  add hl,bc             ;hl += bc : Schedule[(Step - 14) * 4]
  push hl               ;Stack hl
  pop bc                ;UnStack bc = hl
  ld hl,StateT          ;hl = StateT
  call xorl             ;StateT ^= Schedule[(Step - 14) * 4]

  ld a,(Step)           ;a = (Step)
  sub 16                ;a -= 16
  and 15                ;a &= 15
  sla a                 ;a *= 2
  sla a                 ;a *= 2
  ld b,0                ;b = 0
  ld c,a                ;c = a
  ld hl,Schedule        ;hl = Schedule
  add hl,bc             ;hl += bc : Schedule[(Step - 16) * 4]
  push hl               ;Stack hl
  pop bc                ;UnStack bc = hl
  ld hl,StateT          ;hl = StateT
  call xorl             ;StateT ^= Schedule[(Step - 16) * 4]

  ld bc,StateT          ;bc = StateT
  ld hl,RollT           ;hl = RollT
  call movl             ;RollT = StateT
  ld bc,RollT           ;bc = RollT
  ld hl,StateT          ;hl = StateT
  call roll1            ;StateT = roll1(RollT)

  ld hl,sp+12           ;hl = Stack+12 = ArgE
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  push bc               ;Stack bc
  pop hl                ;UnStack hl = bc
  ld bc,StateT          ;bc = StateT
  call addl             ;ArgE += StateT

  ld a,(Step)           ;a = (Step)
  and 15                ;a &= 15
  sla a                 ;a *= 2
  sla a                 ;a *= 2
  ld b,0                ;b = 0
  ld c,a                ;c = a
  ld hl,Schedule        ;hl = Schedule
  add hl,bc             ;hl += bc : Schedule[Step * 4]
  ld bc,StateT          ;bc = StateT
  call movl             ;Schedule[Step * 4] = StateT

  ret

roundtail:
  ld hl,sp+4            ;hl = Stack+4 = ArgB
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  push bc               ;Stack bc
  ld hl,RollT           ;hl = RollT
  call movl             ;RollT = ArgB
  pop hl                ;UnStack hl = ArgB
  ld bc,RollT           ;bc = RollT
  call roll30           ;ArgB = roll30(RollT)

  ld hl,sp+10           ;hl = Stack+10 = ArgE
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = ArgE
  push bc               ;Stack bc
  push bc               ;Stack bc
  pop hl                ;UnStack hl = bc
  ld bc,StateT          ;bc = StateT
  call addl             ;ArgE += StateT

  pop hl                ;UnStack hl = ArgE
  ld bc,RoundK          ;bc = RoundK
  call addl             ;ArgE += RoundK

  ld hl,sp+2            ;hl = Stack+2 = ArgA
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  ld hl,RollT           ;hl = RollT
  call roll5            ;RollT = roll5(ArgA)

  ld hl,sp+10           ;hl = Stack+10 = ArgE
  ld c,(hl)             ;c = (hl)
  inc hl                ;hl += 1
  ld b,(hl)             ;b = (hl)
  push bc               ;Stack bc
  pop hl                ;UnStack hl = bc
  ld bc,RollT           ;bc = RollT
  call addl             ;ArgE += RollT
  
  ret

roll1:                  ;hl=roll(bc,1)
  push hl               ;Stack hl
  pop de                ;UnStack de (de = hl)

  push bc
  pop hl

  ld b,3
looproll1:
  ldi a,(hl)
  sla a
  ld c,(hl)
  srl c
  srl c
  srl c
  srl c
  srl c
  srl c
  srl c
  or c
  ld (de),a
  inc de
  dec b
  jr nz,looproll1

  ldd a,(hl)
  dec hl
  dec hl
  sla a
  ld c,(hl)
  srl c
  srl c
  srl c
  srl c
  srl c
  srl c
  srl c
  or c
  ld (de),a

  ret

roll5:                  ;hl=roll(bc,5)
  push hl               ;Stack hl
  pop de                ;UnStack de = hl

  push bc               ;Stack bc
  pop hl                ;UnStack hl = bc

  ld b,3                ;b = 3
looproll5:
  ldi a,(hl)            ;a = (hl), hl += 1
  sla a                 ;a <<= 1
  sla a                 ;a <<= 1
  sla a                 ;a <<= 1
  sla a                 ;a <<= 1
  sla a                 ;a <<= 1
  ld c,(hl)             ;c = (hl)
  srl c                 ;c >>= 1
  srl c                 ;c >>= 1
  srl c                 ;c >>= 1
  or c                  ;a |= c
  ld (de),a             ;(de) = a
  inc de                ;de += 1
  dec b                 ;b -= 1
  jr nz,looproll5       ;Si b != 0 : Jump looproll5

  ldd a,(hl)            ;a = (hl), hl -= 1
  dec hl                ;hl -= 1
  dec hl                ;hl -= 1
  sla a                 ;a <<= 1
  sla a                 ;a <<= 1
  sla a                 ;a <<= 1
  sla a                 ;a <<= 1
  sla a                 ;a <<= 1
  ld c,(hl)             ;c = (hl)
  srl c                 ;c >>= 1
  srl c                 ;c >>= 1
  srl c                 ;c >>= 1
  or c                  ;a |= c
  ld (de),a             ;(de) = a

  ret

roll30:                 ;hl=roll(bc,30)
  push hl               ;Stack hl
  pop de                ;UnStack de (de = hl)

  push bc
  pop hl

  ld b,3
looproll30:
  ldi a,(hl)
  sla a
  sla a
  sla a
  sla a
  sla a
  sla a
  ld c,(hl)
  srl c
  srl c
  or c
  inc de
  ld (de),a
  dec b
  jr nz,looproll30

  dec de
  dec de
  dec de
  ldd a,(hl)
  dec hl
  dec hl
  sla a
  sla a
  sla a
  sla a
  sla a
  sla a
  ld c,(hl)
  srl c
  srl c
  or c
  ld (de),a

  ret


movl:                   ;hl=bc
  ld e,4                ;e = 4
loopmovl:
  ld a,(bc)             ;a = (bc)
  ldi (hl),a            ;(hl) = a, hl += 1
  inc bc                ;bc += 1
  dec e                 ;e -= 1
  jr nz,loopmovl        ;Si e != 0 : Jump loopmovl
  ret

addl:                   ;hl+=bc
  inc bc                ;bc += 1
  inc bc                ;bc += 1
  inc bc                ;bc += 1
  inc hl                ;hl += 1
  inc hl                ;hl += 1
  inc hl                ;hl += 1
  ld d,5                ;d = 5
  jp addlstart          ;Jump addlstart
addlnext:
  pop de                ;UnStack de
  pop hl                ;UnStack hl
  dec hl                ;hl -= 1
addlstart:
  dec d                 ;d -= 1
  jr z,addlend          ;Si d == 0 : Jump addlend
  ld a,(bc)             ;a = (bc)
  dec bc                ;bc -= 1
  push hl               ;Stack hl
  push de               ;Stack de
  add (hl)              ;a += (hl)
  ld (hl),a             ;(hl) = a
  jr nc,addlnext        ;Si a+(hl) < 256 : Jump addlnext (si pas de retenue)
addlnc:
  dec d                 ;d -= 1
  jr z,addlnext         ;Si d == 0 : Jump addlnext (si on est sur le dernier octet)
  dec hl                ;hl -= 1
  inc (hl)              ;(hl) += 1 (on applique la retenue)
  jr nz,addlnext        ;Si (hl) != 0 : Jump addlnext
  jp addlnc             ;Sinon Jump addlnc (on fait suivre la retenue sur l'octet suivant)
addlend:
  ret

xorl:                   ;hl^=bc
  ld e,4                ;e = 4
loopxorl:
  ld a,(bc)             ;a = (bc)
  xor (hl)              ;a &= (hl)
  ldi (hl),a            ;(hl) = a, hl += 1
  inc bc                ;bc += 1
  dec e                 ;e -= 1
  jr nz,loopxorl        ;Si e != 0 : Jump loopxorl
  ret

andl:                   ;hl&=bc
  ld e,4                ;e = 4
loopandl:
  ld a,(bc)             ;a = (bc)
  and (hl)              ;a &= (hl)
  ldi (hl),a            ;(hl) = a, hl += 1
  inc bc                ;bc += 1
  dec e                 ;e -= 1
  jr nz,loopandl        ;Si e != 0 : Jump loopandl
  ret

orl:                    ;hl&=bc
  ld e,4                ;e = 4
looporl:
  ld a,(bc)             ;a = (bc)
  or (hl)               ;a |= (hl)
  ldi (hl),a            ;(hl) = a, hl += 1
  inc bc                ;bc += 1
  dec e                 ;e -= 1
  jr nz,looporl         ;Si e != 0 : Jump looporl
  ret


.ORG $0800
tiles:
  .INCBIN "THIN_SS.bin"
strText:
  .DB "Text :"
strHash:
  .DB "SHA1 :"
strPressUpDown:
  .DB $18, "/", $19, " : Change char"
strPressA:
  .DB "A   : Select char"
strPressB:
  .DB "B   : Deselect char"
strPressStart:
  .DB "Press START to hash"