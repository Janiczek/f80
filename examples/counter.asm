ROM_CLS equ 0x0DAF
AT      equ 0x16

      org  0x5ccb
            
            call ROM_CLS
            
            ; counter = 0
            ld hl,counter
            ld (hl),0
                         
            call renderStaticText
            
loop:
            call renderCounter

waitForJKDown:
            ld a, 0xbf       ; port for keys H J K L ENTER
            in a, (0xfe)     ; read from the keyboard port
            bit 3, a         ; bit 3: 0 = J pressed, 1 = J not pressed
            jp z, waitForJKDown
            bit 2, a         ; bit 2: 0 = K pressed, 1 = K not pressed
            jp z, waitForJKDown
            
waitForJKUp:
            ld a, 0xbf   ; port for keys H J K L ENTER
            in a, (0xfe)     ; read from the keyboard port
            bit 3, a       ; bit 3: 0 = J pressed, 1 = J not pressed
            jp z, onJ
            bit 2, a       ; bit 2: 0 = K pressed, 1 = K not pressed
            jp z, onK
            jp waitForJKUp ; Loop until J or K is pressed

onJ:        ; counter--
            ld a,(counter)
            dec a
            ld (counter),a
            jp loop
            
onK:        ; counter++
            ld a,(counter)
            inc a
            ld (counter),a
            jp loop

renderStaticText:
            ld hl,0x0505 ; X,Y = 5,5
            ld de,counterLabel
            call renderString
            
            ld hl,0x0507 ; X,Y = 5,7
            ld de,helpLabel
            call renderString
            ret

renderCounter:
            ; cleanup: draw 3 spaces
            ld hl,0x0e05 ; X,Y = 5,14
            ld de,emptySpaces
            call renderString
            
            ; DE = String.fromU8(counter)
            ld a,(counter)
            call u8ToString
            ex de,hl
            
            ld hl,0x0e05 ; X,Y = 5,14
            call renderString
            ret

; UTILITIES
; =========

; Render.string(s: String, x: U8, y: U8)
; -------------------------
; Expects: 
; H: X
; L: Y
; DE: string start (0-terminated)
; -------------------------
; Returns:
; A: clobbered
; DE: string 0-terminator
renderString:
            ld a, AT
            rst 0x10
            ld a, l ; Y
            rst 0x10
            ld a, h ; X
            rst 0x10                       
renderStringLoop:
            ld a, (de)
            cp 0
            ret z      ; return if A == 0
            rst 0x10   ; print char in A
            inc de     ; next char
            jr renderStringLoop


; String.fromU8(n: U8): String
; ----------------
; Expects:
; A: number 
; ----------------
; Returns:
; HL: address of the string, 0-terminated
; A: clobbered
; B: 10
; C: clobbered
; D: length
u8ToString:
            ld hl, u8ToStringBuffer+3 ; reverse storage
            ld (hl), 0
            dec hl
            ld d, 0  ; string length
            ld b, 10 ; divisor
u8ToStringLoop:
            call divU8U8    ; A/10, remainder in A
            add a, 0x30     ; convert digit 0-9 to ASCII char
            ld (hl), a
            dec hl          ; Move back for next digit
            inc d
            ld a, c         ; Load quotient
            or a            ; If A == 0, stop
            jr nz, u8ToStringLoop
            inc hl          ; Correct the address in HL
            ret



; U8.divMod(n: U8, d: U8): (quot: U8, rem: U8)
; --------------------
; Expects:
; A: dividend
; B: divisor
; --------------------
; Returns:
; A: remainder
; B: unchanged
; C: quotient
divU8U8:
            ld c, 0          ; quotient = 0
divU8U8Loop:
            sub b
            jr c, divU8U8End ; If A < 0, finish
            inc c            ; increment quotient
            jr divU8U8Loop
divU8U8End:
            add a, b         ; undo last subtraction - restore remainder
            ret

; DATA

counterLabel     db 'Counter: ', 0
helpLabel        db 'Press J (-) or K (+)', 0
emptySpaces      db '   ', 0

counter          db 0

u8ToStringBuffer ds 4 ; 3 digits + null

      org 0xff57
           db 0
