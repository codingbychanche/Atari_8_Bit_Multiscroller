; Scroller (even) more advanced
;
; Improved version of "Scroller_advanced.asm"
;
; Does the same as 'Scroller_advanced', but moves not the whole playfield
; when the cursor is moved and is is not near of either edge of the playfield.
;
; Plot routine now can also a read char from screen ram (set 'readflag' to a val > 0).
; When the cursor is moved, char overlapped will be shown in the status line on top of screen.
;
; This source is a good framework to build a playfield editor or a game engine.
;

; ANTIC

DLPTR	EQU 560	
VDLIST	EQU $200	 
NMIEN	EQU $D40E
WSYNC	EQU $D40A
VCOUNT	EQU $D40B
RTCLK	EQU $14
SDMCTL	EQU 559

; Colors

COLPF0	EQU 708  
COLPF1	EQU 709 
COLPF2	EQU 710
COLPF3	EQU 711
COLBAK	EQU 712

COLPF0S	EQU $D016 
COLPF1S	EQU $D017
COLPF2S	EQU $D018
COLPF3S	EQU $D019
COLBAKS	EQU $D01A

; PM Graphics

PMADR	EQU $B800 
PMCNTL	EQU $D01D 

HPOSP0	EQU $D000 
HPOSP1	EQU $D001
HPOSP2	EQU $D002
HPOSP3	EQU $D003

SIZEP0	EQU $D008 
SIZEP1	EQU $D009
SIZEP2	EQU $D00A
SIZEP3	EQU $D00B

COLPM0	EQU 704   
COLPM1	EQU 705
COLPM2	EQU 706
COLPM3	EQU 707

PMBASE	EQU $D407

GRACTL	EQU $D01D 

; PIA

stick0	equ	$278
strig0	equ $284

; Zeropage

ZP		equ $e0
zp2		equ $e2
zp3		equ $e4
zp4		equ $e6
zp5		equ	$e8
zp6		equ $ea
zp7		equ $ec
zp8		equ $ee

; Parameter

maxlin	equ $14		; # of rows of playfield
bytlin	equ $27		; byte/ row
screens	equ $06		; playfield consists of $06 screens

; Keyboard

CONSOL	EQU $d01f

;
; Start
;

	org $a800
	
begin
	lda #<dli   	; Display- List- Interrupt on
	sta vdlist
	lda #>dli
	sta vdlist+1
	lda #$C0
	sta NMIEN
	
	lda #<dlgame	; Show playfield		
	sta dlptr						
	lda #>dlgame
	sta dlptr+1
	
	lda #<message	; Show message below score, just for fun
	sta msg+1
	lda #>message
	sta msg+2
	
	jsr pminit
	jsr setcursor
	
	lda #0
	sta vclocks
	lda #0
	sta hclocks
	
	lda #0
	sta xcount		; No movemend yet in either directions.
	sta ycount
	
	ldy #<scroll	;Scroll Routine: Immediate VBI
	ldx #>scroll
	lda #6			;= Immediate VBI
	jsr $e45c		; Start imediate VBI
	
endless
	jmp endless
	
;
; VBI
; Check joystick and scroll playfield accordingly
;

pfwidth		equ 100		; x- size of pf in  bytes
pfheight	equ	40  	; y- size of pf in bytes

xcount
			.byte 0		; Number of bytes screen has been scrolled in either direction.
ycount
			.byte 0
 	
boundX_left	equ 10		; These constants define the bounding box within
boundX_rgt	equ 30		; the cursor moves but the playfield is not scrolled.
boundY_top	equ 5		; When bounds are reached, playfield scrolls.	
boundY_botn	equ 15

centerx		.byte 20	; Cursor position in center of playfield
centery		.byte 10	; Here the cursor is frozen and only moved when scrolling area reaches on of it's borders.

	
windowx		equ	40		; x- size of physical screen
windowy		equ 18		; y- size of physical screen	
localx		.byte 20	; Position of the cursor inside window 
localy		.byte 10
localxPM	.byte 110	; Position of pm- gfx acroding localx and localy coordinates in screen ram 
localyPM	.byte 73 

xr	.byte 0				; Save place for our registers
yr	.byte 0
a	.byte 0

lines	
	.byte 0				; Number of rows to be scrolled
hclocks
	.byte 0				; Finescroll, number of color clocks the contents of a scanline
vclocks					; has moved up, down, right or left.
	.byte 0

	;
	; Init
	; 
scroll
	stx xr			; Save registers
	sty yr			
	sta a	
	
	;
	; Check Stick
	;

	lda strig0		; Put char on screen?
	bne checkstick
	jsr pl
	
checkstick
	lda stick0
	and #8			; Right?
	bne nr
	jmp movePFLeft
nr
	lda stick0
	and #4			; Left?
	bne nl
	jmp movePFRight
nl
	lda stick0		; Down?
	and #2
	bne nd
	jmp movePFUp
nd
	lda stick0
	and #1
	bne nu
	jmp movePFDown
nu
	jmp out			; Leave VBI

	; Plotter
	; Plot something on center of screen.
pl
	ldx localx
	ldy localy
	lda #0			; Plot, not read!
	sta readflag
	lda #5
	jsr plot
	rts

	;
	; Playfield scroll routines
	;

	; Move PF up.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
movePFUp
	lda localy			; If cursor is not centered, just move cursor down
	cmp #boundY_botn
	bcs dd				; localy >bound_y scroll!
	jsr moveCursorDown	; Else, just move cursor.
	jmp out
	
	;cmp centery
	;beq dd				; Cursor is centered, scroll....
	;jsr moveCursorDown
	;jmp out
dd
	lda ycount			; Playfield scrolled all the way up (bottom reached)?.
	cmp #pfheight	
	bne ok1				; No: Scroll up!
	jsr move cursorDown ; Pf scrolled all the way up, just move cursor down.
	jmp out				; until it reaches the bottom border of the playfeld and Leave.
ok1
	lda vclocks			; Finescroll: Already 7 scanlines moved upwards?
	cmp #7
	beq hardUp			; Yes! Hardscroll!
	inc vclocks			; No! finescroll!
	lda vclocks
	sta $d405
	jmp out				
hardUp
	inc ycount			; Hardscroll!
	lda #0				; Reset vcount
	sta $d405
	sta vclocks
	jsr inithardscroll	; Get starting adress of first row of pf from antic programm
s111				
	lda (zp),y		
	clc
	adc #<pfwidth
	sta (zp),y				
	iny
	lda (zp),y		
	adc #>pfwidth
	sta (zp),y
	iny					; Get adress of next row 
	iny				
	dec lines			; Do we have scrolled all rows of our playfield?		
	bne s111			; No!
	jmp out				; Leave VBI

	;Move PF down.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

movePFDown			
	lda localy
	cmp #boundY_top
	bcc uu				; localy <bound_y scroll!
	jsr moveCursorUp
	jmp out
	
	;cmp centery		
	;beq uu				
	;jsr moveCursorUp	
	;jmp out
uu
	lda ycount		
	bne ok2
	jsr moveCursorUp
	jmp out
ok2
	lda vclocks			
	beq hardDown		
	dec vclocks
	lda vclocks
	sta $d405
	jmp out
hardDown	
	dec ycount
	jsr inithardscroll
	
	lda #7
	sta $d405
	sta vclocks
	ldy #0
s1111				
	lda (zp),y		
	sec
	sbc #<pfwidth
	sta (zp),y				
	iny
	lda (zp),y		
	sbc #>pfwidth
	sta (zp),y
	iny				
	iny				
	dec lines		
	bne s1111		
	jmp out

	;Move PF left.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

movePFleft
	lda localx
	cmp #boundX_rgt
	bcs rr					; localx >bound_x scroll!
	jsr moveCursorRight
	jmp out
;mrr
;	cmp centerx
;	beq rr
;	jsr moveCursorRight
rr
	lda xcount
	cmp #pfwidth-windowx
	bne ok3
	jsr moveCursorRight
	jmp out
ok3
	lda hclocks		
	beq hardleft	
	dec hclocks		
	lda hclocks			
	sta $d404		
	ldx xr			
	ldy yr	
	lda a				
	jmp $e45f		
hardleft	
	inc xcount
	lda #3						
	sta $d404		
	sta hclocks		
	jsr inithardscroll
s1					
	lda (zp),y		
	clc
	adc #1			
	sta (zp),y				
	iny
	lda (zp),y		
	adc #0			
	sta (zp),y
	iny				
	iny				
	dec lines			
	bne s1			
	jmp out
	
	;Move PF right.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

movePFRight		
	lda localx
	lda localx
	cmp #boundX_left
	bcc ll					; localx <bound_x scroll!
	jsr moveCursorLeft
	jmp out
mll
;	cmp centerx
;	beq ll
;	jsr moveCursorLeft
;	jmp out
ll
	lda xcount		
	bne ok4			
	jsr moveCursorLeft
	jmp out
ok4
	lda hclocks		
	cmp #3			
	beq hardright	
	inc hclocks			
	lda hclocks			
	sta $d404		
	jmp out
hardright	
	dec xcount
	lda #0					
	sta $d404		
	sta hclocks		
	jsr inithardscroll
s11					
	lda (zp),y		
	sec
	sbc #1			
	sta (zp),y				
	iny
	lda (zp),y		
	sbc #0			
	sta (zp),y
	iny				
	iny				
	dec lines			
	bne s11			

	;Leave VBI.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
out		
	lda #1				; Get char below cursor
	sta readflag		; and show it in status line.
	ldx localx
	ldy localy
	jsr plot
	sta scorelin
	
	ldx xr
	ldy yr
	lda a
	jmp $e45f			; leave VBI.
	
	; Subroutines for srolling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	;
	; Init hardscroll
	;

inithardscroll
	lda #20 			; 20 lines of pf are scrolled
	sta lines
	ldy #0				
	lda #<(z0+1)		; Get adress of first line of pf from
	sta zp				; Antic- programm and store it
	lda #>(z0+1)		; in zeropage
	sta zp+1
	rts					; now do the hardscroll.

	;
	; Move Cursor
	;

	; Cursor up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

moveCursorUp
	lda localy			; Upper border reached (y=0)?
	cmp #1				; Line 1 is the first vissible line of the screen...
	bne mup				; no, move it
	jmp ret				; Yes, dont't move
mup
	dec localy
	lda localyPM
	sec
	sbc #4
	sta localyPM
	jsr setcursor
	jmp ret
	
	; Cursor down ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
	
moveCursorDown
	lda localy			; Lower border reached?
	cmp #windowy
	bne mdn				
	jmp ret				
mdn
	inc localy
	lda localyPM
	clc
	adc #4
	sta localyPM
	jsr setcursor
	jmp ret

	
	; Cursor left ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

moveCursorLeft
	lda localx
	cmp #4			; 0 is not the vissible, left border of the screen. Bytes 0 to 3 are invissible!
	bne ml
	jmp ret
ml
	dec localx
	lda localxPM
	sec
	sbc #4
	sta localxPM
	jsr setcursor
	jmp ret
		
	; Cursor right ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

moveCursorRight
	lda localx
	cmp #windowx  
	bne mr
	jmp ret
mr
	ldx #4
mmm
	inc localx
	lda localxPM
	clc
	adc #4
	sta localxPM
	jsr setcursor
ret
	rts
	
;
; DLI
;

dli
	pha			;  Save registers
	txa
	pha
	tya
	pha
	
	sta wsync
	sta wsync
	lda vcount
	asl
	cmp #38
	bcs dli1	; Is electron beam at row > 38?

	;
	; Set Chset and colors for score display
	;
	
	lda #>chset	; No! Electron beam is at line # below 38
	sta $d409	; so we are still within our score display
	lda #0		; Change colors for capital charcters (basic-)mode 1 or 2 	
	sta wsync
	sta colpf0s					
	lda #150	; Char color, basic gr.0
	sta colpf1s
	lda #20
	sta colpf2s
	lda #20
	sta colpf3s
	lda #20		; Bright white for the background
	sta colbaks ; End of screen area for score display

dli1							
	lda vcount	; Is electron beam at row # bigger than 38?
	asl			; No? So we are still in area of score display
	cmp #37		; 
	bcc dlout	; Do nothing!

	;
	; Set chset for playfield and playfield colors
	;
								
	lda #>chset12 ; Electron beam has crossed row 38 that means
	sta $d409	; we are in playfild area of our screen

	lda #193	; Color for bit combination: 10
	sta colpf1s									
	sta wsync
	
	lda #250		; Color for bit combination: 11
	sta colpf2s
	sta wsync
	
	lda #250	; Color for bit combination: 01	
	sta colpf0s									
	sta wsync

	; Draw sky

	ldx #0		; Colors Below status
dd22
	lda coltap,x
	beq dlout
	sta colbaks
	sta wsync
	inx
	bne dd22			
dlout
	pla			; Get registers back
	tay
	pla
	tax
	pla
	
	rti
	
;
; Init PM- Graphics
;
	
pminit
	lda #pmadr/256	
	sta pmbase

	lda #255
	sta colpm0

	lda #204
	sta colpm1

	lda #208
	sta colpm2

	lda #210
	sta colpm3

	lda #0
	sta sizep0

	lda #46
	sta sdmctl

	lda #2
	sta gractl

	lda #%00111101
	sta 623
	
	; Now clear al gfx

	ldx #255
	lda #0
c1
	sta pmadr+512,x
	sta pmadr+640,x
	sta pmadr+768,x
	dex
	bne c1
	
	rts
	
;
; Draw cursor
;

cursor .byte 0,255,129,129,129,129,129,129,255

setcursor
	pha				; Save registers
	txa
	pha
	tya
	pha

	ldx #255
	lda #0
cl1
	sta pmadr+512,x
	dex
	bne cl1

	ldx #8
	ldy localyPM
d1
	lda cursor,x
	sta pmadr+512,y
	dey
	dex
	bne d1
	
	lda localxPM
	sta hposp0
	
	pla				; Get registers back
	tay
	pla
	tax
	pla
	
	rts
;
; Plot 
;
; Writes or reads any character you want, at any position into/ from screen ram.
; If read flag is 0 => plot, otherwise read and return in a- register....
;
; x-reg	= xpos
; y-reg	= ypos
; a		= Char
;
; Zeropage: zp7		

zeichen	.byte 0
xr4		.byte 0
yr4		.byte 0
readflag
		.byte 0

plot	
	stx xr4		; Save registers
	sty yr4
	sta zeichen
	
	
	lda z0+1	; Get lsb of screen adress for first line from antic prg.
	sta zp			
	lda z0+2	; Get msb of screen adress for first line from antic prg.
	sta zp+1
	
	cpy #0		; Line 0 => just calc x adress....
	beq plotit
p1	
	lda zp		; Get line- adress from antic program
	clc
	adc #<pfwidth; Next line
	sta zp
	lda zp+1
	adc #>pfwidth
	sta zp+1
	dey			
	bne p1
plotit
	lda readflag
	beq pl1		; if = 0 plot.
	ldy xr4		; Otherwise, get x pos,
	lda (zp),y	; read char from screen rm and return in 'a' register
	jmp go
pl1
	lda zeichen
	ldy xr4		; Xpos to y- reg.
	sta (zp),y
go
	ldx xr4		; Get registers back
	ldy yr4
	
	rts	
	
;	
; Antic program for our playfield
;

gr0		equ $02					; Gr. 0
gr1		equ $06					; Gr. 1
gr2		equ $07
gr12	equ $34					; Gr. 12, horiz. + vertical scrolling enabeled

dlgame	
	.byte $80,$80,$80,$80			 	
	.byte 112+128				; Start of Antic programm for our playfield			
	.byte $40+gr0,a(scorelin)	; Gr.1 display. Tha's where we can see our score and other important messages.....
	.byte 112		
msg
	.byte $40+gr0,a(message)	; Message line, tell the player what's going on
	.byte 112

	.byte $70+128				; 8 empty lines, start display- list interrupt
	
z0	.byte $40+gr12,a(screen)			
z1	.byte $40+gr12,a(screen+1*pfwidth) 	
z2	.byte $40+gr12,a(screen+2*pfwidth) 
z3	.byte $40+gr12,a(screen+3*pfwidth) 
z4	.byte $40+gr12,a(screen+4*pfwidth)
z5	.byte $40+gr12,a(screen+5*pfwidth)
z6	.byte $40+gr12,a(screen+6*pfwidth)
z7	.byte $40+gr12,a(screen+7*pfwidth)
z8	.byte $40+gr12,a(screen+8*pfwidth)
z9	.byte $40+gr12,a(screen+9*pfwidth)
z10	.byte $40+gr12,a(screen+10*pfwidth)
z11	.byte $40+gr12,a(screen+11*pfwidth)
z12	.byte $40+gr12,a(screen+12*pfwidth)
z13	.byte $40+gr12,a(screen+13*pfwidth)
z14	.byte $40+gr12,a(screen+14*pfwidth)
z15	.byte $40+gr12,a(screen+15*pfwidth)
z16	.byte $40+gr12,a(screen+16*pfwidth) 
z17	.byte $40+gr12,a(screen+17*pfwidth)
z18	.byte $40+gr12,a(screen+18*pfwidth)
z19	.byte $40+$14,a(screen+19*pfwidth)	; Row 20
	.byte $41,a(dlgame)				 	; End of display- list, start all over again....
	
scorelin								; Contents of screen ram for status display
	.byte "  -> Char overlaping                    "
message
	.byte "			                               "


;
; Color Tap for DLI
;

coltap

		.byte 1
:130	.byte 120		; Blue
		.byte 1			; Black
:30		.byte 20		; Brown
		.byte 0			; End 

;
; Charset data
;
; For: Text (Atari Basic-) text modes 0,1,2 
;

	 org $4400
chset
	;------------------
	; Dump of:	ECKIG.FNT
	; Taken from: Design Master Disc....

.byte	$00,$00,$00,$00,$00,$00,$00,$00
.byte	$00,$18,$18,$18,$18,$00,$18,$00
.byte	$00,$66,$66,$66,$00,$00,$00,$00
.byte	$00,$66,$ff,$66,$66,$ff,$66,$00
.byte	$18,$3e,$60,$3c,$06,$7c,$18,$00
.byte	$00,$66,$6c,$18,$30,$66,$46,$00
.byte	$1c,$36,$1c,$38,$6f,$66,$3b,$00
.byte	$00,$18,$18,$18,$00,$00,$00,$00
.byte	$00,$0e,$1c,$18,$18,$1c,$0e,$00
.byte	$00,$70,$38,$18,$18,$38,$70,$00
.byte	$00,$66,$3c,$ff,$3c,$66,$00,$00
.byte	$00,$18,$18,$7e,$18,$18,$00,$00
.byte	$00,$00,$00,$00,$00,$18,$18,$30
.byte	$00,$00,$00,$7e,$00,$00,$00,$00
.byte	$00,$00,$00,$00,$00,$18,$18,$00
.byte	$00,$06,$0c,$18,$30,$60,$40,$00
.byte	$00,$7e,$66,$6e,$76,$66,$7e,$00
.byte	$00,$38,$38,$18,$18,$18,$7e,$00
.byte	$00,$7e,$66,$06,$7e,$60,$7e,$00
.byte	$00,$7e,$06,$1e,$06,$66,$7e,$00
.byte	$00,$60,$60,$66,$66,$7e,$0e,$00
.byte	$00,$7e,$60,$7e,$06,$66,$7e,$00
.byte	$00,$7c,$60,$7e,$66,$66,$7e,$00
.byte	$00,$7e,$06,$06,$06,$06,$06,$00
.byte	$00,$7e,$66,$7e,$66,$66,$7e,$00
.byte	$00,$7e,$66,$7e,$06,$06,$06,$00
.byte	$00,$00,$18,$18,$00,$18,$18,$00
.byte	$00,$00,$18,$18,$00,$18,$18,$30
.byte	$06,$0c,$18,$30,$18,$0c,$06,$00
.byte	$00,$00,$7e,$00,$00,$7e,$00,$00
.byte	$60,$30,$18,$0c,$18,$30,$60,$00
.byte	$00,$7e,$66,$0c,$18,$00,$18,$00
.byte	$00,$3e,$66,$6c,$66,$66,$6e,$60
.byte	$00,$7e,$66,$66,$7e,$66,$66,$00
.byte	$00,$7e,$66,$7c,$66,$66,$7e,$00
.byte	$00,$7e,$66,$60,$60,$66,$7e,$00
.byte	$00,$7c,$66,$66,$66,$66,$7c,$00
.byte	$00,$7e,$60,$7c,$60,$60,$7e,$00
.byte	$00,$7e,$60,$7c,$60,$60,$60,$00
.byte	$00,$7e,$60,$60,$6e,$66,$7e,$00
.byte	$00,$66,$66,$7e,$66,$66,$66,$00
.byte	$00,$7e,$18,$18,$18,$18,$7e,$00
.byte	$00,$06,$06,$06,$06,$66,$7e,$00
.byte	$00,$66,$6c,$78,$78,$6c,$66,$00
.byte	$00,$60,$60,$60,$60,$60,$7e,$00
.byte	$00,$63,$77,$7f,$6b,$63,$63,$00
.byte	$00,$66,$76,$7e,$6e,$6e,$66,$00
.byte	$00,$7e,$66,$66,$66,$66,$7e,$00
.byte	$00,$7e,$66,$66,$7e,$60,$60,$00
.byte	$00,$7e,$66,$66,$66,$66,$7e,$03
.byte	$00,$7e,$66,$66,$7e,$6c,$6e,$00
.byte	$00,$7c,$60,$7e,$06,$06,$7e,$00
.byte	$00,$7e,$18,$18,$18,$18,$18,$00
.byte	$00,$66,$66,$66,$66,$66,$7e,$00
.byte	$00,$66,$66,$66,$66,$3c,$18,$00
.byte	$00,$63,$63,$6b,$7f,$77,$63,$00
.byte	$00,$66,$66,$3c,$3c,$66,$66,$00
.byte	$00,$66,$66,$7e,$18,$18,$18,$00
.byte	$00,$7e,$0c,$18,$30,$60,$7e,$00
.byte	$66,$3c,$66,$66,$7e,$66,$66,$00
.byte	$66,$3c,$66,$66,$66,$66,$7e,$00
.byte	$66,$00,$66,$66,$66,$66,$7e,$00
.byte	$00,$08,$1c,$36,$63,$00,$00,$00
.byte	$00,$00,$00,$00,$00,$00,$ff,$00
.byte	$00,$36,$7f,$7f,$3e,$1c,$08,$00
.byte	$18,$18,$18,$1f,$1f,$18,$18,$18
.byte	$03,$03,$03,$03,$03,$03,$03,$03
.byte	$18,$18,$18,$f8,$f8,$00,$00,$00
.byte	$18,$18,$18,$f8,$f8,$18,$18,$18
.byte	$00,$00,$00,$f8,$f8,$18,$18,$18
.byte	$03,$07,$0e,$1c,$38,$70,$e0,$c0
.byte	$c0,$e0,$70,$38,$1c,$0e,$07,$03
.byte	$01,$03,$07,$0f,$1f,$3f,$7f,$ff
.byte	$00,$00,$00,$00,$0f,$0f,$0f,$0f
.byte	$80,$c0,$e0,$f0,$f8,$fc,$fe,$ff
.byte	$0f,$0f,$0f,$0f,$00,$00,$00,$00
.byte	$f0,$f0,$f0,$f0,$00,$00,$00,$00
.byte	$ff,$ff,$00,$00,$00,$00,$00,$00
.byte	$00,$00,$00,$00,$00,$00,$ff,$ff
.byte	$00,$00,$00,$00,$f0,$f0,$f0,$f0
.byte	$00,$1c,$1c,$77,$77,$08,$1c,$00
.byte	$00,$00,$00,$1f,$1f,$18,$18,$18
.byte	$00,$00,$00,$ff,$ff,$00,$00,$00
.byte	$18,$18,$18,$ff,$ff,$18,$18,$18
.byte	$00,$00,$3c,$7e,$7e,$7e,$3c,$00
.byte	$00,$00,$00,$00,$ff,$ff,$ff,$ff
.byte	$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
.byte	$00,$00,$00,$ff,$ff,$18,$18,$18
.byte	$18,$18,$18,$ff,$ff,$00,$00,$00
.byte	$f0,$f0,$f0,$f0,$f0,$f0,$f0,$f0
.byte	$18,$18,$18,$1f,$1f,$00,$00,$00
.byte	$78,$60,$78,$60,$7e,$18,$1e,$00
.byte	$00,$18,$3c,$7e,$18,$18,$18,$00
.byte	$00,$18,$18,$18,$7e,$3c,$18,$00
.byte	$00,$18,$30,$7e,$30,$18,$00,$00
.byte	$00,$18,$0c,$7e,$0c,$18,$00,$00
.byte	$66,$00,$7e,$66,$66,$66,$7e,$00
.byte	$00,$00,$3e,$06,$7e,$66,$7f,$00
.byte	$00,$60,$60,$7e,$66,$66,$7e,$00
.byte	$00,$00,$7c,$60,$60,$60,$7c,$00
.byte	$00,$06,$06,$7e,$66,$66,$7e,$00
.byte	$00,$00,$7e,$66,$7e,$60,$7e,$00
.byte	$00,$1e,$18,$3e,$18,$18,$18,$00
.byte	$00,$00,$7e,$66,$66,$7e,$06,$7e
.byte	$00,$60,$60,$7e,$66,$66,$66,$00
.byte	$00,$18,$00,$38,$18,$18,$3c,$00
.byte	$00,$06,$00,$06,$06,$06,$06,$3e
.byte	$00,$60,$60,$6e,$78,$66,$66,$00
.byte	$00,$38,$18,$18,$18,$18,$3c,$00
.byte	$00,$00,$67,$7f,$7f,$6b,$63,$00
.byte	$00,$00,$7e,$66,$66,$66,$66,$00
.byte	$00,$00,$7e,$66,$66,$66,$7e,$00
.byte	$00,$00,$7e,$66,$66,$7e,$60,$60
.byte	$00,$00,$7e,$66,$66,$7e,$06,$06
.byte	$00,$00,$7e,$66,$60,$60,$60,$00
.byte	$00,$00,$7e,$60,$7e,$06,$7e,$00
.byte	$00,$18,$7e,$18,$18,$18,$1e,$00
.byte	$00,$00,$66,$66,$66,$66,$7e,$00
.byte	$00,$00,$66,$66,$66,$7e,$18,$00
.byte	$00,$00,$63,$63,$6b,$7f,$77,$00
.byte	$00,$00,$66,$7e,$18,$7e,$66,$00
.byte	$00,$00,$66,$66,$66,$7e,$06,$7e
.byte	$00,$00,$7e,$0c,$18,$30,$7e,$00
.byte	$66,$00,$3e,$06,$7e,$66,$7f,$00
.byte	$18,$18,$18,$18,$18,$18,$18,$18
.byte	$66,$00,$00,$66,$66,$66,$7e,$00
.byte	$08,$18,$38,$78,$38,$18,$08,$00
.byte	$10,$18,$1c,$1e,$1c,$18,$10,$00
.byte	$01
	
;
; Charset data
;
; For: Text (Atari Basic-) text modes 12 and 13 
;
; They are not ordered :(
;

		org $4800														
		
chset12
:8		.byte 0																; Empty						//0
 		.byte 127,127,127,31,31,7,7,1										; Cloud tile 1, bottom left //1
		.byte 255,125,20,0,0,0,0,0											; Cloud tile bottom			//2
		.byte 0	,	0	,	20	,	125	,	255	,	255	,	255	,	255		; Cloud tile top			//3
		.byte 253	,	253	,	244	,	244	,	208	,	208	,	64	,	64  ; Cloud tile, bottom right	//4
:8		.byte 255															; Cloud tile, solid block	//5

:8		.byte 85															; Solid black block			//6		
		.byte 85	,	85	,	85	,	85	,	85	,	101	,	166	,	170 ; Solid black block, bottom //7

:8		.byte 187													        ; Light green block 		//8
		.byte 186	,	186	,	186	,	186	,	186	,	186	,	186	,	186	; Not so light green block  //9
:8		.byte 174															; Solid green block			//10
:8		.byte 171															; Solid green block			//11
:8		.byte 64															; Flag pole 				//12
	    .byte 76	,	127	,	127	,	127	,	127	,	127	,	127	,	76  ; Flag part 	1			//13
		.byte 207	,	255	,	252	,	252	,	252	,	252	,	255	,	207 ; Flag Part 	2			//14
		.byte 85	,	85	,	65	,	65	,	65	,	85	,	85	,	85  ; Building part with window //15

;
; Screen- ram of playfield
;
			org $5000
screen		.byte 0



		

	