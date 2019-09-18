; Scroller
;
; Scrolls the screen joystick controlled in 4 directions.
; Displays a cursor in the middle of the screen and 
; plots a char at this pos, everytime the stick is moved.
;
; This prg shows how multi directonal fine scrolling can be done and
; how to locate or set a character inside the scrolling area.
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

pfwidth		equ 255	; x- size of pf in  bytes
pfheight	equ	255; y- size of pf in bytes	
windowx		equ	39	; x- size of screen
windowy		equ 20	; y- size of screen		

xr	.byte 0			; Save place for our registers
yr	.byte 0
a	.byte 0

lines	
	.byte 0			; Number of rows to be scrolled
hclocks
	.byte 0			; Finescroll, number of color clocks
vclocks
	.byte 0
	
xcount
	.byte 0			; Number of bytes screen has been scrolled in either direction.
ycount
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
	
	lda stick0
	and #8			; Right?
	bne nr
	jsr pl
	jmp movePFLeft
nr
	lda stick0
	and #4			; Left?
	bne nl
	jsr pl
	jmp movePFRight
nl
	lda stick0		; Down?
	and #2
	bne nd
	jsr pl
	jmp movePFUp
nd
	lda stick0
	and #1
	bne nu
	jsr pl
	jmp movePFDown
nu
	jmp out			; Leave VBI

	; Plotter
	; Plot something on center of screen.
pl
	ldx #20
	ldy #10
	lda #5
	jsr plot
	rts
	
	;
	; Playfield scroll routines
	;

	; Move PF up.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
movePFUp
	lda ycount
	cmp #pfheight
	bne ok1
	jmp out
ok1
	lda vclocks		; Already 7 scanlines moved upwards?
	cmp #7
	beq hardUp		; Yes! Move one entire line upwards!
	inc vclocks	
	lda vclocks
	sta $d405
	jmp out			; No hardscroll yet...
hardUp
	inc ycount
	lda #0			; Reset vcount
	sta $d405
	sta vclocks
	
	lda #20 		; We scroll 20 rows of our playfield
	sta lines
	lda #<(z0+1)	
	sta zp			
	lda #>(z0+1)
	sta zp+1
	ldy #0
s111				
	lda (zp),y		
	clc
	adc #<pfwidth
	sta (zp),y				
	iny
	lda (zp),y		
	adc #>pfwidth
	sta (zp),y
	iny				; Get adress of next row 
	iny				
	dec lines		; Do we have scrolled all rows of our playfield?		
	bne s111		; No!
	jmp out			; Leave VBI

	;Move PF down.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

movePFDown			; Same code as for the upward move except that now we
	lda ycount		; decrease the vcount value....
	bne ok2
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
	lda #20 		
	sta lines
	lda #<(z0+1)	
	sta zp			
	lda #>(z0+1)
	sta zp+1
	
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
	lda xcount
	cmp #pfwidth-windowx
	bne ok3
	jmp out
ok3
	lda hclocks		; Fine Scroll?
	beq hardleft	; No! Do hard scroll
	dec hclocks		; Do fine scroll	
	lda hclocks		; Colocks=3,2,1 counting down this values scrolls		
	sta $d404		; character to the left
	ldx xr			; Write regsiters back
	ldy yr	
	lda a				
	jmp $e45f		; Leave VBI
hardleft	
	inc xcount
	lda #3			; Reset fine scroll register				
	sta $d404		; after chracter was moved to it's leftmost position 
	sta hclocks		
	
	lda #20 		; We scroll 20 rows of our playfield
	sta lines
	lda #<(z0+1)	; Store adress of ram area where we have saved our adress for screen ram
	sta zp			; of line 0 of playfield (complicated? :-) Noooooooooo, just bad english :-(
	lda #>(z0+1)
	sta zp+1
	ldy #0
s1					
	lda (zp),y		; Get adress of row (Low)
	clc
	adc #1			; add => move row to the left => hardscroll
	sta (zp),y				
	iny
	lda (zp),y		; Get adress of row (High)
	adc #0			; Add contents of carry flag
	sta (zp),y
	iny				; Get adress of next row 
	iny				
	dec lines		; Do we have scrolled all rows of our playfield?		
	bne s1			; No!
	jmp out
	
	;Move PF right.;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

movePFRight			
	lda xcount		; Left border reached?
	bne ok4			; if nt scroll, else leave
	jmp out
ok4
	lda hclocks		; Same code as for the left move except that now we
	cmp #3			; increase the vcount value....
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
	
	lda #20 		
	sta lines
	lda #<(z0+1)	
	sta zp			
	lda #>(z0+1)
	sta zp+1
	ldy #0
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
	ldx xr
	ldy yr
	lda a
	jmp $e45f		
	
;
; DLI
;

dli
	pha			;  Save registers
	txa
	pha
	tya
	pha
	
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
	cmp #38		; 
	bcc dlout	; Do nothing!

	;
	; Set chset for playfield and playfield colors
	;
								
	lda #>chset12 	; Electron beam has crossed row 38 that means
	sta $d409		; we are in playfild area of our screen

	lda #200		; Color for bit combination: 10
	sta colpf1s									
	sta wsync
	
	lda #200			; Color for bit combination: 11
	sta colpf2s
	sta wsync
	
	lda #250		; Color for bit combination: 01	
	sta colpf0s									
	sta wsync

	lda #109		; Color for bit combination=color 5 bit combination 11 (reverse character)
	sta colpf3s	
	
	lda #24
	sta colbaks
	
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
	ldx #8
	ldy #73
d1
	lda cursor,x
	sta pmadr+512,y
	dey
	dex
	bne d1
	
	lda #110
	sta hposp0
	
	rts
	
;
; Plot 
;
; Writes any character you want, at any position into screen ram
;
; x-reg	= xpos
; y-reg	= ypos
; a		= Char
;
; Zeropage: zp7		

zeichen	.byte 0
xr4		.byte 0
yr4		.byte 0

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
	lda zeichen
	ldy xr4		; Xpos to y- reg.
	sta (zp),y
	
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
	.byte "Score                Hi-Score           "
message
	.byte "Mesage..                              "

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



		

	