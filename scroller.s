       SECTION Tut,CODE_C

RELEASE	SET	1
w      =352
h      =256
bplsize=w*h/8
ScrBpl=w/8
logobgcolor = $fff
bgcolor=$44f

plotY = 70
countLoops = $1337
QR_SCREEN_WIDTH = 320
QR_SCREEN_BPL = 1
QR_SCREEN_BROW = QR_SCREEN_WIDTH/8
QR_SCREEN_LINE = QR_SCREEN_BROW*QR_SCREEN_BPL

logoScreenW = 320
logoScreenBpl = logoScreenW/8
logow =144
logoh =151
logobitplanes = 5
logomargin=(320-logow)/2
logobpl =logow/8
logobwid = logobpl*logobitplanes

font_x = 8
font_y = 8
font_start = $20
font_stop = $ff
font_modulo = $98
fontbpls = 3
offset_fontdata = $0
offset_fontloc = $4c0
width = offset_fontloc - offset_fontdata
FontBpl = (width)/8    ; font width / 8
offset_fontspace = $844
offset_fontkern = $a06

SCREEN_LINE		= ScrBpl*fontbpls
SCREEN_MEMSIZE	= SCREEN_LINE*h


LogoScreen_Line     = logoScreenBpl*logobitplanes
LogoScreen_MemSize  = LogoScreen_Line*logoh+1000

       incdir "include"
       include "hw.i"
       
       RSRESET
oldStack:	rs.l	1
oldView:	rs.l	1
oldIntena:	rs.w	1
oldDma:		rs.w	1
gfxBase:	rs.l	1
dt_SIZEOF:	rs.b	0

start:
       movem.l d1-a6,-(sp)

       bsr DoVariables
       bsr OpenGraphicsLibrary
       beq.w	Exit
       bsr.w DisableOs
       bsr.w ProgramLoop
**********************************
Quit:  bsr EnableOs
Exit:	bsr.w	CloseGraphicsLibrary
       ;beq.w	Exit
       move.l dt+oldStack(pc),a7

       movem.l (sp)+,d1-a6
       moveq #0,d0
       rts                         ;end of program return to AmigaOS

       
************ ROUTINES  ****************
ProgramLoop:

       bsr init
       bsr StartMusic
       
       move.l #copper,cop1lc(a5)
       move #$87e0,dmacon(a5)         ;enable priority,all,bitpln,copper,blitter DMA
       move.w	#$e000,intena(a5)

       ;move #$4c,d7                ;start y position
       ;moveq #1,d6                  ;y add

       bsr main

       bsr WaitRaster
       bsr WaitRaster
       bsr WaitRaster
       bsr WaitRaster
       bsr WaitRaster
       bsr WaitRaster
       bsr WaitRaster

       bsr ShowQrCode

       bsr LSP_MusicDriver_CIA_Stop

       rts

;-----------------------------------------------------------------------------
;in
;a5	custom
;a6	dt
;
EnableOs:
		move.l	a6,a4

		move.l	gfxBase(a4),a6
		jsr	-228(a6)	;gfx WaitBlit()

		bsr.b	StopDmaAndIntsAtVBlank

	;restore hardware regs
		move.w	oldIntena(a4),intena(a5)
		move.w	oldDma(a4),dmacon(a5)

		jsr	-462(a6)	;gfx DisownBlitter()

	;load old view
		move.l	oldView(a4),a1
		bsr.b	LoadView

		move.l	$26(a6),cop1lc(a5)

		move.l	a4,a6
		rts

StopDmaAndIntsAtVBlank:
       bsr WaitRaster
       move.w	#$7fff,d0
       move.w	d0,dmacon(a5)	;dma off
       move.w	d0,intena(a5)	;disable ints
       move.w	d0,intreq(a5)	;clear pending ints
       rts

;-----------------------------------------------------------------------------
;in
;	a6 - gfx base
;	a1 - view
LoadView:
		jsr	-222(a6)	;gfx LoadView(view)
		jsr	-270(a6)	;gfx WaitTOF()
		jmp	-270(a6)	;gfx WaitTOF()

DisableOs:
	;save old view
		move.l	gfxBase(a6),a5
		move.l	$22(a5),oldView(a6)
		exg	a5,a6

	;set no view 
		sub.l	a1,a1
		bsr.b	LoadView

	;takeover the blitter
		jsr	-456(a6)	;gfx OwnBlitter
		jsr	-228(a6)	;gfx WaitBlit

		move.l	a5,a6

	;store hardware registers
		lea	CUSTOM,a5
		move.w	#$c000,d1

		move.w	intenar(a5),d0
		or.w	d1,d0
		move.w	d0,oldIntena(a6)

		add.w	d1,d1
		move.w	dmaconr(a5),d0
		or.w	d1,d0
		move.w	d0,oldDma(a6)

		bra.b	StopDmaAndIntsAtVBlank

CloseGraphicsLibrary:
		move.l	gfxBase(a6),a1	;library base
		move.l	4.w,a6		;exec base
	IFND KICKSTART2
		move.l	a1,d0		;trick to check if lib base is zero
		beq.b	.exit
	ENDC
		jsr	-414(a6)	;exec CloseLibrary
.exit		rts

OpenGraphicsLibrary:
		move.l	4.w,a6			;exec base
		lea	gfxname,a1		;library name
		jsr	-408(a6)		;exec OldOpenLibrary()
		lea	dt(pc),a6
		move.l	d0,gfxBase(a6)		;store result of opening
		rts

DoVariables:
		lea	dt(pc),a6

	;clear dt
		move.l	a6,a0
		moveq	#0,d0
		move.w	#dt_SIZEOF/4-1,d1
.clear		move.l	d0,(a0)+
		dbf	d1,.clear

       lea	Screen,a0

       IFND KICKSTART2
	;clear BSS area 
		move.l	a0,a1
		move.l	#SCREEN_MEMSIZE/4,d1
.clearBSS	move.l	d0,(a1)+
		subq.l	#1,d1
		bne.b	.clearBSS
	ENDC

       lea	LogoScreen,a0

       IFND KICKSTART2
	;clear BSS area 
		move.l	a0,a1
		move.l	#LogoScreen_MemSize/4,d1
.clearBSS2	move.l	d0,(a1)+
		subq.l	#1,d1
		bne.b	.clearBSS2
	ENDC

	;store old stack pointer
		lea	4(a7),a0
		move.l	a0,oldStack(a6)

		rts

ShowQrCode:
       movem.l d0-a6,-(sp)

       move.l #copper2,cop1lc(a5)
       bsr PlotQRCode
       IFEQ RELEASE
       lea qrCode,a0
       ENDC

mainloop2:
       btst #6,$bfe001
       bne mainloop2

       movem.l (sp)+,d0-a6
       rts

main:
       movem.l d0-a6,-(sp)

       moveq	#0,d1
       IFEQ RELEASE
       move.l	#countLoops,d2
       ;moveq	#1,d1		; 0 = forward, 1 = backward
.loop
       bsr RotateQrCodePlanHorizontal
       sub.l	#1,d2
       tst.l	d2
       bne .loop
	ENDC

mainloop:
       move.w #$02a,d0
       bsr.w WaitRaster2
       btst #2,$dff016
       bne.b .normb
       ;move.w #$02b,d0
       bsr.w WaitRaster2
       bra.b mainloop

;--------frame loop start---------    
.normb:
       bsr BounceScroller

       cmp.b #$ef,Spr1
       bne .dontReset
       move.b #0,Spr1
       move.b #$10,Spr1+2

.dontReset:
       add.b #1,Spr1                 ;move sprite to the right
       add.b #1,Spr1+2   
                         
       cmp.b #$ef,Spr2
       bne .dontReset2
       move.b #0,Spr2
       move.b #$10,Spr2+2

.dontReset2:
       add.b #1,Spr2                 ;move sprite to the right
       add.b #1,Spr2+2                   ;move sprite to the right

.endSprite:
       lea Sine,a0                        ;move the copper bar depending on the
       move.w SineCtr,d6                  ;pre calced sine wave
       move.w #$2c+75,d7
       add.w (a0,d6.w),d7

       addq.w #2,d6
       cmp.w #SineEnd-Sine,d6
       blt.s .nowrap3
       moveq #0,d6
.nowrap3:
       move.w d6,SineCtr


ok2:

       lea waitras1,a0
       move d7,d0
       moveq #6-1,d1
.l:    
       move.b d0,(a0)
       add.w #1,d0
       add.w #4+(4*1),a0
       dbf d1,.l

       ;move.l #waitras1,a0

       bsr scrollit                ; scroll the plotted text to the left

       move.w ScrollCtr,d0         ; plot new char every 32 pixels
       addq.w #4,d0                ; cause of blitter scrolling
       cmp.w #32,d0
       blo.s .nowrap

       move.l ScrollPtr,a0
       cmp.l #ScrollTextWrap,a0
       blo.s .noplot
       lea ScrollText,a0
.noplot:

       bsr PlotChar2               ; preserves a0

       addq.w #1,a0
       move.l a0,ScrollPtr

       sub.w #32,d0
.nowrap:
       move.w d0,ScrollCtr

       IF 1=0
       move.w ColorCycleCtr,d0     ; scroll the color 7
       addq.w #1,d0
       cmp.w #5,d0
       blo.s .nowrap2
       
       lea Font2PalP,a0
       add.l #8*4-2,a0
       add.w #1,(a0)

       sub.w #5,d0
.nowrap2:
       move.w d0,ColorCycleCtr
       ENDC

       
       IFNE RELEASE
       bsr RotateQrCodePlanHorizontal
	ENDC
       
;--------frame loop end-----------

       btst #6,$bfe001
       bne mainloop

       movem.l (sp)+,d0-a6
       rts
       
************************************
init:
       movem.l d0-a6,-(sp)

       IF 1=0
       moveq #0,d1
       lea Screen,a1
       move.w #bplsize/2-1,d0
.l:    move.w d1,(a1)+
       addq.w #1,d1
       dbf d0,.l
       ENDC

       lea Spr1,a1
       lea SprP,a2
       moveq #2-1,d1

.nextSprite
       move.l a1,d0
       
       swap d0
       move d0,2(a2)
       swap d0
       move d0,6(a2)
       
       add #Spr1Size,a1
       addq #8,a2
       dbf d1,.nextSprite

       if 1=1
       lea NullSpr,a1
       move.l a1,d0
       lea SprP+16,a1
       move #6-1,d1
.l2:
       swap d0
       move d0,2(a1)
       swap d0
       
       move d0,6(a1)
       addq #8,a1

       dbf d1,.l2
       endc

       lea LogoScreen,a0
       lea CopBplP,a1
       move #logobitplanes-1,d0
.l3:
       move.l a0,d1
       swap d1
       move d1,2(a1)
       swap d1
       move d1,6(a1)

       addq #8,a1
       lea logoScreenBpl(a0),a0
       dbf d0,.l3

       lea CopBplP,a1

       

       lea Font2E-7*2,a0
       lea Font2PalP+2,a1
       moveq #7-1,d0
.col:  move.w (a0)+,(a1)+
       addq.w #2,a1
       dbf d0,.col

       lea Screen,a0
       lea copper2bplP,a1
       move.l a0,d1
       swap d1
       move d1,2(a1)
       swap d1
       move d1,6(a1)

       ; copy logo to screen
       lea logo,a0
       lea LogoScreen,a1
       add.l #10,a1

       
       move #logoh*logobitplanes-1,d0
.copy
       move #logobpl-1,d1
.copyByte:
       move.b (a0)+,(a1)+
       dbf d1,.copyByte
       sub.l #logobpl,a1
       lea logoScreenBpl(a1),a1
       dbf d0,.copy

       ;bsr PlotChar

       movem.l (sp)+,d0-a6

       

       rts

font2_char_w = 32
font2_char_h = 25

scrollY2 = plotY
scrollX2 = w-font2_char_w
font2_w = 320
font2_line = font2_w/8
font2_bpls = 3
font2_bpl = font2_w/8
font2_selected_x = 0
font2_selected_y = 3
font2_row = font2_w*font2_bpls*font2_char_h/8
font2_col = 4
PlotChar2:           ;a0=scrollptr
       movem.l d0-a6,-(sp)
       ;lea CUSTOM,a6
       bsr BlitWait

       moveq #0,d0
       move.b (a0)+,d0                    ;ASCII value
       move.b d0,LastChar

       sub.w #32,d0
       lea FontTbl,a0
       move.b (a0,d0.w),d0
       divu #10,d0                        ;row
       move.l d0,d1
       swap d1                            ;remainder (column)

       mulu #font2_row,d0
       mulu #font2_col,d1

       add.l d1,d0                        ;offset into font bitmap
       add.l #Font2,d0


       
       move.l #$09f00000,bltcon0(a5)
       move.l #$ffffffff,bltafwm(a5)
       ;move.l #Font2+(font2_selected_y*font2_char_h*font2_line*font2_bpls)+(font2_selected_x*4),bltapt(a6)
       move.l d0,bltapt(a5)
       move.l #Screen+ScrBpl*fontbpls*scrollY2+scrollX2/8,bltdpt(a5)
       move.w #font2_bpl-font2_col,bltamod(a5)
       move.w #ScrBpl-font2_col,bltdmod(a5)


       move.w #font2_char_h*font2_bpls*64+2,bltsize(a5)
       movem.l (sp)+,d0-a6
       rts

FontTbl:
       dc.b 47,26,47,47,46,47,47
       dc.b 45,41,42,47,47,40,43,44,47
       dc.b 30,31,32,33,34,35,36,37,38,39
       dc.b 28,29,47,47,47
       dc.b 27,47
       dc.b 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26
       EVEN

       IF 1=0
scrollY = 100
PlotChar:
       movem.l d0-a6,-(sp)
       ;lea CUSTOM,a6

       bsr BlitWait

       move.l #$09f00000,bltcon0(a5)
       move.l #$ffffffff,bltafwm(a5)
       move.l #Font,bltapt(a5)
       move.l #LogoScreen+logoScreenBpl*logobitplanes*scrollY,bltdpt(a5)
       move.w #(width-16)/8,bltamod(a5)
       move.w #((logoScreenW-16)/8)+(logoScreenW/8*4),bltdmod(a5)


       move.w #8*1*64+1,bltsize(a5)
       movem.l (sp)+,d0-a6
       rts
       endc

scrollit:
bltx   =0
blty   =plotY
bltoffs=blty*(ScrBpl*fontbpls)+bltx/8

blth   =25
bltw   =w/16
bltskip=(w-w)/8
brcorner=blth*ScrBpl*font2_bpls-2
       movem.l d0-a6,-(sp)
       ;lea CUSTOM,a6

       bsr BlitWait

       move.l #$49f00002,bltcon0(a5)
       move.l #$ffffffff,bltafwm(a5)
       move.l #Screen+bltoffs+brcorner,bltapt(a5)
       move.l #Screen+bltoffs+brcorner,bltdpt(a5)
       move.w #bltskip,bltamod(a5)
       move.w #bltskip,bltdmod(a5)


       move.w #blth*fontbpls*64+bltw,bltsize(a5)
       movem.l (sp)+,d0-a6
       rts

CopyB:        ;d0,a0,a1=count,source,destination
.l:    move.b (a0)+,(a1)+
       subq.l #1,d0
       bne.s .l
       rts

BlitWait:
       tst dmaconr(a5)                        ;for compatibility
.waitblit:
       btst #14-8,dmaconr(a5)
       bne.s .waitblit
       rts

WaitRaster:
       lea	vposr(a5),a0
.1	moveq	#1,d0
       and.w	(a0),d0
       bne.b	.1
.2	moveq	#1,d0
       and.w	(a0),d0
       beq.b	.2
       rts

WaitRaster2:
       move.l #$1ff00,d2
       lsl.l #8,d0
       and.l d2,d0
       lea vposr(a5),a0
.wr:   move.l (a0),d1
       and.l d2,d1
       cmp.l d1,d0
       bne.s .wr
       rts

BounceScroller:
       movem.l d0-a6,-(sp)

       lea Screen,a0
       move.w BounceY,d0
       move.w BounceYaccel,d1
       add.w d1,BounceYspeed
       add.w BounceYspeed,d0
       bpl.s .nobounce
       lea BounceYspeed,a3
       move.w #25,BounceYspeed
       clr.w d0
.nobounce:
       move.w d0,BounceY

       lsr #3,d0
       mulu #font2_bpls*ScrBpl,d0
       add.l d0,a0

       lea ScrBplP,a1
       moveq #fontbpls-1,d0
.l4:
       move.l a0,d1
       swap d1
       move d1,2(a1)
       swap d1
       move d1,6(a1)

       addq #8,a1
       lea ScrBpl(a0),a0
       dbf d0,.l4

       movem.l (sp)+,d0-a6
       rts

StartMusic:
; Init LSP and start replay using easy CIA toolbox
       movem.l a5-a6,-(a7) 
       lea		LSPMusic,a0
       lea		LSPBank,a1
       suba.l	a2,a2			; suppose VBR=0 ( A500 )
       moveq	#0,d0			; suppose PAL machine
       bsr		LSP_MusicDriver_CIA_Start
       movem.l (a7)+,a5-a6
       rts


rotateMoves:	              dc.b	-1, 3, 1, 0
				dc.b	10, -1, 1, 0
				dc.b	11, -1, 1, 0
				dc.b	-1, 1, 1, 1
				dc.b	13, -1, 1, 0
				dc.b	14, -1, 1, 0
				dc.b	-1, 1, 1, 1
				dc.b	-1, 2, 1, 1
				dc.b	17, 2, 1, 0
				dc.b	18, -1, 1, 0
				dc.b	19, -1, 1, 0
				dc.b	-1, 1, 2, 1
				dc.b	14, -1, 1, 0
				dc.b	10, -1, 1, 0
				dc.b	-1, 1, 1, 0
				dc.b	-1, 3, 1, 0
				dc.b	13, 2, 1, 0
				dc.b	-1, 1, 1, 0
				dc.b	15, 1, 1, 1
				dc.b	20, 1, 1, 1
				dc.b	-1, 2, 1, 0
				dc.b	-1, 2, 1, 1
				dc.b	19, -1, 1, 0
				dc.b	-1, 1, 1, 1
rotateMovesLength  EQU     *-rotateMoves

rectWidth=4
rectHeight=29

RotateQrCodePlanHorizontal:
		movem.l	d0-a6,-(a7)


		move	#rotateMovesLength,d3
		lsr.w	#2,d3
		sub.w	#1,d3
		
		lea		rotateMoves+rotateMovesLength-4,a0
		move.l	#-8,d2

		tst.b	d1
		bne		.backward

		lea		rotateMoves,a0
		move.l	#0,d2
.backward		
		
.loop
		clr.l	d6
		move.b	(a0)+,d6
		clr.l	d0
		move.b	(a0)+,d0
		clr.l	d4
		move.b	(a0)+,d4
		clr.l	d5
		move.b	(a0)+,d5
		eor		d1,d5
		bsr RotateQrCode2
		add.l	d2,a0		
		dbf		d3,.loop
		

		movem.l	(a7)+,d0-a6

		rts

;-----------------------------------------------------------------------------
;	; xline=1, yline=0, steps=2, direction=0
;	d0 = yline
;	d4 = steps
;	d5 = direction - 0 DownLeft, 1 RightUp
;	d6 = xline
;
;-----------------------------------------------------------------------------

RotateQrCode2:
		movem.l	d1-d3/d7/a0,-(a7)
		subq	#1,d4
		moveq	#rectWidth,d3

.stepsloop:
		lea		qrCode,a0
		move.l	a0,a1
		
		

		tst		d5
		beq		.directionDownLeft
.directionRightUp
		cmp.b	#$FF,d6
		beq		.doUp
.doRight:
		move	#rectWidth-1,d1
		move.l	d6,d7
		muls	#rectWidth,d7
		add.l	d7,a0
		add.l	#rectWidth-1,a0
		move.b	(a0),d2
.loop4
		move.b	-1(a0),(a0)
		sub		#1,a0
		dbf		d1,.loop4

		addq	#1,a0
		move.b	d2,(a0)

.doUp:
		cmp.b	#$ff,d0
		beq		.endStep
		lea		qrCode,a0
		move	#rectHeight-1,d1
		add.l	d0,a0
		move.b	(a0),d2
.loop
		move.b	(a0,d3),(a0)
		add		d3,a0
		dbf		d1,.loop

		sub		d3,a0
		move.b	d2,(a0)
		jmp		.endStep
		;dbf		d4,.stepsloop
		;jmp .exit
.directionDownLeft:
		cmp.b	#$FF,d0
		beq		.doLeft
.doDown:
		move	#rectHeight,d1
		subq	#1,d1
		add.l	d0,a0
		move	d3,d7
		muls	#rectHeight-1,d7
		add.l	d7,a0
		move.b	(a0),d2
.loop2
		sub.l	d3,a0
		move.b	(a0),(a0,d3)
		dbf		d1,.loop2

		add.l	d3,a0
		move.b	d2,(a0)

.doLeft:
		move	#rectWidth,d1
		subq	#1,d1
		cmp.b	#$ff,d6
		beq		.endStep
		lea		qrCode,a0
		move.l	d6,d7
		muls	#rectWidth,d7
		add.l	d7,a0
		move.b	(a0),d2
.loop3
		move.b	1(a0),(a0)
		add		#1,a0
		dbf		d1,.loop3

		subq	#1,a0
		move.b	d2,(a0)

.endStep:
		dbf		d4,.stepsloop
.exit:
		movem.l	(a7)+,d1-d3/d7/a0
		rts


;--------------------------------------------------------------

PlotQRCode:
		lea		qrCode,a0
		clr.l	d6
		move.l	#29-1,d0
              lea Screen,a1
		;move.l	screen(a6),a1
		move.l	a1,a2
		move.l	a1,a3
		jmp		StartRows
LoopQRRows:
		move	#QR_SCREEN_LINE,d6
		lsl.l	#3,d6
		move.l	a2,a1
		add.l	d6,a1
		move.l	a1,a2
StartRows:
		moveq	#16-1,d1
		
.nextbyte:
		move.b	(a0)+,d6
		move.l	#$80,d7
.nextbit:		
		move.l	a1,a3
		cmpi.b	#0,d7
		beq		.nextbyte

		move.w	#$0000,d2

		move.l	d6,d5
		
		and		d7,d5
		
		beq		.white

		; color is black
		

		move.w	#$FF00,d2
.white:	; color is white
		lsr		#1,d7
		
		move.b	#$00,d2

		move.l	d6,d5
		
		and		d7,d5
		
		beq		.white2

		; color is black
		
	; color is white
		move.b	#$FF,d2
.white2:
		lsr		#1,d7
		
		
.loopRow:
		move.w	#8-1,d4

.loop292:		
		move.w	d2,(a1)
		add.l	#QR_SCREEN_LINE,a1
		dbf d4,.loop292
		
		move.l	a3,a1
		add.l	#2,a1

		dbf d1,.nextbit

		dbf d0,LoopQRRows

		rts
       ; Include simple CIA toolkit
	include	"music/LightSpeedPlayer_cia.s"
       ; Include generic LSP player
	include	"music/LightSpeedPlayer.s"

************ DATA ****************
dt:		ds.b	dt_SIZEOF

       SECTION data,DATA_C
ScrollPtr:
       dc.l ScrollText
ScrollText:
       dc.b "     JOKERX IS PROUD TO PRESENT: HV23 DAY 21"
       dc.b " ----- "
       dc.b "CHALLENGE DONE BY: JOKERX ON DECEMBER 18, 2023"
       dc.b " ----- "
       dc.b "GREETS GO OUT TO JOGEIR LILJEDAHL FOR THE MUSIC AND TO ARNAUD CARRE FOR LSP"
       dc.b " ----- "
       dc.b "PRESS LMB TO SHOW QR-CODE OR CONTACT US!      "
       dc.b "POBOX 1337  1234 WONDERLAND!"
       dc.b " ----- "
       dc.b "SOON COMING MORE AND MORE AMIGA CHALLENGES FROM $$$$$ JOKERX $$$$$"
       dc.b " ----- "
       DC.B "CALL OUR BOARDS"
       dc.b " ----- "
       dc.b "$ HAPPY ISLAND $ JOKERX WORLD HQ (555)555-1337"
       dc.b "                 "
       
ScrollTextWrap:
LastChar:
       dc.b 0
       even

ScrollCtr:
       dc.w 0
ColorCycleCtr:
       dc.w 0
BounceY:
       dc.w 48*8
BounceYspeed:
       dc.w 0
BounceYaccel:
       dc.w -1
SineCtr:
       dc.w 0
gfxname:
       dc.b "graphics.library",0,0
Sine:  incbin "Sine.75.400.w"
SineEnd:

       SECTION TutData,DATA_C
Spr1:
       dc.w $0050,$1000
       dc.w %0000001010000000,%0000000000000000
       dc.w %0010010101001000,%0000000000000000
       dc.w %0101001010010100,%0000000000000000
       dc.w %1001011011010010,%0000000000000000
       dc.w %0110110001101100,%0000001010000000
       dc.w %0001000100010000,%0000010001000000
       dc.w %0001010101010000,%0000100100100000
       dc.w %0001001010010000,%0000101010100000
       dc.w %0001010101010000,%0000100100100000
       dc.w %0001000100010000,%0000010001000000
       dc.w %0110110001101100,%0000001010000000
       dc.w %1001001010010010,%0000000000000000
       dc.w %0101001010010100,%0000000000000000
       dc.w %0010010101001000,%0000000000000000
       dc.w %0000001010000000,%0000000000000000
       dc.w %0000000000000000,%0000000000000000
       dc.w 0,0
Spr1Size equ *-Spr1
Spr2:
       dc.w $10c0,$2000
       dc.w %0000001010000000,%0000000000000000
       dc.w %0010010101001000,%0000000000000000
       dc.w %0101001010010100,%0000000000000000
       dc.w %1001011011010010,%0000000000000000
       dc.w %0110110001101100,%0000001010000000
       dc.w %0001000100010000,%0000010001000000
       dc.w %0001010101010000,%0000100100100000
       dc.w %0001001010010000,%0000101010100000
       dc.w %0001010101010000,%0000100100100000
       dc.w %0001000100010000,%0000010001000000
       dc.w %0110110001101100,%0000001010000000
       dc.w %1001001010010010,%0000000000000000
       dc.w %0101001010010100,%0000000000000000
       dc.w %0010010101001000,%0000000000000000
       dc.w %0000001010000000,%0000000000000000
       dc.w %0000000000000000,%0000000000000000
       dc.w 0,0
Spr2Length  EQU     *-Spr2

NullSpr:
       dc.w $2a20,$2b00
       dc.w 0,0
       dc.w 0,0

       
copper2:
       dc.w $1fc,0
       dc.w $100,$0200
       dc.w $102,0
copper2bplP:
       dc.w bplpt,$0                 ; bitplane0 address
       dc.w bplpt+2,$0
       dc.w diwstrt,$2c81
       dc.w diwstop,$1480
       dc.w ddfstrt,$38
       dc.w ddfstop,$d0
       dc.w bpl1mod,0
       dc.w bpl2mod,0

       dc.w dmacon,$20                    ; disable sprites
       
       dc.w $0180,$FFF,$0182,$000
       
       dc.w bplcon0,QR_SCREEN_BPL*$1000+$200
       dc.w $ffff,$fffe            ; copper list end
copper2E:

       IF 1=0
BarBehind:
       dc.w $558
       dc.w $0111,$0322,$0411
       dc.w $0712,$0643,$0334,$0556

       dc.w $99d
       dc.w $0111,$0322,$0411
       dc.w $0712,$0643,$0334,$0556

       dc.w $fff
       dc.w $0111,$0322,$0411
       dc.w $0712,$0643,$0334,$0556

       dc.w $99d
       dc.w $0111,$0322,$0411
       dc.w $0712,$0643,$0334,$0556

       dc.w $558
       dc.w $0111,$0322,$0411
       dc.w $0712,$0643,$0334,$0556

       dc.w logobgcolor
       dc.w $0111,$0322,$0411
       dc.w $0712,$0643,$0334,$0556

BarInFront:
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558

       dc.w $99d
       dc.w $99d
       dc.w $99d
       dc.w $99d
       dc.w $99d
       dc.w $99d
       dc.w $99d
       dc.w $99d
       
       dc.w $fff
       dc.w $fff
       dc.w $fff
       dc.w $fff
       dc.w $fff
       dc.w $fff
       dc.w $fff
       dc.w $fff

       dc.w $99d
       dc.w $999
       dc.w $99d
       dc.w $999
       dc.w $99d
       dc.w $999
       dc.w $99d
       dc.w $999


       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558
       dc.w $558

       dc.w logobgcolor
       dc.w $0111,$0322,$0411
       dc.w $0712,$0643,$0334,$0556

       ENDC
copper:
       dc.w $1fc,0                 ; slow fetch mode, AGA compatibility
       dc.w bplcon0,$0200             ; enable color burst output signal
                                   ; to make compatible with all amigas

       dc.w $102,0                 ;disable playfield 2

       ;dc.w $1a2,$ff0              ; sprite colors
       ;dc.w $1a4,$f00
       ;dc.w $1a6,$000

SprP:                              ; 8 x hi & low word Ptr address
       dc.w $120,$0
       dc.w $122,$0
       dc.w $124,$0
       dc.w $126,$0
       dc.w $128,$0
       dc.w $12a,$0
       dc.w $12c,$0
       dc.w $12e,$0
       dc.w $130,$0
       dc.w $132,$0
       dc.w $134,$0
       dc.w $136,$0
       dc.w $138,$0
       dc.w $13a,$0
       dc.w $13c,$0
       dc.w $13e,$0

CopBplP:
       dc.w bplpt,$0                 ; bitplane0 address
       dc.w bplpt+2,$0
       dc.w bplpt+4,$0                 ; bitplane1 address
       dc.w bplpt+6,$0
       dc.w bplpt+8,$0                 ; bitplane2 address
       dc.w bplpt+$a,$0
       dc.w bplpt+$c,$0                 ; bitplane3 address
       dc.w bplpt+$e,$0
       dc.w bplpt+$10,$0                 ; bitplane4 address
       dc.w bplpt+$12,$0
       dc.w diwstrt,$2c81
       dc.w diwstop,$2cc1
       dc.w ddfstrt,$38;+logomargin/2
       dc.w ddfstop,$d0;-logomargin
       dc.w bpl1mod,logoScreenBpl*logobitplanes-(320/8)
       dc.w bpl2mod,logoScreenBpl*logobitplanes-(320/8)
       
       dc.w color,logobgcolor

       dc.w $0182,$0111,$0184,$0322,$0186,$0411
       dc.w $0188,$0712,$018A,$0643,$018C,$0334,$018E,$0556
       dc.w $0190,$0812,$0192,$0923,$0194,$0833,$0196,$0933
       dc.w $0198,$0A23,$019A,$0A34,$019C,$0C34,$019E,$0944
       dc.w $01A0,$0955,$01A2,$027c,$01A4,$0bef,$01A6,$06bf
       dc.w $01A8,$0977,$01AA,$0B55,$01AC,$0E45,$01AE,$0e67
       dc.w $01B0,$0B86,$01B2,$0A88,$01B4,$0B99,$01B6,$0CAA
       dc.w $01B8,$0CBB,$01BA,$0DB8,$01BC,$0DCC,$01BE,$0EDD
       dc.w bplcon0,logobitplanes*$1000+$200

       dc.w $2c07,$fffe
       
waitras1:
       dc.w $8007,$fffe
       dc.w $180,$77c
waitras2:
       dc.w $8107,$fffe
       dc.w $180,$99d
waitras3:
       dc.w $8207,$fffe
       dc.w $180,$fff
waitras4:
       dc.w $8307,$fffe
       dc.w $180,$99d
waitras5:
       dc.w $8407,$fffe
       dc.w $180,$77c
waitras6:
       dc.w $8507,$fffe
       dc.w $180,logobgcolor

       dc.w $c7df,$fffe
       dc.w $180,$fff
       dc.w bplcon0,$0200
       ;dc.w $c8e8,$fffe

ScrBplP:
       dc.w bplpt,$0                 ; bitplane0 address
       dc.w bplpt+2,$0
       dc.w bplpt+4,$0                 ; bitplane1 address
       dc.w bplpt+6,$0
       dc.w bplpt+8,$0                 ; bitplane2 address
       dc.w bplpt+$a,$0
       dc.w bpl1mod,ScrBpl*fontbpls-320/8
       dc.w bpl2mod,ScrBpl*fontbpls-320/8
       
       
       dc.w $c801,$fffe
       dc.w bplcon0,fontbpls*$1000+$200 ; set bitplane(s)
       
Font2PalP:
       dc.w $0182,$0BEF,$0184,$09CF,$0186,$07AD
       dc.w $0188,$058B,$018A,$0369,$018C,$0147,$018E,$0C22

       dc.w $c907,$fffe
       dc.w $180,$eef
       dc.w $ca07,$fffe
       dc.w $180,$fff
       dc.w $cb07,$fffe
       dc.w $180,$eef

       dc.w $cc07,$fffe
       dc.w $180,$ddf
       dc.w $cd07,$fffe
       dc.w $180,$eef
       dc.w $ce07,$fffe
       dc.w $180,$ddf

       dc.w $cf07,$fffe
       dc.w $180,$ccf
       dc.w $d007,$fffe
       dc.w $180,$ddf
       dc.w $d107,$fffe
       dc.w $180,$ccf

       dc.w $d207,$fffe
       dc.w $180,$bbf
       dc.w $d307,$fffe
       dc.w $180,$ccf
       dc.w $d407,$fffe
       dc.w $180,$bbf

       

       dc.w $d507,$fffe
       dc.w $180,$aaf
       dc.w $d607,$fffe
       dc.w $180,$bbf
       dc.w $d707,$fffe
       dc.w $180,$aaf

       dc.w $d807,$fffe
       dc.w $180,$99f
       dc.w $d907,$fffe
       dc.w $180,$aaf
       dc.w $da07,$fffe
       dc.w $180,$99f

       dc.w $db07,$fffe
       dc.w $180,$88f
       dc.w $dc07,$fffe
       dc.w $180,$99f
       dc.w $dd07,$fffe
       dc.w $180,$88f

       dc.w $de07,$fffe
       dc.w $180,$77f
       dc.w $df07,$fffe
       dc.w $180,$88f
       dc.w $e007,$fffe
       dc.w $180,$77f

       dc.w $e107,$fffe
       dc.w $180,$66f
       dc.w $e207,$fffe
       dc.w $180,$77f
       dc.w $e307,$fffe
       dc.w $180,$66f

       dc.w $e407,$fffe
       dc.w $180,$55f
       dc.w $e507,$fffe
       dc.w $180,$66f
       dc.w $e607,$fffe
       dc.w $180,$55f

       dc.w $e707,$fffe
       dc.w $180,$44f
       dc.w $e807,$fffe
       dc.w $180,$55f
       dc.w $e907,$fffe
       dc.w $180,bgcolor

       dc.w $f507,$fffe
       

       
       ;dc.w $ffdf,$fffe            ; wait till line 255, to catch line above it
       ;dc.w $2c07,$fffe

       dc.w $ffff,$fffe            ; copper list end
CopperE:

       IF 1=0
Font:
       INCBIN "font.font"               ; 8x8
FontE:
       ENDC
Font2:
       INCBIN "font_bitmap.raw"
Font2E:
Font2Pal = Font2E-8*2

logo:
       incbin "hackvent_kugel.raw"
logoE:
       dcb.b logobwid*7,0


qrCode:
	IFEQ RELEASE
	incbin "qrcode_hv23_shuffled.bin"
	else
	incbin "qrcode_hv23_shuffled.bin"
	endc
	even

       SECTION sound,DATA_C
LSPBank:	incbin	"music/christ_1.lsbank"
              even

LSPMusic:	incbin	"music/christ_1.lsmusic"
              even

       SECTION TutBSS,BSS_C
Screen:
       ds.b SCREEN_MEMSIZE
LogoScreen:
       ds.b LogoScreen_MemSize
END