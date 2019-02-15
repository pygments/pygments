; Part of this file comes from http://coppershade.org/asmskool/Tut22.S

; WARNING: I arbitrarily cut the files to get many different cases
; of M68k code. This file is not supposed to work if compiled.

; Comment
* Comment
    ; Indented comment
    * Indented comment
*********** We love titles ***********

; This should mark asterisks ar comment but not the line after them

**
section SomeSection


Label:


*********** VASM directives without arguments ***********

bss
bss_c
bss_f
clrfo
clrso
code
code_c
code_f
comment
cseg
data
data_c
data_f
dseg
einline
else
end
endif
endm
endr
erem
even
inline
list
mexit
nolist
nopage
odd
page
rem
rsreset
rsset
text

*********** VASM typed directives with a single argument ***********

blk.b 5
blk.b 5,456
blk.d 5,456
blk.l 5,456
blk.q 5,456
blk.s 5,456
blk.w 5,456
blk.x 5,456
dc.b "somestring"
dc.b 'somestring'
dc.b SomeLabel
dc.b 123,456
dc.b 3
dc.d 123,456
dc.l 123,456
dc.q 123,456
dc.s 123,456
dc.w 123,456
dc.x 123,456
dcb.b 234
dcb.b 234.567
dcb.d 234.567
dcb.l 234.567
dcb.q 234.567
dcb.s 234.567
dcb.w 234.567
dcb.x 234.567
dr.b 1
dr.b 1,2
dr.l 1,2
dr.w 1,2
ds.b 1234
ds.d 1234
ds.l 1234
ds.q 1234
ds.s 1234
ds.w 1234
ds.x 1234
ds.x 1234 ; Testing comments after arguments

*********** VASM untyped directives with a single argument ***********

align 2
echo "Somestring"
fail "ARGH!"
if 1
ifb 123
ifeq 1
ifge 1
ifgt 1
ifle 1
iflt 1
ifnb 321
ifne 1
incdir "some/path"
include "somefile.asm"
llen 123
offset 1234
org 123
output "someoutput"
plen 123
rept 123
rorg 123
setfo 123
setso 123
spc 123
spc 123 ; Testing comments after arguments

*********** VASM untyped directives with a multiple arguments ***********

ifc "string1","string2"
ifnc "string1","string2"
incbin "somefile.bin"
incbin "somefile.bin",123
incbin "somefile.bin",123,321
printt "Just a string"
printt "Just a string","And another"
cnop 30,2
printv 123
printv 123,456
printv 123,456 ; Testing comments after arguments

*********** VASM directives for symbol definitions ***********

ABCDEF equ 1234
ABCDEF equ.d 1234
ABCDEF equ.p 1234
ABCDEF equ.s 1234
ABCDEF equ.x 1234
ABCDEF fequ.d 1234
ABCDEF fequ.p 1234
ABCDEF fequ.s 1234
ABCDEF fequ.x 1234
ABCDEF fo.b 1234
ABCDEF fo.d 1234
ABCDEF fo.l 1234
ABCDEF fo.p 1234
ABCDEF fo.q 1234
ABCDEF fo.s 1234
ABCDEF fo.w 1234
ABCDEF fo.x 1234
ABCDEF rs.b 123
ABCDEF rs.d 123
ABCDEF rs.l 123
ABCDEF rs.p 123
ABCDEF rs.q 123
ABCDEF rs.s 123
ABCDEF rs.w 123
ABCDEF rs.x 123
ABCDEF set 12345
ABCDEF so.b 123
ABCDEF so.d 123
ABCDEF so.l 123
ABCDEF so.p 123
ABCDEF so.q 123
ABCDEF so.s 123
ABCDEF so.w 123
ABCDEF so.x 123
ABCDF = 123
ABCDF =.d 123
ABCDF =.p 123
ABCDF =.s 123
ABCDF =.x 123
Name ttl ; This is not a proper definition, but the keyword follows the argument

*********** VASM directive for sections ***********

; Sections are tricky, because they can use keywords as a second argument
; (code, text, etc.), and two specific values for the third argument
; (chip, fast) which are not keywords

section Mysec
section Mysec,code
section Mysec,text
section Mysec,data
section Mysec,bss
section Mysec,code,chip
section Mysec,code,fast

; Here "chip" and "fast" should be coloured like variables, not keywords
chip equ 1
fast equ 3

; Here the second and third arguments shouldn't be coloured, as they are wrong
section Mysec,date ; <--- SHOULD FAIL
section Mysec,date,chip ; <--- SHOULD FAIL
section Mysec,data,chop ; <--- SHOULD FAIL


; Check that comments do now get used from later lines
fast equ 3

; Here the second and third arguments shouldn't be coloured, as they are wrong
section Mysec ; <--- This should be correct


; Check that comments do now get used from later lines
fast equ 3

* Here the second and third arguments shouldn't be coloured, as they are wrong
section Mysec ; <--- This should be correct


*********** VASM directive for macros ***********

; Standard VASM syntax
macro structure

; Amiga syntax
STRUCTURE   MACRO       ; structure name, initial offset
\1      EQU     0
SYMBOL1  SET     \@
SYMBOL1  SET     \@!
SYMBOL1  SET     \@?
SYMBOL1  SET     \@@
SYMBOL1  SET     \#
SYMBOL1  SET     \?0
SYMBOL1  SET     \?9
SYMBOL1  SET     \.
SYMBOL1  SET     \+
SYMBOL1  SET     \-
SYMBOL1  SET     \SYMBOL2
SYMBOL1  SET     \$SYMBOL2
SOFFSET     SET     \2
        ENDM

*********** VASM directives with symbol as arguments ***********

cargs #4,VALUEONE,VALUETWO.b,VALUE3.w
comm ABCDEF,40
idnt Name
ifd ABCDEF
ifmacrod MYMACRO
ifmacrond MYMACRO
ifnd ABCDEF
nref ABCDEF
nref ABCDEF,FEDCBA
public ABCDEF
public ABCDEF,FEDCBA
ttl Name
weak ABCDEF
weak ABCDEF,FEDCBA
xdef ABCDEF
xdef ABCDEF,FEDCBA
xref ABCDEF
xref ABCDEF,FEDCBA

*********** VASM ignored directives ***********

load Something
jumpptr Something
jumperr Something


*********** Motorola 68k instructions without arguments ***********

InstrNoArgs:
    illegal
    nop
    reset
    rte
    rtr
    rts
    stop
    trapv
    trapv ; Testing comments after arguments

*********** Motorola 68k instructions without type ***********

InstrNoType:
    jmp -408(a6)
    jsr -408(a6)
    trap -408(a6)
    trap -408(a6) ; Testing comments after arguments

*********** Motorola 68k instructions with type ***********

InstrType:
    move.b 4,a6
    move.w 4,a6
    move.s 4,a6
    move.l 4,a6
    move.l 4,a6 ; Testing comments after arguments


; Check that keywords are not higlighted if they are used in names

section rem
include "blk.b"


    SECTION TutDemo,CODE
    JUMPPTR Start

    INCDIR ""
    INCLUDE "Blitter-Register-List.S"


Start:

OSoff:
    movem.l d1-a6,-(sp)
    move.l 4.w,a6       ;execbase
    move.l #gfxname,a1
    jsr -408(a6)        ;oldopenlibrary()
    move.l d0,a1
    move.l 38(a1),d4    ;original copper ptr

    jsr -414(a6)        ;closelibrary()

    move.w #$4c-6,d7    ;start y position
    moveq #1,d6     ;y add
    move.w $dff01c,d5
    move.w $dff002,d3

    move.w #$138,d0     ;wait for EOFrame
    bsr.w WaitRaster
    move.w #$7fff,$dff09a   ;disable all bits in INTENA
    move.w #$7fff,$dff09c   ;disable all bits in INTREQ
    move.w #$7fff,$dff09c   ;disable all bits in INTREQ
    move.w #$7fff,$dff096   ;disable all bits in DMACON
    move.w #$87e0,$dff096

    bsr Init

    move.l #copper,$dff080
    bsr Main

OSon:

    move.w #$7fff,$dff096
    or.w #$8200,d3
    move.w d3,$dff096
    move.l d4,$dff080
    or #$c000,d5
    move d5,$dff09a
    movem.l (sp)+,d1-a6
    moveq #0,d0
    rts         ;end of program return to AmigaOS



********** ROUTINES **********
Main:
    movem.l d0-a6,-(sp)

**************************
 ***************

MainLoop:
    move.w #$02a,d0     ;wait for EOFrame
    bsr.w WaitRaster

;-----frame loop start---
    bsr BounceScroller

    add.b #1,Spr+1

    add d6,d7       ;add "1" to y position

    cmp #$4c+logoh+1,d7 ;bottom check
    blo.b ok1
    neg d6          ;change direction
ok1:

    cmp.b #$4c-6,d7     ;top check
    bhi.b ok2
    neg d6          ;change direction
ok2:

    move.l #waitras1,a0
    move d7,d0
    moveq #6-1,d1
.l:
    move.b d0,(a0)
    add.w #1,d0
    add.w #8,a0
    DBF d1,.l

    bsr Scrollit

    moveq #32,d2
    move.b LastChar(PC),d0
    cmp.b #'I',d0
    bne.s .noi
    moveq #16,d2
    move.l #$09f00000,BLTCON0(a6)
    move.l #$ffffffff,BLTAFWM(a6)
    move.l d0,BLTAPTH(a6)
    move.l #Screen+ScrBpl*3*plotY+plotX/8,BLTDPTH(a6)
    move.w #FontBpl-col,BLTAMOD(a6)
    move.w #ScrBpl-col,BLTDMOD(a6)

    move.w #20*3*64+2,BLTSIZE(a6)
    movem.l (sp)+,d0-a6
    rts

    movem.l d0-a6,-(sp)
    lea $dff000,a6
    bsr BlitWait

    move.l #$49f00002,BLTCON0(a6)
    move.l #$ffffffff,BLTAFWM(a6)
    move.l #Screen+bltoffs+brcorner,BLTAPTH(a6)
    move.l #Screen+bltoffs+brcorner,BLTDPTH(a6)
    move.w #bltskip,BLTAMOD(a6)
    move.w #bltskip,BLTDMOD(a6)

    move.w #blth*3*64+bltw,BLTSIZE(a6)
    movem.l (sp)+,d0-a6
    rts

    MOVEM.L (SP)+,D0-A6
    RTS

********** DATA **********
FontTbl:
    dc.b 43,38
    dc.d 43,38
    dc.l 43,38
    dc.q 43,38
    dc.s 43,38
    dc.w 43,38
    dc.x 43,38
    dcb.b 42
    dcb.d 42
    dcb.l 42
    dcb.q 42
    dcb.s 42
    dcb.w 42
    dcb.x 42
    blk.b 5,0
    blk.d 5,0
    blk.l 5,0
    blk.q 5,0
    blk.s 5,0
    blk.w 5,0
    blk.x 5,0
    ds.b 43,38
    ds.d 43,38
    ds.l 43,38
    ds.q 43,38
    ds.s 43,38
    ds.w 43,38
    ds.x 43,38
    dr.b 5,38
    dr.w 5,38
    dr.l 5,38
    dc.b 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21
    EVEN

ScrollPtr:
    dc.l ScrollText
ScrollText:
    dc.b "JUST FOR THE RETRO FEELING OF IT! YOU DIG??"
    blk.b w/32,' '
ScrollTextWrap:

LastChar:dc.b 0
    EVEN
ScrollCtr:
    dc.w 0
BounceY:
    dc.w 48
BounceYspeed:
    dc.w 0
BounceYaccel:
    dc.w -1


gfxname:
    dc.b "graphics.library",0

Spr:
    dc.w $ec40,$fc00    ;Vstart.b,Hstart/2.b,Vstop.b,%A0000SEH
    dc.w %0000011111000000,%0000000000000000
    dc.w %0001111111110000,%0000000000000000
    dc.w %0011111111111000,%0000000000000000
    dc.w %0111111111111100,%0000000000000000
    dc.w %0110011111001100,%0001100000110000
    dc.w %1110011111001110,%0001100000110000
    dc.w %1111111111111110,%0000000000000000
    dc.w %1111111111111110,%0000000000000000
    dc.w %1111111111111110,%0010000000001000
    dc.w %1111111111111110,%0001100000110000
    dc.w %0111111111111100,%0000011111000000
    dc.w %0111111111111100,%0000000000000000
    dc.w %0011111111111000,%0000000000000000
    dc.w %0001111111110000,%0000000000000000
    dc.w %0000011111000000,%0000000000000000
    dc.w %0000000000000000,%0000000000000000
    dc.w 0,0


*********** VDA disassembled code ***********

00000000: 1111                      move.b  (a1),-(a0)
00000002: 4ef9 00fc 00d2            jmp     0xfc00d2.l
00000008: 0000 ffff                 ori.b   #-0x1,d0
0000000c: 0022 0005                 ori.b   #0x5,-(a2)
00000010: 0022 0002                 ori.b   #0x2,-(a2)
00000014: ffff                      linef   
00000016: ffff                      linef   

00000018: 6a ; j
00000019: 75 ; u
0000001a: 73 ; s
0000001b: 74 ; t
0000001c: 20 ; SP
0000001d: 61 ; a
0000001e: 20 ; SP
0000001f: 73 ; s
00000020: 74 ; t
00000021: 72 ; r
00000022: 69 ; i
00000023: 6e ; n
00000024: 67 ; g

00000034: ffff                      linef   
00000036: ffff                      linef   
00000038: 0d0a 0a41                 movep.w 0xa41(a2),d6
0000003c: 4d49                              
0000003e: 4741                              
00000040: 2052                      movea.l (a2),a0
00000042: 4f4d                              
00000044: 204f                      movea.l sp,a0
00000046: 7065                      moveq   #0x65,d0
00000048: 7261                      moveq   #0x61,d1

; Comments inside decompiled code work
0000004a: 7469                      moveq   #0x69,d2
0000004c: 6e67                      bgt.b   0xb5
0000004e: 2053                      movea.l (a3),a0
00000050: 7973                      moveq   #0x73,d4
00000052: 7465                      moveq   #0x65,d2
00000054: 6d20                      blt.b   0x76
00000056: 616e                      bsr.b   0xc6
00000058: 6420                      bcc.b   0x7a
0000005a: 4c69 6272 6172            divul.l 0x6172(a1),d2:d6
00000060: 6965                      bvs.b   0xc7
00000062: 730d                      moveq   #0xd,d1
00000064: 0a43 6f70                 eori.w  #0x6f70,d3
00000068: 7972                      moveq   #0x72,d4
0000006a: 6967                      bvs.b   0xd3
0000006c: 6874                      bvc.b   0xe2
0000006e: 2028 4329                 move.l  0x4329(a0),d0
00000072: 2031 3938 352c 2043       move.l  (0x352c2043,a1,d3.l),d0
0000007a: 6f6d                      ble.b   0xe9
0000007c: 6d6f                      blt.b   0xed
0000007e: 646f                      bcc.b   0xef
00000080: 7265                      moveq   #0x65,d1
00000082: 2d41 6d69                 move.l  d1,0x6d69(a6)
00000086: 6761                      beq.b   0xe9
00000088: 2c20                      move.l  -(a0),d6
0000008a: 496e                              
0000008c: 632e                      bls.b   0xbc
0000008e: 0d0a 416c                 movep.w 0x416c(a2),d6
00000092: 6c20                      bge.b   0xb4
00000094: 5269 6768                 addq.w  #0x1,0x6768(a1)
00000098: 7473                      moveq   #0x73,d2
0000009a: 2052                      movea.l (a2),a0
0000009c: 6573                      bcs.b   0x111
0000009e: 6572                      bcs.b   0x112
000000a0: 7665                      moveq   #0x65,d3
000000a2: 642e                      bcc.b   0xd2
000000a4: 0d0a 0000                 movep.w 0(a2),d6

