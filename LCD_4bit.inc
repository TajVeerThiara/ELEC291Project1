$NOLIST
cseg

; When using a 22.1184MHz crystal in fast mode
; one cycle takes 1.0/22.1184MHz = 45.21123 ns


;---------------------------------;
; Wait 40 microseconds            ;
;---------------------------------;
Wait40uSec:
	push AR0
	mov R0, #177
L0: 
	nop
	nop
	djnz R0, L0 ; 1+1+3 cycles->5*45.21123ns*177=40us
	pop AR0
    ret

;---------------------------------;
; Wait 'R2' milliseconds          ;
;---------------------------------;
Wait_Milli_Seconds mac
	push AR2
	mov R2, %0
	lcall ?Wait_Milli_Seconds
	pop AR2
endmac

?Wait_Milli_Seconds:
	push AR0
	push AR1
L3: mov R1, #45
L2: mov R0, #166
L1: djnz R0, L1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, L2 ; 22.51519us*45=1.013ms
    djnz R2, L3 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret
	
Wait_5Milli_Seconds mac
	push AR2
	mov R2, %0
	lcall ?Wait_5Milli_Seconds
	pop AR2
endmac

?Wait_5Milli_Seconds:
	push AR0
	push AR1
L6: mov R1, #(45*5)
L5: mov R0, #166
L4: djnz R0, L4 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, L5 ; 22.51519us*45=1.013ms
    djnz R2, L6 ; number of millisecons to wait passed in R2
    pop AR1
    pop AR0
    ret

TEMPERATURE_CHECKER mac
	
	mov x+3, #0
	mov x+2, #0
	mov x+1, %3
	mov x+0, bcd+0
	mov y+3, #0
	mov y+2, #0
	mov y+1, %1
	mov y+0, %0
	lcall %2
endmac

beep_once:
	setb TR0
	setb SOUND_OUT
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	clr TR0
	clr SOUND_OUT
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
ret

BEEP_2SEC:
	setb TR0
	setb SOUND_OUT
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	clr TR0
	clr SOUND_OUT
ret
;---------------------------------;
; Toggles the 'E' pin in the LCD  ;
;---------------------------------;
LCD_pulse:
	setb LCD_E
	lcall Wait40uSec
	clr LCD_E
    ret

;---------------------------------;
; Writes acc to LCD in 4-bit mode ;
;---------------------------------;
LCD_byte:
	; Write high 4 bits first
	mov c, ACC.7
	mov LCD_D7, c
	mov c, ACC.6
	mov LCD_D6, c
	mov c, ACC.5
	mov LCD_D5, c
	mov c, ACC.4
	mov LCD_D4, c
    lcall LCD_pulse
    lcall Wait40uSec
	; Write low 4 bits next
	mov c, ACC.3
	mov LCD_D7, c
	mov c, ACC.2
	mov LCD_D6, c
	mov c, ACC.1
	mov LCD_D5, c
	mov c, ACC.0
	mov LCD_D4, c
    lcall LCD_pulse
	ret

;---------------------------------;
; Write data to LCD               ;
;---------------------------------;
WriteData mac
	mov a, %0
	lcall ?WriteData
endmac
	
?WriteData:
	setb LCD_RS
	ljmp LCD_byte

;---------------------------------;
; Write command to LCD            ;
;---------------------------------;
WriteCommand mac
	mov a, %0
	lcall ?WriteCommand
endmac

?WriteCommand:
	clr LCD_RS
	ljmp LCD_byte

;---------------------------------;
; Configure LCD in 4-bit mode     ;
;---------------------------------;
LCD_4BIT:
	clr LCD_E   ; Resting state of LCD's enable pin is zero
	clr LCD_RW  ; We are only writing to the LCD in this program
	
	; After power on, let the LCD start up before initializing
	; NOTE: the preprogrammed power-on delay of 16 ms on the AT89LP52
	; seems to be enough.  That is why this line is commented out.
	; Wait_Milli_Seconds(#40)
	
	; First make sure the LCD is in 8-bit mode and then change to 4-bit mode
	WriteCommand(#0x33)
	WriteCommand(#0x33)
	WriteCommand(#0x32) ; change to 4-bit mode

	; Configure the LCD
	WriteCommand(#0x28)
	WriteCommand(#0x0c)
	WriteCommand(#0x01) ;  Clear screen command (takes some time)

    ;Wait for the clear screen command to finish.
	Wait_Milli_Seconds(#2)
    ret

;---------------------------------;
; Send a constant string to LCD   ;
;---------------------------------;
Send_Constant_String mac
	push dph
	push dpl
	push acc
	mov dptr, %0
	lcall ?Send_Constant_String
	pop acc
	pop dpl
	pop dph
endmac

?Send_Constant_String:
    clr a
    movc a, @a+dptr
    jz ?Send_Constant_String_Done
    lcall ?WriteData
    inc dptr
    sjmp ?Send_Constant_String
?Send_Constant_String_Done:
    ret	

;---------------------------------;
; Set LCD cursor at row, column   ;
;---------------------------------;
Set_Cursor mac
	push acc
	mov a, #%1
	dec a
	lcall ?Set_Cursor_%0 ; Select column and row
	pop acc
endmac

?Set_Cursor_2:
	orl a, #01000000B
?Set_Cursor_1:
	orl a, #10000000B
	ljmp ?WriteCommand ; Select column and row

;---------------------------------;
; Display a BCD number in the LCD ;
;---------------------------------;
Display_BCD mac
	push ar0
	mov r0, %0
	lcall ?Display_BCD
	pop ar0
endmac

?Display_BCD:
	push acc
	; Write most significant digit
	mov a, r0
	swap a
	anl a, #0fh
	orl a, #30h
	lcall ?WriteData
	; write least significant digit
	mov a, r0
	anl a, #0fh
	orl a, #30h
	lcall ?WriteData
	pop acc
	ret

;------------------------------------;
; Display a char in the LCD          ;
;------------------------------------;
DISPLAY_SOAK_PROFILE:
	lcall CLEAR_LCD_SCREEN
	Set_Cursor(1, 13)
	Send_Constant_String(#SOAK_MESSAGE)
	Set_Cursor(1, 2)
	Display_BCD(sec_soak)
	Set_Cursor(2, 3)
	Display_BCD(SOAK_TEMP+0)
	Set_Cursor(2, 1)
	Display_BCD(SOAK_TEMP+1)
	Set_Cursor(1, 6)
    Send_Constant_String(#SEC_SYMBOL)
	Set_Cursor(2, 6)
	Send_Constant_String(#DEGREE_SYMBOL)
	ret
	
DISPLAY_REFLOW_PROFILE:
	lcall CLEAR_LCD_SCREEN
	Set_Cursor(1, 11)
	Send_Constant_String(#REFLOW_MESSAGE)
	Set_Cursor(1, 2)
	Display_BCD(sec_REFLOW)
	Set_Cursor(2, 3)
	Display_BCD(REFLOW_TEMP+0)
	Set_Cursor(2, 1)
	Display_BCD(REFLOW_TEMP+1)
	Set_Cursor(1, 6)
    Send_Constant_String(#SEC_SYMBOL)
	Set_Cursor(2, 6)
	Send_Constant_String(#DEGREE_SYMBOL)
	ret
	
PB_MACROO mac
	jb %0, %1
	Wait_Milli_Seconds(#50) 
	jb %0, %1
	jnb %0, $ 
	
	mov a, %2
	add a, #1H
	da a
	mov %2, a
	lcall %6
	mov x+3, #0
	mov x+2, #0
	mov x+1, %3
	mov x+0, %2
	mov y+3, #0
	mov y+2, #0
	mov y+1, %4
	mov y+0, %5
	lcall x_eq_y
	
endmac

CLEAR_LCD_SCREEN:
     WriteCommand(#1)

    ;Wait for clear screen command to finish. Usually takes 1.52ms.

     Wait_Milli_Seconds(#2)
    ret

DISPLAY_RUN_TEMP mac
	Set_Cursor(1, 2)
	Display_BCD(min)
	Set_Cursor(1, 4)
	Send_Constant_String(#COLON_SYMBOL)
	Set_Cursor(1, 5)
	Display_BCD(sec)
	Set_Cursor(1, 10)
	Send_Constant_String(%0)
	Set_Cursor(2, 1)
	Send_Constant_String(#TEMP_MESSAGE)
	Set_Cursor(2, 6)
	Display_BCD(bcd+1)
	Set_Cursor(2, 8)
	Display_BCD(bcd+0)
	Set_Cursor(2, 11)
	Send_Constant_String(#DEGREE_SYMBOL)

endmac

clear_all:
	setb SSR
	clr mf
	clr STAGE1_DONE_FLAG
	clr STAGE2_DONE_FLAG
	clr STAGE3_DONE_FLAG
	clr STAGE4_DONE_FLAG
	clr STAGE5_DONE_FLAG
	clr TERMINATION_ERROR_FLAG
	clr EA
	clr SOUND_OUT
	clr PREHEAT_TIMEDONE_FLAG
	clr REFLOW_TIMEDONE_FLAG
	
	mov min, #0x00
	mov sec, #0x00
	mov sec_soak, #0x00
	mov sec_REFLOW, #0x00
	mov SOAK_TEMP, #0x00
	mov SOAK_TEMP+1, #0x00
	mov REFLOW_TEMP, #0x00
	mov REFLOW_TEMP+1, #0x00
	mov bcd+0, #00H
	mov bcd+1, #00H
	mov bcd+2, #00H
	mov bcd+3, #00H
	mov bcd+4, #00H
	mov Count1ms+0, #00H
	mov Count1ms+1, #00H
	mov PREHEAT_TIME, #00H
	mov REFLOW_TEMP, #00H
	ret

Display_char mac
	push acc
	mov a, %0
	lcall ?WriteData
	pop acc
endmac

Send_BCD mac
    push ar0
    mov r0, %0
    lcall ?Send_BCD
    pop ar0
endmac

?Send_BCD:
    push acc
    ; Write most significant digit
    mov a, r0
    swap a
    anl a, #0fh
    orl a, #30h
    lcall putchar
    ; write least significant digit
    mov a, r0
    anl a, #0fh
    orl a, #30h
    lcall putchar
    pop acc
    ret
	
$LIST
