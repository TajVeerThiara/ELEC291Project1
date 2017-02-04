$MODLP52
org 0000H
   ljmp STARTTTT
   
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
	
CLK  EQU 22118400
BAUD equ 115200
T1LOAD equ (0x100-(CLK/(16*BAUD)))
VLED EQU 207
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))
TEMPERATURE EQU 20

DSEG at 30H
buffer: ds 30
Result: ds 10
x:   ds 4
y:   ds 4
bcd: ds 5
Count1ms: ds 2
beep_counter: ds 1

temp: ds 2
min:  ds 1
sec:  ds 1
sec_soak: ds 1
sec_REFLOW: ds 1
SOAK_TEMP: ds 2
REFLOW_TEMP: ds 2

BSEG
mf: dbit 1
soak_stage_beepdone_flag: dbit 1
reflow_stage_beepdone_flag: dbit 1
reflow_time_flag: dbit 1
long_beep_done_flag: dbit 1
cool_enough_flag: dbit 1
TERMINATION_ERROR_FLAG: dbit 1

CSEG
CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3
LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
SOUND_OUT     equ P3.7
BOOT_BUTTON   equ P4.5


SOAK_TEMP_BUTTON EQU P0.1
SOAK_SEC_BUTTON EQU P0.4
REFLOW_TEMP_BUTTON EQU P0.5
REFLOW_SEC_BUTTON EQU P0.7
START_BUTTON EQU P2.7
STOP_BUTTON EQU P2.6
;RESET EQU P0.8 

;                          1234567890123456    <- This helps determine the location of the counter
SEC_SYMBOL:  db 		  'SEC', 0
DEGREE_SYMBOL: db         'C', 0
SOAK_MESSAGE: db          'SOAK', 0
REFLOW_MESSAGE: db 		  'REFLOW', 0
PREHEAT_MESSAGE: db 	  'PREHEAT', 0
OVEN_MESSAGE:    db		  'OPEN OVEN', 0
 
$NOLIST
$include(LCD_4bit.inc)
$include(math32.inc)
$LIST

Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; In mode 1 we need to reload the timer.
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	                                                                                                           
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	mov a, Count1ms+0
	cjne a, #low(1000), jump_to_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), jump_to_done
	
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	jnb reflow_stage_beepdone_flag, jump_to_done
	mov a, beep_counter
	add a, #1
	mov beep_counter, a
	cjne a, #30, jump_to_done
	setb reflow_time_flag
	
jump_to_done:
	pop psw
	pop acc
	reti

INIT_SPI:
	setb MY_MISO ; Make MISO an input pin
	clr MY_SCLK ; For mode (0,0) SCLK is zero
	ret

DO_SPI_G:
	push acc
	mov R1, #0 ; Received byte stored in R1
	mov R2, #8 ; Loop counter (8-bits)
	
DO_SPI_G_LOOP:
	mov a, R0 ; Byte to write is in R0
	rlc a ; Carry flag has bit to write
	mov R0, a
	mov MY_MOSI, c
	setb MY_SCLK ; Transmit
	mov c, MY_MISO ; Read received bit
	mov a, R1 ; Save received bit in R1
	rlc a
	mov R1, a
	clr MY_SCLK
	djnz R2, DO_SPI_G_LOOP
	pop acc
	ret

; Configure the serial port and baud rate using timer 1
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, or risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can safely proceed with the configuration
	clr	TR1
	anl	TMOD, #0x0f
	orl	TMOD, #0x20
	orl	PCON,#0x80
	mov	TH1,#T1LOAD
	mov	TL1,#T1LOAD
	setb TR1
	mov	SCON,#0x52
    ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret
 
getchar:
	jnb RI, getchar
	clr RI
	mov a, SBUF
	ret

Hello_World:
    DB  '\r', '\n', 0

STARTTTT:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    mov PMOD, #0 ; Configure all ports in bidirectional mode
	lcall LCD_4BIT
	lcall clear_all
	Set_Cursor(1, 6)
    Send_Constant_String(#SEC_SYMBOL)
	Set_Cursor(2, 6)
	Send_Constant_String(#DEGREE_SYMBOL)
	mov SOAK_TEMP+1, #01H
	mov SOAK_TEMP+0, #30H
	mov REFLOW_TEMP+1, #02H
	mov REFLOW_TEMP+0, #00H
	mov sec_REFLOW, #25H
	mov sec_soak, #50H
	
SETTINGS:

jb START_BUTTON, PB_SEC_SOAK
Wait_Milli_Seconds(#50)
jb START_BUTTON, PB_SEC_SOAK
jnb START_BUTTON, $
ljmp MainProgram

PB_SEC_SOAK:
PB_MACROO (SOAK_SEC_BUTTON, PB_SOAK_TEMP, sec_soak, #00H, #00H, #90H, DISPLAY_SOAK_PROFILE)
jnb mf, SETTINGS1
mov sec_soak, #50H
clr mf
ljmp SETTINGS
;0.pushbutton 1.jump to if not pushed 2. x+0 and parameter to increment 3.x+1 4. y+1 5. y+0 6. display
														   ;(checks if soak time is over bound. if it is, set it to 0)
PB_SOAK_TEMP:
PB_MACROO (SOAK_TEMP_BUTTON, PB_SEC_REFLOW, SOAK_TEMP, SOAK_TEMP+1, #02H, #00H, DISPLAY_SOAK_PROFILE)
jnb mf, SETTINGS1
mov SOAK_TEMP, #30H
mov SOAK_TEMP+1, #01H
clr mf
ljmp SETTINGS

SETTINGS1:
	ljmp SETTINGS
	
PB_SEC_REFLOW:	
PB_MACROO (REFLOW_SEC_BUTTON, PB_REFLOW_TEMP, sec_REFLOW, #00H, #00H, #45H, DISPLAY_REFLOW_PROFILE)
jnb mf, SETTINGS1
mov sec_REFLOW, #25H
clr mf
ljmp SETTINGS

PB_REFLOW_TEMP:
PB_MACROO (REFLOW_TEMP_BUTTON, SETTINGS1, REFLOW_TEMP, REFLOW_TEMP+1, #02H, #35H, DISPLAY_REFLOW_PROFILE)
jnb mf, SETTINGS1
mov REFLOW_TEMP, #00H
mov REFLOW_TEMP+1, #02H
clr mf
ljmp SETTINGS


MainProgram:

	lcall InitSerialPort
	lcall INIT_SPI
	lcall Timer0_Init
	lcall Timer2_Init
	setb EA
	clr TR2
	lcall beep_once
	setb TR2
	; SSR ON/OFF timings should be calculated here using the parameters that we have set from above
Forever:

	clr CE_ADC
	mov R0, #00000001B ; Start bit:1
	lcall DO_SPI_G
	mov R0, #10000000B ; Single ended, read channel 0
	lcall DO_SPI_G
	mov a, R1 ; R1 contains bits 8 and 9
	anl a, #00000011B ; We need only the two least significant bits
	mov Result+1, a ; Save result high.
	mov R0, #55H ; It doesn't matter what we transmit...
	lcall DO_SPI_G
	mov Result, R1 ; R1 contains bits 0 to 7. Save result low.
	setb CE_ADC
	Wait_Milli_Seconds(#50)
	sjmp Do_Something_With_Result

	
Do_Something_With_Result:
	mov x+3, #0
	mov x+2, #0
	mov x+1, Result+1
	mov x+0, Result+0              ;change these formulas
	Load_y(500)
	lcall mul32
	Load_y(1023)
	lcall div32
	Load_y (273)
	lcall sub32
	lcall hex2bcd ; converts binary in x to BCD in bcd
	Send_BCD (bcd)
	mov DPTR, #Hello_World
	lcall Sendstring
	mov a, bcd+0
	 
	jb soak_stage_beepdone_flag, reflow_beeeep ;will make a macro for the this one and the ones repeated
	cjne a, #0x30, DONE1 ;change this so that it compares a current temp to the temp we want. Jumps to the appropriate SSR power on/off timing if not equal.
	;TEMPERATURE_CHECKER (SOAK_TEMP, SOAK_TEMP+1)
	;jnb mf, PREHEAT_FREQUENCY   if mf is 0, meaning that the temperature has not reached soak temp, jump to the appropriate SSR power on/off timing
	lcall beep_once ;gets to here if mf is 1
	setb soak_stage_beepdone_flag ;once this is set, it will stay on until STOP button is pressed
	ljmp forever

;PREHEAT_FREQUENCY:
	;clr SSR
	;Wait_Milli_Seconds(#200)	change these timings
	;setb SSR
	;Wait_Milli_Seconds(#200)  *****this off time is 200+50 milliseconds. 50 ms comes from the forever loop up there
	;ljmp forever
reflow_beeeep:
	jb reflow_stage_beepdone_flag, LONG_BEEEEEEEEEEEEP
	cjne a, #0x33, DONE1
	lcall beep_once
	setb reflow_stage_beepdone_flag
	ljmp forever 
	
DONE1:
	ljmp DONE
	
LONG_BEEEEEEEEEEEEP:
	jb long_beep_done_flag, COOOL_BEEEEP
	jnb reflow_time_flag, DONE1
	BEEP_2SEC(long_beep_done_flag)
	clr reflow_time_flag
	ljmp forever

COOOL_BEEEEP:
	cjne a, #0x30, DONE
	lcall beep_once
	lcall beep_once
	lcall beep_once
	lcall beep_once
	lcall beep_once
	lcall beep_once
	clr TR0
	clr SOUND_OUT
	clr long_beep_done_flag
	ljmp forever
	
DONE:
	Wait_Milli_Seconds (#200)
	Wait_Milli_Seconds (#200)
	Wait_Milli_Seconds (#200)
	ljmp forever

END