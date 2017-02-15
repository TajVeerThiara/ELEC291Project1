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

min:  ds 1
sec:  ds 1
sec_soak: ds 1
sec_REFLOW: ds 1
SOAK_TEMP: ds 2
REFLOW_TEMP: ds 2
PREHEAT_TIME: ds 1
REFLOW_TIME: ds 1
ON_TIME: ds 1
OFF_TIME: ds 1
ON_TIME_COUNTER: ds 1
OFF_TIME_COUNTER: ds 1
pwm: ds 1
TO_SOAK_PWM: ds 1
PREHEAT_PWM: ds 1
TO_PEAK_PWM: ds 1
REFLOW_PWM: ds 1
COOLING_PWM: ds 1

BSEG
mf: dbit 1
STAGE1_DONE_FLAG: dbit 1
STAGE2_DONE_FLAG: dbit 1
STAGE3_DONE_FLAG: dbit 1
STAGE4_DONE_FLAG: dbit 1
STAGE5_DONE_FLAG: dbit 1
TERMINATION_ERROR_FLAG: dbit 1
PREHEAT_TIMEDONE_FLAG: dbit 1
REFLOW_TIMEDONE_FLAG: dbit 1
SSR_ON_OFF_FLAG: dbit 1

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

SSR           EQU P0.1
SOAK_TEMP_BUTTON EQU P0.3
SOAK_SEC_BUTTON EQU P0.4
REFLOW_TEMP_BUTTON EQU P0.5
SLOW_COOK_BUTTON EQU P0.6
FAST_COOK_BUTTON EQU P2.6
REFLOW_SEC_BUTTON EQU P0.7
START_BUTTON EQU P2.4
STOP_BUTTON EQU P2.5
HOT_SURFACE_INDICATOR EQU P2.6
;RESET EQU P0.8 

;                          1234567890123456    <- This helps determine the location of the counter
COLON_SYMBOL: db		  ':', 0
TEMP_MESSAGE: db		  'TEMP:', 0
TO_SOAK_MEESAGE: db 	  'TO SOAK', 0
TO_PEAK_MESSAGE: db 	  'TO PEAK', 0
SEC_SYMBOL:  db 		  'SEC', 0
DEGREE_SYMBOL: db         'C', 0
SOAK_MESSAGE: db          'SOAK   ', 0
REFLOW_MESSAGE: db 		  'REFLOW ', 0
PREHEAT_MESSAGE: db 	  'PREHEAT', 0
COOLING_MESSAGE: db 	  'COOLING', 0
PROGRAM_STOPPED: db 	  'REFLOW PROCESS', 0
PROGRAM_STOPPED2: db	  'STOPPED!', 0
WELCOME_MEESAGE: db		  'WELCOME!', 0
RESTART_MESSAGE: db 	  'PRESS START TO', 0
RESTART_MESSAGE2: db      'CONTINUE', 0
 
$NOLIST
$include(project1_macro.inc)
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
JUMP_TO_DONE2:
	ljmp jump_to_done
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
	cjne a, #low(1000), JUMP_TO_DONE2 ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), JUMP_TO_DONE2
	
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a

	mov B, #10
	mov A, pwm
	DIV AB
	mov ON_TIME, A
	mov A, #10
	SUBB A, ON_TIME
	mov OFF_TIME, A
	
	jb STAGE4_DONE_FLAG, TURN_SSR_OFF
	sjmp NOT_ZERO_POWER
	
TURN_SSR_OFF:
	setb SSR
	ljmp CONT
	
NOT_ZERO_POWER:
	mov a, ON_TIME
	cjne a, #10, CHECK_SSR_ON_OFF
	mov OFF_TIME, #1

CHECK_SSR_ON_OFF:
	jnb SSR_ON_OFF_FLAG, COUNT_OFF_TIME
	clr SSR
	mov a, ON_TIME_COUNTER
	add a, #1H
	da a
	mov ON_TIME_COUNTER, a
	cjne a, ON_TIME, CONT
	mov ON_TIME_COUNTER, #0x00
	cpl SSR_ON_OFF_FLAG
	ljmp CONT
	
COUNT_OFF_TIME:
	setb SSR
	mov a, OFF_TIME_COUNTER
	add a, #1H
	da a
	mov OFF_TIME_COUNTER, a
	cjne a, OFF_TIME, CONT
	mov OFF_TIME_COUNTER, #0x00
	cpl SSR_ON_OFF_FLAG

CONT:
	mov a, sec
	add a, #1H
	da a
	mov sec, a
	cjne a, #60H, continuee
	
	mov sec, #00H
	mov a, min
	add a, #1H
	da a 
	mov min, a
	cjne a, #01H, continuee
	mov a, sec
	cjne a, #00H, continuee
	TEMPERATURE_CHECKER(#50H, #00H, x_lteq_y)	;if in the first 60s temp <= 50, abort reflow process
	jnb mf, continuee
	clr mf
	setb TERMINATION_ERROR_FLAG
	
continuee:
	jb PREHEAT_TIMEDONE_FLAG, continuee2
	jnb STAGE1_DONE_fLAG, jump_to_done
	mov a, PREHEAT_TIME
	add a, #1H
	da a
	mov PREHEAT_TIME, a
	cjne a, sec_soak, jump_to_done
	setb PREHEAT_TIMEDONE_FLAG
	ljmp jump_to_done
	
continuee2:
	jb REFLOW_TIMEDONE_FLAG, jump_to_done
	jnb STAGE3_DONE_FLAG, jump_to_done
	mov a, REFLOW_TIME
	add a, #1H
	da a
	mov REFLOW_TIME, a
	cjne a, sec_REFLOW, jump_to_done
	setb REFLOW_TIMEDONE_FLAG

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
	lcall Timer0_Init
	lcall Timer2_Init
	lcall clear_all
	Set_Cursor(1, 5)
	Send_Constant_String(#WELCOME_MEESAGE)
	mov SOAK_TEMP+1, #01H
	mov SOAK_TEMP+0, #30H
	mov REFLOW_TEMP+1, #02H
	mov REFLOW_TEMP+0, #17H
	mov sec_REFLOW, #30H
	mov sec_soak, #60H
	
	mov TO_SOAK_PWM, #100
	mov PREHEAT_PWM, #20
	mov TO_PEAK_PWM, #100
	mov REFLOW_PWM, #20
	mov COOLING_PWM, #0 ;default pwm values for reflow process
	
SETTINGS:

jb START_BUTTON, PB_SEC_SOAK
Wait_Milli_Seconds(#50)
jb START_BUTTON, PB_SEC_SOAK
jnb START_BUTTON, $
ljmp MainProgram

PB_SEC_SOAK:
PB_MACROO (SOAK_SEC_BUTTON, PB_SOAK_TEMP, sec_soak, #00H, #00H, #90H, DISPLAY_SOAK_PROFILE)
jnb mf, SETTINGS1
mov sec_soak, #60H
clr mf
ljmp SETTINGS
;0.pushbutton 1.jump to if not pushed 2. x+0 and parameter to increment 3.x+1 4. y+1 5. y+0 6. display
														   ;(checks if soak time is over bound. if it is, set it to 50H)
PB_SOAK_TEMP:
PB_MACROO (SOAK_TEMP_BUTTON, PB_SEC_REFLOW, SOAK_TEMP, SOAK_TEMP+1, #01H, #70H, DISPLAY_SOAK_PROFILE)
jnb mf, SETTINGS1
mov SOAK_TEMP+0, #30H
mov SOAK_TEMP+1, #01H
clr mf
ljmp SETTINGS

SETTINGS1:
	ljmp SETTINGS
	
PB_SEC_REFLOW:	
PB_MACROO (REFLOW_SEC_BUTTON, PB_REFLOW_TEMP, sec_REFLOW, #00H, #00H, #45H, DISPLAY_REFLOW_PROFILE)
jnb mf, SETTINGS1
mov sec_REFLOW, #30H
clr mf
ljmp SETTINGS

PB_REFLOW_TEMP:
PB_MACROO (REFLOW_TEMP_BUTTON, OVEN_SLOW_COOK, REFLOW_TEMP, REFLOW_TEMP+1, #02H, #35H, DISPLAY_REFLOW_PROFILE)
jnb mf, SETTINGS1
mov REFLOW_TEMP+0, #17H
mov REFLOW_TEMP+1, #02H
clr mf
ljmp SETTINGS

OVEN_SLOW_COOK:
	jb SLOW_COOK_BUTTON, OVEN_FAST_COOK
	Wait_Milli_Seconds(#50)
	jb SLOW_COOK_BUTTON, OVEN_FAST_COOK
	jnb SLOW_COOK_BUTTON, $
	mov TO_SOAK_PWM, #100
	mov PREHEAT_PWM, #20
	mov TO_PEAK_PWM, #100
	mov REFLOW_PWM, #20
	mov COOLING_PWM, #0 ;slow cook power control
	lcall beep_once
	ljmp SETTINGS

OVEN_FAST_COOK:
	jb FAST_COOK_BUTTON, SETTINGS2
	Wait_Milli_Seconds(#50)
	jb FAST_COOK_BUTTON, SETTINGS2
	jnb FAST_COOK_BUTTON, $
	mov TO_SOAK_PWM, #100
	mov PREHEAT_PWM, #20
	mov TO_PEAK_PWM, #100
	mov REFLOW_PWM, #20
	mov COOLING_PWM, #0 ;fast cook power control
	lcall beep_once 
SETTINGS2:
	ljmp SETTINGS
	
CONTINUE_PROGRAM_JUMPER:
	ljmp CONTINUE_PROGRAM
MainProgram:

	lcall InitSerialPort
	lcall INIT_SPI
	lcall CLEAR_LCD_SCREEN
	setb SSR_ON_OFF_FLAG
	setb EA
	clr TR2
	lcall beep_once
	setb TR2
	; SSR ON/OFF timings should be calculated here using the parameters that we have set from above
Forever:
	jb TERMINATION_ERROR_FLAG, ABORT_PROCESS
	jb STOP_BUTTON, CONTINUE_PROGRAM_JUMPER
	jnb STOP_BUTTON, $
	
ABORT_PROCESS:
	clr TR2
	setb SSR
	lcall CLEAR_LCD_SCREEN
	Set_Cursor(1, 1)
	Send_Constant_String(#PROGRAM_STOPPED)
	Set_Cursor(2, 1)
	Send_Constant_String(#PROGRAM_STOPPED2)
	lcall BEEP_2SEC
	lcall CLEAR_LCD_SCREEN
	Set_Cursor(1, 1)
	Send_Constant_String(#RESTART_MESSAGE)
	Set_Cursor(2, 1)
	Send_Constant_String(#RESTART_MESSAGE2)
	ljmp SETTINGS

CONTINUE_PROGRAM:	
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
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	sjmp Do_Something_With_Result

	
Do_Something_With_Result:
	mov x+3, #0
	mov x+2, #0
	mov x+1, Result+1
	mov x+0, Result+0              ;change these formulas
	load_y (4998510)
    lcall mul32
    load_y (1023*41*367)
    lcall div32
    load_y (22)
    lcall ADD32
	;Load_y(500)
	;lcall mul32
	;Load_y(1023)
	;lcall div32
	;Load_y (273)
	;lcall sub32
	lcall hex2bcd ; converts binary in x to BCD in bcd
	Send_BCD (bcd+1)
	Send_BCD (bcd)
	mov DPTR, #Hello_World
	lcall Sendstring
	;mov a, bcd+0
	sjmp STAGE1_RAMP_TO_SOAK
	
STAGE2_PREHEAT_JUMPER:
	ljmp STAGE2_PREHEAT
	
STAGE1_RAMP_TO_SOAK:
	TEMPERATURE_CHECKER (#50H, #00H, x_gteq_y)
	jnb mf, CONTINUE_STAGE_1
	clr mf
	clr HOT_SURFACE_INDICATOR
CONTINUE_STAGE_1:
	jb STAGE1_DONE_fLAG, STAGE2_PREHEAT_JUMPER ;will make a macro for the this one and the ones repeated
	;cjne a, #0x30, DONE1 ;change this so that it compares a current temp to the temp we want. Jumps to the appropriate SSR power on/off timing if not equal.
	TEMPERATURE_CHECKER (SOAK_TEMP+0, SOAK_TEMP+1, x_gteq_y)
	mov pwm, TO_SOAK_PWM
	DISPLAY_RUN_TEMP(#TO_SOAK_MEESAGE)
		Set_Cursor (2, 13)
	Display_BCD(SOAK_TEMP+1)
		Set_Cursor (2, 15)
	Display_BCD(SOAK_TEMP+0)
	jnb mf, forever_jumper   ;if mf is 0, meaning that the temperature has not reached soak temp, jump to the appropriate SSR power on/off timing
	lcall beep_once ;gets to here if mf is 1
	clr mf
	setb SSR_ON_OFF_FLAG
	mov ON_TIME_COUNTER, #0x00
	mov OFF_TIME_COUNTER, #0x00
	lcall CLEAR_LCD_SCREEN
	setb STAGE1_DONE_fLAG ;once this is set, it will stay on until STOP button is pressed
forever_jumper:
	ljmp forever

	
STATE3_RAMP_TO_PEAK_JUMPER:
	ljmp STATE3_RAMP_TO_PEAK
	
STAGE2_PREHEAT:
	jb STAGE2_DONE_FLAG, STATE3_RAMP_TO_PEAK_JUMPER
	;cjne a, #0x33, DONE1
	mov pwm, PREHEAT_PWM
	DISPLAY_RUN_TEMP(#PREHEAT_MESSAGE)
	Set_Cursor (2, 15)
	Display_BCD(PREHEAT_TIME)
	jnb PREHEAT_TIMEDONE_FLAG, forever_jumper1
	clr mf
	setb SSR_ON_OFF_FLAG
	mov ON_TIME_COUNTER, #0x00
	mov OFF_TIME_COUNTER, #0x00
	lcall beep_once
	lcall CLEAR_LCD_SCREEN
	setb STAGE2_DONE_FLAG
forever_jumper1:
	ljmp forever 

STAGE4_REFLOW_JUMPER:
	ljmp STAGE4_REFLOW
	
STATE3_RAMP_TO_PEAK:
	jb STAGE3_DONE_FLAG, STAGE4_REFLOW_JUMPER
	mov pwm, TO_PEAK_PWM
			Set_Cursor (2, 13)
	Display_BCD(REFLOW_TEMP+1)
		Set_Cursor (2, 15)
	Display_BCD(REFLOW_TEMP+0)
	TEMPERATURE_CHECKER (REFLOW_TEMP+0, REFLOW_TEMP+1, x_gteq_y)
	DISPLAY_RUN_TEMP(#TO_PEAK_MESSAGE)
	jnb mf, forever_jumper2
	clr mf
	setb SSR_ON_OFF_FLAG
	mov ON_TIME_COUNTER, #0x00
	mov OFF_TIME_COUNTER, #0x00
	lcall beep_once
	lcall CLEAR_LCD_SCREEN
	setb STAGE3_DONE_FLAG
forever_jumper2:
	ljmp forever
	
STAGE5_COOLING_JUMPER:
	ljmp STAGE5_COOLING

STAGE4_REFLOW:
	jb STAGE4_DONE_FLAG, STAGE5_COOLING_JUMPER
	mov pwm, REFLOW_PWM
	DISPLAY_RUN_TEMP(#REFLOW_MESSAGE)
	Set_Cursor (2, 15)
	Display_BCD(REFLOW_TIME)
	jnb REFLOW_TIMEDONE_FLAG, forever_jumper3
	clr mf
	setb SSR_ON_OFF_FLAG
	mov ON_TIME_COUNTER, #0x00
	mov OFF_TIME_COUNTER, #0x00
	lcall BEEP_2SEC
	lcall CLEAR_LCD_SCREEN
	setb STAGE4_DONE_FLAG
forever_jumper3:
	ljmp forever
	
STAGE5_COOLING:
	DISPLAY_RUN_TEMP(#COOLING_MESSAGE)
	jb STAGE5_DONE_FLAG, forever_jumper4
	mov pwm, COOLING_PWM
	TEMPERATURE_CHECKER (#30H, #0, x_lteq_y) ;if temp <= 30, beep six times
	jnb mf, forever_jumper4
	lcall beep_once
	lcall beep_once
	lcall beep_once
	lcall beep_once
	lcall beep_once
	lcall beep_once
	clr mf
	setb STAGE5_DONE_FLAG
forever_jumper4:
	ljmp forever

END