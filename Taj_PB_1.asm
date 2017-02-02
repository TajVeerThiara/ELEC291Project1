
$NOLIST
$MODLP52
$LIST

;FROM LAB 3
CLK  EQU 22118400
BAUD equ 115200
T1LOAD equ (0x100-(CLK/(16*BAUD)))
CE_ADC EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3
TEMP_B EQU p2.4

;FROM LAB 2
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))


BOOT_BUTTON   equ P4.5
SOUND_OUT EQU P3.7
;UPDOWN        equ P0.0


; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
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


dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
Result:     ds  2
x:   ds 4
y:   ds 4
bcd: ds 5
temp: ds 1
hour:  ds 1
min:  ds 1
sec:  ds 1
min_soak: ds 1
sec_soak: ds 1
min_reflow: ds 1
sec_REFLOW: ds 1
Soak_Temp: ds 1
Reflow_Temp: ds 1

Beep1_min: ds 1
Beep1_sec: ds 1


BSEG
second_flag: dbit 1 ; Set to one in the ISR 
sound_flag: dbit 1
mf: dbit 1

CSEG
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5

;PARAMETERS ADDED FOR PROJECT 1(9)
SOAK_BUTTON EQU P0.0
SOAK_TEMP_BUTTON EQU P0.1
SOAK_MIN EQU P0.3
SOAK_SEC EQU P0.4
REFLOW_BUTTON EQU P1.0
REFLOW_TEMP_BUTTON EQU P0.5
REFLOW_MIN EQU P0.6
REFLOW_SEC EQU P0.7
START EQU P0.8
STOP EQU P0.7
RESET EQU P0.8 


$NOLIST
$include(math32.inc)
$include(LCD_4Bit.inc)
$LIST


;                     1234567890123456   
SoakTime:  db '  :  :     ', 0
Empty:	DB	'\r\n',0
;DON'T NEED INITIAL MESSAGE FOR TEMPERATURE
;OR STATE TRANSTIONS 

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
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
	cpl SOUND_OUT ; Connect speaker to P3.7!
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
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb second_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the clock
	
	mov a, sec
	add a, #1
	da a
	mov sec, a
	cjne a, #0x60, Timer2_ISR_done
	mov sec, #0
	 
	mov a, min
	add a, #1
	da a
	mov min, a
	cjne a, #0x60, Timer2_ISR_done
	mov min, #0
	
	lcall Eh
	
	Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
	Eh:
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

WaitHalfSec:
    mov R2, #89
WH3: mov R1, #250
WH2: mov R0, #166
WH1: djnz R0, WH1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, WH2 ; 22.51519us*250=5.629ms
    djnz R2, WH3 ; 5.629ms*89=0.5s (approximately)
    ret

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
			

	


 
	
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:

	; Initialization
    mov SP, #0x7F
    mov PMOD, #0 ; Configure all ports in bidirectional mode
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    
    MOV sound_flag, #0x0
    setb second_flag
	mov min, #0x00
	mov sec, #0x00
	mov min_soak, #0x00
	mov sec_soak, #0x00
	
; FOR SOAK TIME
PUSHBUTTON_SOAK:

 	setb TR2
    clr second_flag
	
	
jnb SOAK_BUTTON, PB_SOAK_TIME
ljmp PB_SOAK_TEMP


;Now check soak_time should be
PB_SOAK_TIME:
jb SOAK_MIN,PB_SEC_SOAK
Wait_Milli_Seconds(#50) 
jb SOAK_MIN,PB_SEC_SOAK
jnb SOAK_MIN, $ 
	
	mov a, min_soak
	add a, #0x01
	da a
	mov min_soak, a
	cjne a, #0x61,PB_SEC_SOAK
	mov min_soak, #0x01
	ljmp Finish
	
	
PB_SEC_SOAK:	
jb SOAK_SEC,PB_SOAK_TEMP
Wait_Milli_Seconds(#50) 
jb SOAK_SEC,PB_SOAK_TEMP  
jnb SOAK_SEC, $	

	mov a, sec_soak
	add a, #0x01
	da a
	mov sec_soak, a
	cjne a, #0x61,PB_SOAK_TEMP
	mov sec_soak, #0x01
	ljmp Finish	
	
	
PB_SOAK_TEMP:
jb SOAK_TEMP_BUTTON,PB_REFLOW_TIME
Wait_Milli_Seconds(#50) 
jb SOAK_TEMP_BUTTON,PB_REFLOW_TIME
jnb SOAK_TEMP_BUTTON, $ 
	
	mov a, SOAK_TEMP
	add a, #0x01
	da a
	mov SOAK_TEMP, a
	cjne a, #0x99,PB_REFLOW_TIME
	mov SOAK_TEMP, #0x01
	ljmp Finish
	
PB_REFLOW_TIME:

jb REFLOW_MIN,PB_SEC_REFLOW
Wait_Milli_Seconds(#50) 
jb REFLOW_MIN,PB_SEC_REFLOW
jnb REFLOW_MIN, $ 
	
	mov a, min_REFLOW
	add a, #0x01
	da a
	mov min_REFLOW, a
	cjne a, #0x61,PB_SEC_REFLOW
	mov min_REFLOW, #0x01
	ljmp Finish
	
	
PB_SEC_REFLOW:	
jb REFLOW_SEC,PB_REFLOW_TEMP
Wait_Milli_Seconds(#50) 
jb REFLOW_SEC,PB_REFLOW_TEMP  
jnb REFLOW_SEC, $	

	mov a, sec_REFLOW
	add a, #0x01
	da a
	mov sec_REFLOW, a
	cjne a, #0x61,PB_REFLOW_TEMP
	mov sec_REFLOW, #0x01
	ljmp Finish	

PB_REFLOW_TEMP:
jb REFLOW_TEMP_BUTTON,loop
Wait_Milli_Seconds(#50) 
jb REFLOW_TEMP_BUTTON,loop
jnb REFLOW_TEMP_BUTTON, $ 
	
	mov a, REFLOW_TEMP
	add a, #0x01
	da a
	mov REFLOW_TEMP, a
	cjne a, #0x99,loop
	mov REFLOW_TEMP, #0x01
	ljmp Finish
	  
loop:	
	jnb second_flag, Finish ;0, goes to loop
	
Finish:	
	Set_Cursor(1, 1)
	Display_BCD(min)
	Set_Cursor(1, 4)
	Display_BCD(sec) 
	
	Set_Cursor(2, 1)
	Display_BCD(min_soak)
	Set_Cursor(2, 4)
	Display_BCD(sec_soak)

	mov a, min
	mov R1, Beep1_min
	subb a, R1
	jnz no_sound
	
	mov a, sec
	mov R1, Beep1_sec
	subb a, R1
	jnz no_sound
	
	setb sound_flag
	sjmp Silent
no_sound: 
	clr sound_flag
Silent:
    ;;;;;Main Program (Lab 3)
    lcall InitSerialPort
  
    lcall INIT_SPI
forever:    
    clr CE_ADC
    mov R0, #00000001B
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
    
    lcall WaitHalfSec
    
    mov x+3, #0
    mov x+2, #0
    mov x+1, Result+1
    mov x+0, Result
    load_y (497)
    lcall mul32
    load_y (1023)
    lcall div32
    
    ; for displaying temperature ( C)
    load_y(273)
    lcall sub32
    
    lcall hex2bcd
    
    Send_BCD(bcd+1)
    ;mov a, #'.'
    ;lcall putchar
    Send_BCD(bcd)
    
    mov DPTR, #Empty
    lcall SendString
    ;sjmp forever

	

	lcall PUSHBUTTON_SOAK
	
END
