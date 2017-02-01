; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP52
$LIST

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

BOOT_BUTTON   equ P4.5
SOUND_OUT      equ P3.7
UPDOWN        equ P0.0
Pushsecond    equ p2.7
Pushminute    equ p2.6
Pushhour      equ p2.5
Pushalarm     equ P2.4
Pushtimer     equ P2.3
Pushampm      equ P2.2
SSR           equ p0.1


; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
  ; ljmp Timer0_ISR
  reti
    
    
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

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
hour:  ds 1
min:  ds 1
sec:  ds 1
alarm_hour: ds 1
alarm_min: ds 1
alarm_sec: ds 1
count: ds 1


; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
minutes_flag: dbit 1 ;
ampm_flag:  dbit 1 ;
alarm_flag:  dbit 1 ;
cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db '   :  :   ', 0
;Initial_Message1:  db '  :  :    ', 0
PM:               db 'PM', 0
AM:               db 'AM', 0
W:                db 'Wake', 0
U:                db 'Up!', 0
Empty3:           db '   ', 0
Empty4:           db '    ', 0
;WP:               db '    Wake Up!    ', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
;Timer0_Init:
;	mov a, TMOD
;	anl a, #0xf0 ; Clear the bits for timer 0
;	orl a, #0x01 ; Configure timer 0 as 16-timer
;	mov TMOD, a
;	mov TH0, #high(TIMER0_RELOAD)
;	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
 ;   setb ET0  ; Enable timer 0 interrupt
 ;   setb TR0  ; Start timer 0
;	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
;Timer0_ISR:
;	clr TF0  ; According to the data sheet this is done for us already.
	; In mode 1 we need to reload the timer.
;	clr TR0
;	mov TH0, #high(TIMER0_RELOAD)
;;	mov TL0, #low(TIMER0_RELOAD)
;	setb TR0
;	cpl SOUND_OUT ; Connect speaker to P3.7!
;	reti

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
	reti

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
   cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
   mov a, Count1ms+1
   cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know a second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	; Increment the CLOCK
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
	
	mov a, hour
	add a, #1
	da a
	mov hour, a
	cjne a, #0x12, CONT
	cpl ampm_flag 
	CONT:
	cjne a, #0x13, Timer2_ISR_done
	mov hour, #1
	
	
	 
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
    
	; Initialization
	clr SSR
    mov SP, #0x7F
    mov PMOD, #0 ; Configure all ports in bidirectional mode
    ;lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience ae few handy macros are included in 'LCD_4bit.inc':
    setb ampm_flag
    setb alarm_flag
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    Set_Cursor(2, 1)
    Send_Constant_String(#Initial_Message)
    setb seconds_flag
    setb minutes_flag
	mov hour, #0x12
	mov min, #0x59
	mov sec, #0x50
	mov alarm_hour, #0x01
	mov alarm_min, #0x00
	mov alarm_sec, #0x00
loop:
   jnb seconds_flag, loop ;if seconds_flag 0, goes to loop
   clr seconds_flag
    mov a, count
	add a, #1
	da a
	mov count, a
	cjne a, #0x5, turnovenon
	setb SSR
	mov count, #0
	sjmp loop_jump
	turnovenon:
	clr SSR
	;ljmp loop_jump	
	loop_jump:
	jb Pushminute, loop_checksecond
	jnb Pushminute, $	
	ljmp setminute
loop_checksecond:
    jb Pushsecond, loop_checkampm
	Wait_Milli_Seconds(#50)	
	jb Pushsecond, loop_checkampm
	jnb Pushsecond, $
	ljmp setsecond
loop_checkampm:
    jb Pushampm, loop_ab
	Wait_Milli_Seconds(#50)	
	jb Pushampm, loop_ab
	jnb Pushampm, $
	ljmp setampm
	
Press_timer:
    jb Pushtimer, loop_ab  
loop_checktimerhour:
    jb Pushhour, loop_checktminute  
	Wait_Milli_Seconds(#50)	
	jb Pushhour, loop_checktminute
	jnb Pushhour, $		
	ljmp settimerhour
loop_checktminute:
    jb Pushminute, loop_checktsecond 
	Wait_Milli_Seconds(#50)	
	jb Pushminute, loop_checktsecond
	jnb Pushminute, $	
	ljmp settimerminute
loop_checktsecond:
    jb Pushsecond, loop_checktampm
	Wait_Milli_Seconds(#50)	
	jb Pushsecond, loop_checktampm
	jnb Pushsecond, $
	ljmp settimersecond
loop_checktampm:
    jb Pushampm, loop_ab
	Wait_Milli_Seconds(#50)	
	jb Pushampm, loop_ab
	jnb Pushampm, $
	ljmp settimerampm
loop_ab:
;non_ring:
    ;clr SOUND_OUT
    ;jnb seconds_flag, loop ;if seconds_flag 0, goes to loop
	;clr seconds_flag

timer_AMPM:
    Set_Cursor(1, 11)
    jb ampm_flag, set_AM
	Send_Constant_String(#PM)
	ljmp alarm_AMPM
    set_AM:
    Send_Constant_String(#AM)

loop_a:    
hour_eq:
   mov a, hour
   cjne a, alarm_hour, CONTA
min_eq:
   mov a, min
    cjne a, alarm_min, CONTA
check_ampm:
    jb ampm_flag, check_flag
    jb alarm_flag, CONTA
    sjmp beep
check_flag:
    jnb alarm_flag, CONTA

beep:
    cpl P3.7
    Set_Cursor(2, 14)
    Send_Constant_String(#U)
    Set_Cursor(1, 13)
    Send_Constant_String(#W)
    ljmp CONTB
CONTA:
   ;jnb seconds_flag, loop_a ;if seconds_flag 0, goes to loop
   ;clr seconds_flag
   alarm_AMPM: 
    Set_Cursor(2, 11) 
    jb alarm_flag, set_alarmAM
    Send_Constant_String(#PM)
    ljmp loop_a
    set_alarmAM:
    Send_Constant_String(#AM)
    Set_Cursor(2, 2)
	Display_BCD(alarm_hour) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 5)
	Display_BCD(alarm_min) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 8)
	Display_BCD(alarm_sec) ; This macro is also in 'LCD_4bit.inc'	
	Set_Cursor(2, 14)
    Send_Constant_String(#Empty3)
    Set_Cursor(1, 13)
    Send_Constant_String(#Empty4)
CONTB:
   	Set_Cursor(1, 2)
	Display_BCD(hour) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 5)
	Display_BCD(min) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 8)
	Display_BCD(sec) ; This macro is also in 'LCD_4bit.inc'

	ljmp loop

sethour:
    clr a
    mov a, alarm_hour
    add a, #1
    da a
    mov alarm_hour, a
    cjne a, #0x12, cont_loop
    cpl alarm_flag
cont_loop:
    cjne a, #0x13, loop_conth
    mov  a, #1
    mov alarm_hour, a
loop_conth:
    clr a
    ljmp loop_a
setminute:
    clr a
    mov a, alarm_min
    add a, #1
    da a
    mov alarm_min, a
    cjne a, #0x60, loop_contm
    mov  a, #0
    mov alarm_min, a
loop_contm:
    clr a
    ljmp loop_a
setsecond:
    clr a
    mov a, alarm_sec
    add a, #1
    da a
    mov alarm_sec, a
    cjne a, #0x60, loop_conts
    mov  a, #0
    mov alarm_sec, a
loop_conts:
    clr a
    ljmp loop_a
    
settimerhour:
    clr a
    mov a, hour
    add a, #1
    da a
    mov hour, a
    cjne a, #0x12, cont_loop1
    cpl ampm_flag
cont_loop1:
    cjne a, #0x13, loop_conth1
    mov  a, #1
    mov hour, a
loop_conth1:
    clr a
    ljmp loop_a
settimerminute:
    clr a
    mov a, min
    add a, #1
    da a
    mov min, a
    cjne a, #0x60, loop_contm1
    mov  a, #0
    mov min, a
loop_contm1:
    clr a
    ljmp loop_a
settimersecond:
    clr a
    mov a, sec
    add a, #1
    da a
    mov sec, a
    cjne a, #0x60, loop_conts1
    mov  a, #0
    mov sec, a
loop_conts1:
    clr a
    ljmp loop_a
setampm:
    cpl alarm_flag
    ljmp loop_a
settimerampm:
    cpl ampm_flag
    ljmp loop_a	
stop:
    cpl P3.7
    ljmp loop_a
    

Turnovenoff:
    setb SSR
END