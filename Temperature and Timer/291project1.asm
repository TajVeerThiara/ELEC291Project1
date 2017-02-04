$MODLP52

CLK  EQU 22118400
BAUD equ 115200
T1LOAD equ (0x100-(CLK/(16*BAUD)))
;timer constants
TIMER0_RATE EQU 4096
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

;timer pins
BOOT_BUTTON   equ P4.5
SOUND_OUT     equ P3.7
UPDOWN        equ P0.0


;SPI pins
CE_ADC  EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3

; Reset vector
org 0x0000
    ljmp MainProgram

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

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
sec: 		  ds 1 ;ds reserves a specific number of bits where the label is assigned to memory address
Result:       ds 2
x:   	      ds 4
y:   		  ds 4
bcd: 		  ds 5
; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
mf: 	  	dbit 1

CSEG
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5

$NOLIST
$include(math32.inc)
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Temperature', 0

;-----------------------------;
;Interrupts Timers	      ;
;-----------------------------;
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
;
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
	; Checks if count1ms is 1000ms = 1sec
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	clr TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed

	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, sec ; seconds
	;jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x1
	da a
	mov sec, a	
	
;Timer2_ISR_decrement:
	;add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
	;.....
	
Timer2_ISR_done:
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
;----------------------------------------;
;Reads from ADC and updates temperature  ;
;----------------------------------------;    
Update_Temp:
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
  ;  lcall WaitHalfSec
    Wait_Milli_Seconds(#250)

    mov x+3, #0 ;convert from voltage to temperature and from binary to ASCII
    mov x+2, #0
    mov x+1, Result+1
    mov x+0, Result+0
    load_y (5000000)
    lcall mul32
    load_y (1023*41*303)
    lcall div32
    load_y (22)
    lcall add32    
    lcall hex2bcd
    Send_BCD(bcd+1)
    mov a, #'.'
    lcall putchar
    Send_BCD(bcd)

    mov DPTR, #newline
    lcall SendString
    ret
    
newline:
	DB	'\r\n',0


;-------------------;
;Main Program       ;
;-------------------;
MainProgram:
    mov SP, #7FH ; Set the stack pointer to the begining of idata
    mov PMOD, #0 ; Configure all ports in bidirectional mode

;----------------------------;
;Initialization of timers    ;
;----------------------------;
Timerset:
    lcall InitSerialPort
    lcall INIT_SPI
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    clr TR0
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    mov sec, #0x00
forever:
    Set_Cursor(1, 13)  ; the place in the LCD where we want the BCD counter value
    Display_BCD(bcd+1) ; upper byte of bcd
    Display_BCD(bcd)
    Set_Cursor(2, 13)
    Display_BCD(sec)
    lcall Update_Temp
    ljmp forever
    
END
