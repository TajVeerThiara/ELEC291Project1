$NOLIST
$MODLP52
$LIST

org 0000H
    ljmp myprogram
    
    
;PINS
KEY EQU P2.0
LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5

DSEG at 30H

pwm: ds 2
state:ds 2
sec: ds 2
temp: ds 2
temp_soak: ds 2 
time_soak: ds 2
Temp_refl: ds 1
Time_refl: ds 1
bcd: ds 2
x: ds 2
y: ds 2

BSEG
mf: dbit 1

CSEG

$include(math32.inc)
$include(LCD_4bit.inc)

myprogram:     
mov a, state

state0:
cjne a, #0, state1
mov pwm, #0
jb KEY, state0_done
jnb KEY, $ ; Wait for key release to start reflow process
mov state, #1

state0_done:
ljmp forever

state1:
cjne a, #1, state2
mov pwm, #100
mov sec, #0
mov a, temp_soak ;150
clr c			;clearing carry flag
subb a, temp 
jnc state1_done
mov state, #2

state1_done:
ljmp forever

state2:
cjne a, #2, state3
mov pwm, #20
mov a, time_soak ;60
clr c
subb a, sec
jnc state2_done
mov state, #3

state2_done:
ljmp forever

state3:
cjne a, #2, state3
mov pwm, #100
mov sec, #0
mov a, temp_soak ;220
clr c
subb a, temp
jnc state3_done
mov state, #4
state3_done:
ljmp forever

state4:
cjne a, #2, state3
mov pwm, #20
mov a, time_soak ;45
clr c
subb a, sec
jnc state4_done
mov state, #5

state4_done:
ljmp forever

state5:
cjne a, #2, state3
mov pwm, #0
mov a, temp_soak ;60
clr c
subb a, temp
jnc state5_done
mov state, #0

state5_done:
ljmp forever


abort:
mov pwm, #0
ljmp forever


forever: 
		sjmp forever

;ISR for reset from other labs

;Abort function
;if temp does not reach x by y seconds

;Data flash memory: To store config of the reflow controller
; - available after reset then





END