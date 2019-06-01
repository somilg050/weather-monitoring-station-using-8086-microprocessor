#LOAD_SEGMENT=0100h#
#LOAD_OFFSET=0000h#

#CS=0100h#
#IP=0000h#

#DS=0000h#
#ES=0000h#

#SS=0000h#
#SP=FFFEh#

#AX=0000h#
#BX=0000h#
#CX=0000h#
#DX=0000h#
#SI=0000h#
#DI=0000h#
#BP=0000h#          



;-------------------------IVT-init-----------------------;
mov ax, offset isr0
mov [00200h], ax
mov ax, seg isr0
mov [00202h], ax

mov ax, offset isr1
mov [00204h], ax
mov ax, seg isr1
mov [00206h], ax

mov ax, offset isr2
mov [00208h], ax
mov ax, seg isr2
mov [0020Ah], ax

mov ax, offset isr3
mov [0020Ch], ax
mov ax, seg isr3
mov [0020Eh], ax

;-------------------------IVT-init-----------------------;
jmp start
db 512 dup(0)


;-------------------------Data-segment-----------------------;
cstatea db 00h
cstateb db 00h
lcdln1 db 'Temp(C): ' 
lcdln2 db 16 dup('-')
lcdln3 db 16 dup('.')
lcdln4 db 16 dup('*')
lcdcnt1 db 9d
lcdcnt2 db 16d
lcdcnt3 db 16d
lcdcnt4 db 16d



flagcount	dw 0
vals 		db 12 dup(0)
ctr 		dw 0
readyForHour db 1 dup(0)

thp db 1 dup(0)


lcdln11 db 'Humi1(%): ' 
lcdln22 db 16 dup('-')
lcdln33 db 16 dup('.')
lcdln44 db 16 dup('*')
lcdcnt11 db 9d
lcdcnt22 db 16d
lcdcnt33 db 16d
lcdcnt44 db 16d



;Humi~
flagcount11	dw 0
vals11 		db 12 dup(0)
ctr11 		dw 0



lcdln111 db 'Pres(Ba):' 
lcdln222 db 16 dup('-')
lcdln333 db 16 dup('.')
lcdln444 db 16 dup('*')
lcdcnt111 db 9d
lcdcnt222 db 16d
lcdcnt333 db 16d
lcdcnt444 db 16d


;Pres
flagcount111	dw 0
vals111		db 12 dup(0)
ctr111		dw 0


numstr db 16 dup(0)

q db 0
r db 0



divby dw 12d
updatenow db 00h


;------------------------start-inits-----------------------; 

start: cli

a8259 equ 4000h
a8255 equ 4010h
b8255 equ 4020h
a8253 equ 4030h
b8253 equ 4040h


8259_init:

;icw1
mov al, 00010011b   ;ICW4 Needed (single 8259)
mov dx, a8259+00h   ; dx has 1st address of 8259
out dx, al

;icw2
mov al, 10000000b   ; dx has 2nd address of 8259
mov dx, a8259+02h   ; 80h is generatedfor IR0 ie But-INT
out dx,al

;icw4
mov al, 00000011b       ; rest follow 80h - 87h
out dx,al
 
;ocw1
mov al, 11111110b   ; non buffered mode with AEOI enabled
out dx, al  

; Initialising 8255A ...

8255_init:
mov al, 10000010b       ;Cmnd Word - port a(o/p), !(prt B: i/p)
mov dx, a8255+06h
out dx, al

; !(Same as prev 8255) B - i/p C - for controlling ADC

mov al, 10000010b
mov dx, b8255+06h
out dx, al  

8253_init:              ; counter0 - sq. wave - binary i/p(2MHz-i/p)
;1Mhz
mov al, 00010110b
mov dx, a8253+06h
out dx, al
mov al, 02h
mov dx, a8253+00h       ; To divide by 2 - to give 1MHz
out dx, al


;16hz
mov al, 01110110b       ;counter1 - sq. wave - binary i/p
mov dx, a8253+06h
out dx, al
mov al, 24h              ; count = 62500 = 0f424h
mov dx, a8253+02h
out dx, al
mov al, 0F4h
out dx, al

;5min
mov al, 10110100b       ;cntr3 - Using mode 2 - every 5 min(low)
mov dx, a8253+06h       ;Must be inverted and given as interrupt
out dx, al
mov al, 0C0h
mov dx, a8253+04h
out dx, al
mov al, 12h
out dx, al

;1hr
mov al, 00110100b       ;counter 0 - 16Hz to 1Hr pulse (obsolete)
mov dx, b8253+06h
out dx, al
mov al, 00h
mov dx, b8253+00h
out dx, al
mov al, 0E1h
out dx, al  


LCD_init:
lcden equ 80h
lcdrw equ 40h
lcdrs equ 20h

aclrb lcdrw
lcd_out 38h
lcd_out 0Eh
lcd_out 06h

lcd_clear


LCD_init1:
lcden1 equ 10h
lcdrw equ 40h
lcdrs equ 20h

aclrb lcdrw
lcd_out1 38h
lcd_out1 0Eh
lcd_out1 06h

lcd_clear



LCD_init11:
lcden11 equ 08h
lcdrw equ 40h
lcdrs equ 20h

aclrb lcdrw
lcd_out11 38h
lcd_out11 0Eh
lcd_out11 06h

lcd_clear
 
;------------------------start-code-------------------------;

; Perform Initial display ...

;turn on adc - temperature
mov thp,00h
int 81h     
        
;wait for eoc

eocint08:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint08

eocint18:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint18

;Store Values - Temperature

mov thp,00h
int 83h

;Repeat process for Humi~ty

mov thp,01h
int 81h     ; do same for humi~

;wait for eoc
eocint010:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint010

eocint101:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint101


mov thp,01h
int 83h

; FOR pressure

mov thp,11h
int 81h     ; do same for humi

;wait for eoc
eocint0109:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint0109

eocint1018:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint1018


mov thp,11h
int 83h



mov thp,00h
int 82h
mov thp,01h
int 82h
mov thp,11h
int 82h

; --------END of initial display ---------------


;poll portb of a8255 forever
xinf:

;check if button is pressed using a flag stored in memory
mov al, updatenow
cmp al, 01h
jnz cont
mov updatenow, 00h

mov thp,00h         ;FOR TEMPERATURE
int 81h ;turn on adc

;wait for eoc
eocint0:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint0

eocint1:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint1


mov thp,00h
int 83h

mov thp,01h
int 81h     ; do same for humi

;wait for eoc
eocint00:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint00

eocint11:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint11


mov thp,01h
int 83h



mov thp,11h ; FOR PRESSURE
int 81h



;wait for eoc
eocint000:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint000

eocint111:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint111

mov thp,11h
int 83h





mov thp,00h
int 82h
mov thp,01h
int 82h
mov thp,11h
int 82h



;regular polling
cont:
mov dx, a8255+02h
in al, dx

mov bl, al
and bl, 01h
jz butint

mov bl, al
and bl, 02h
jz fivemin

mov bl, al
and bl, 04h
jz onehr

mov bl, al
and bl, 08h
jz eocint
jmp xinf

;low logic detected. Wait for whole pulse
butint:
in al, dx
and al, 01h
jz butint

int 80h
jmp xinf

fivemin:
in al, dx
and al, 02h
jz fivemin

mov thp,00h
int 81h

eocint09:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint09

eocint19:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint19

mov thp,00h
int 83h


mov thp,01h
int 81h


;wait for eoc
eocint009:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint009


eocint119:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint119

mov thp,01h
int 83h


mov thp,11h
int 81h



;wait for eoc
eocint00999:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jnz eocint00999


eocint11999:
mov dx, a8255+02h
in al, dx
mov bl, al
and bl, 08h
jz eocint11999

mov thp,11h
int 83h


;Change value of no. of 5 min intervals taken during simulation
 
inc readyForHour

cmp readyForHour,02h
jnz donotcallonehour

mov thp,00h
int 82h ; Call the 1 hour interrupt
mov thp,01h
int 82h
mov thp,11h
int 82h

mov readyForHour,00h ; Reset the 12, 5-min interval count


donotcallonehour: jmp xinf


; Obsolete. Is not use as intrpt is called directly

onehr:
in al, dx
and al, 04h
jz onehr

int 82h
jmp xinf

eocint:
in al, dx
and al, 08h
jz eocint

mov thp,00h
int 83h
mov thp,01h
int 83h
mov thp,11h
int 83h


jmp xinf




jmp quit



;------------------------start-macros-----------------------;
pushall macro
    push ax
    push bx
    push cx
    push dx
    push si
    push di
endm

popall macro
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
endm

;set/clear pins since BSR was being strange
asetb    macro mbit
    pushall
    mov al, mbit
    mov bl, cstatea
    or  al, bl
    mov dx, a8255+04h
    out dx, al
    mov bl, al
    mov cstatea, bl
    popall            
endm


aclrb    macro mbit
    pushall
    mov al, mbit
    xor al, 0FFh
    mov bl, cstatea
    and al, bl
    mov dx, a8255+04h
    out dx, al
    mov bl, al
    mov cstatea, bl
    popall             
endm

adcst equ 01h
adcoe equ 02h
adcA equ 04h
adcB equ 08h
adcC equ 10h
adcALE equ 20h

bsetb    macro mbit
    pushall
    mov al, mbit
    mov bl, cstateb
    or  al, bl
    mov dx, b8255+04h
    out dx, al
    mov bl, al
    mov cstateb, bl
    popall            
endm


bclrb    macro mbit
    pushall
    mov al, mbit
    xor al, 0FFh
    mov bl, cstateb
    and al, bl
    mov dx, b8255+04h
    out dx, al
    mov bl, al
    mov cstateb, bl
    popall             
endm

lcd_out    macro dat
    aclrb lcdrs
    pushall
    mov al, dat
    mov dx, a8255+00h
    out dx, al
    asetb lcden
    aclrb lcden
    call delay_20ms
    popall
endm


lcd_out1    macro dat
    aclrb lcdrs
    pushall
    mov al, dat
    mov dx, a8255+00h
    out dx, al
    asetb lcden1
    aclrb lcden1
    call delay_20ms
    popall
endm

lcd_out11    macro dat
    aclrb lcdrs
    pushall
    mov al, dat
    mov dx, a8255+00h
    out dx, al
    asetb lcden11
    aclrb lcden11
    call delay_20ms
    popall
endm


lcd_write_char macro dat
    asetb lcdrs
    pushall
    mov al, dat
    mov dx, a8255+00h
    out dx, al
    asetb lcden
    aclrb lcden
    ;call delay_20ms
    popall    

endm

         
lcd_write_char1 macro dat
    asetb lcdrs
    pushall
    mov al, dat
    mov dx, a8255+00h
    out dx, al
    asetb lcden1
    aclrb lcden1
    ;call delay_20ms
    popall    

endm

lcd_write_char11 macro dat
    asetb lcdrs
    pushall
    mov al, dat
    mov dx, a8255+00h
    out dx, al
    asetb lcden11
    aclrb lcden11
    ;call delay_20ms
    popall    

endm

lcd_clear macro
    lcd_out 01h
endm
 
;division routine since div was acting strange
wow_divide macro  divi
    
    pushall
    mov cx, 00
    mov bx, divi
    
    loopy:
    sub ax, bx
	inc cx
    cmp ax, 0
    jge loopy

	dec cx
	add ax, bx
	
	mov r, al
	mov q, cl

	popall
endm

          
wow_divide1 macro  divi
    
    pushall
    mov cx, 00
    mov bx, divi
    
    loopy1:
    sub ax, bx
	inc cx
    cmp ax, 0
    jge loopy1

	dec cx
	add ax, bx
	
	mov r, al
	mov q, cl

	popall
endm

wow_divide2 macro  divi
    
    pushall
    mov cx, 00
    mov bx, divi
    
    loopy2:
    sub ax, bx
	inc cx
    cmp ax, 0
    jge loopy2

	dec cx
	add ax, bx
	
	mov r, al
	mov q, cl

	popall
endm

;------------------------Start-Procedure Defs-----------------------;

;delay proc: just a huge loop
delay_20ms proc near
    mov dx, 10
r1: mov cx, 2353
r2: loop r2
    dec dx
    jne r1
    ret     
delay_20ms endp


;write a string in memory to LCD
write_string proc near   
    lea si, lcdln1
    mov cl, lcdcnt1
l1: lcd_write_char [si]
    inc si
    loop l1
    ret
write_string endp


;write a string in memory to LCD
write_string1 proc near   
    lea si, lcdln11
    mov cl, lcdcnt11
l11: lcd_write_char1 [si]
    inc si
    loop l11
    ret
write_string1 endp


;write a string in memory to LCD
write_string11 proc near   
    lea si, lcdln111
    mov cl, lcdcnt111
l111: lcd_write_char11 [si]
    inc si
    loop l111
    ret
write_string11 endp


;-----------------------------------------------
;Scale Humidity 

convert_humi proc near
    
    ;get it to scale (0-99%)
    mov ah, 00h
    mov al, q
    mov bl, 99d            
    mul bl
    mov bl, 0FFh            ;FFh is the max o/p from ADC
    div bl
	
    
    ;split the numbers                        
    mov ah, 00h
    mov bl, 10d
    div bl
    
    lea si, numstr          ;Load appropriate ascii value(quo)
    add ax, 3030h
    mov [si], al
    mov [si+1], ah 
    
    mov al, r
    mov ah, 00h
    
    mov bx, 100d
    mul bx
    mov bl, 12d
    div bl
    
    mov ah, 00h
    mov bl, 10d
    div bl
    add ax, 3030h
    
    mov [si+2], al      ;Load appropriate ascii value(rem)
    mov [si+3], ah
   
    ret

convert_humi endp

; Scaling fns ----------------------------------------------

;Scale temperature 

convert_temp proc near
    
    ;get it to scale (5-50 C)
    mov ah, 00h
    mov al, q
    mov bl, 45d
    mul bl
    mov bl, 0FFh
    div bl
	add ax, 05h
    
    ;split the numbers                     
    mov ah, 00h
    mov bl, 10d
    div bl
    
    lea si, numstr
    add ax, 3030h
    mov [si], al
    mov [si+1], ah 
    
    mov al, r
    mov ah, 00h
    
    mov bx, 100d
    mul bx
    mov bl, 12d
    div bl
    
    mov ah, 00h
    mov bl, 10d
    div bl
    add ax, 3030h
    
    mov [si+2], al
    mov [si+3], ah
   
    ret

convert_temp endp


;--------------------------------------------------------------------------
;Scale Pressure  

convert_pres proc near
    
    ;get it to scale (0-2 Bar)
    mov ah, 00h
    mov al, q
    mov bl, 02d            
    mul bl
    mov bl, 0FFh            ;FFh is the max o/p from ADC
    div bl
	
    
    ;split the numbers                        
    mov ah, 00h
    mov bl, 10d
    div bl
    
    lea si, numstr          ;Load appropriate ascii value(quo)
    add ax, 3030h
    mov [si], al
    mov [si+1], ah 
    
    mov al, r
    mov ah, 00h
    
    mov bx, 100d
    mul bx
    mov bl, 12d
    div bl
    
    mov ah, 00h
    mov bl, 10d
    div bl
    add ax, 3030h
    
    mov [si+2], al      ;Load appropriate ascii value(rem)
    mov [si+3], ah
   
    ret

convert_pres endp


;output ascii equiv values on LCD from mem location

num_out proc near
    
    lcd_out 01h
    call write_string
    
    mov al, numstr
    mov ah, numstr+1
    lcd_write_char al
    lcd_write_char ah
    lcd_write_char '.'
    mov al, numstr+2
    mov ah, numstr+3
    
    lcd_write_char al
    lcd_write_char ah
   
    
    ret     
    
num_out endp


;output ascii equiv values on LCD from mem location
num_out1 proc near
    
    lcd_out1 01h
    call write_string1
    
    mov al, numstr
    mov ah, numstr+1
    lcd_write_char1 al
    lcd_write_char1 ah
    lcd_write_char1 '.'
    mov al, numstr+2
    mov ah, numstr+3
    
    lcd_write_char1 al
    lcd_write_char1 ah
   
    
    ret     
    
num_out1 endp

num_out11 proc near
    
    lcd_out11 01h
    call write_string11
    
    mov al, numstr
    mov ah, numstr+1
    lcd_write_char11 al
    lcd_write_char11 ah
    lcd_write_char11 '.'
    mov al, numstr+2
    mov ah, numstr+3
    
    lcd_write_char11 al
    lcd_write_char11 ah
   
    
    ret     
    
num_out11 endp

; ------------------------- End of procedure Defs --------------------------

;------------------------Start Of ISRs-----------------------;

;5 minute interrupt
isr1:

    
	; First make OE high PC1
	bsetb adcoe


    cmp thp,00h
    jnz humiisr1

	;Assuming that CBA is connected to PC 4-3-2
	;select channel 000
	bclrb adcA
	bclrb adcB
	bclrb adcC

	;Now make a high-low pulse on ALE;PC5
	bsetb adcALE
	bclrb adcALE

	;High-low pulse on SOC - connected to PC0
	bsetb adcst
	bclrb adcst

	;now wait for EOC interrupt
    jmp isr1end

    humiisr1:           ; thp == 1
    cmp thp,01h
    jnz presisr1

    ;select channel 001
	bsetb adcA
	bclrb adcB
	bclrb adcC

	;Now make a high-low pulse on ALE;PC5
	bsetb adcALE
	bclrb adcALE

	;High-low pulse on SOC - connected to PC0
	bsetb adcst
	bclrb adcst

    jmp isr1end

    presisr1:

    ;select channel 010
	bclrb adcA
	bsetb adcB
	bclrb adcC

	;Now make a high-low pulse on ALE;PC5
	bsetb adcALE
	bclrb adcALE

	;High-low pulse on SOC - connected to PC0
	bsetb adcst
	bclrb adcst

	

isr1end:
    
iret


;EOC interrupt
isr3:


    cmp thp,00h
    jnz humiisr3


	mov dx, b8255+02h
	in al, dx

	;Finally make OE low
	bclrb adcoe


	cmp flagcount, 0
	jnz x4
	;for the first hour, flagcnt = 0; for consecutive iterations, it'll be >0

	mov bx, ctr
	lea si, vals 
	  
	mov [si+bx], al
	inc bx
	mov ctr, bx
	cmp bx, 12
	jnz x5

	mov flagcount, 1
	mov ctr, 0
	jmp endisr1

	x4:	mov bx, ctr
	lea si, vals
	mov [si+bx], al
	inc bx
	cmp bx, 12
	jnz x5
	mov bx, 0

	x5:	mov ctr, bx

    jmp endisr1

    humiisr3:       ;thp == 1
    cmp thp,01h
    jnz presisr3

    
	mov dx, b8255+02h
	in al, dx

	;Finally make OE low
	bclrb adcoe


	cmp flagcount11, 0
	jnz x41

	mov bx, ctr11
	lea si, vals11 
	  
	mov [si+bx], al
	inc bx
	mov ctr11, bx
	cmp bx, 12
	jnz x51

	mov flagcount11, 1
	mov ctr11, 0
	jmp endisr1

	x41:	mov bx, ctr11
	lea si, vals11
	mov [si+bx], al
	inc bx
	cmp bx, 12
	jnz x51
	mov bx, 0

	x51:	mov ctr11, bx
    jmp endisr1

    presisr3:

    
	mov dx, b8255+02h
	in al, dx

	;Finally make OE low
	bclrb adcoe


	cmp flagcount111, 0
	jnz x411
	

	mov bx, ctr111
	lea si, vals111 
	  
	mov [si+bx], al
	inc bx
	mov ctr111, bx
	cmp bx, 12
	jnz x511

	mov flagcount111, 1
	mov ctr111, 0
	jmp endisr1

	x411:	mov bx, ctr111
	lea si, vals111
	mov [si+bx], al
	inc bx
	cmp bx, 12
	jnz x511
	mov bx, 0

	x511:	mov ctr111, bx

    
	 
	endisr1:
iret
      
      
      
;1hr int 
isr2:

cmp thp,00h
jnz humiisr2

	mov bx, 00h
	mov cx, 12d
	lea si, vals

	xadd:
	mov dl, [si]
	mov dh, 00h
	add bx, dx
	inc si
	dec cx
	jnz xadd
	mov ax, bx

	mov dx, flagcount
	cmp dx, 1
	jnz x2

	mov divby, 12d
	jmp x3

	x2:
	mov dx, ctr
	mov divby, dx
	x3:

	wow_divide divby 

	call convert_temp
	call num_out

jmp endisr2:

    humiisr2:
    cmp thp,01h
    jnz presisr2

	mov bx, 00h
	mov cx, 12d
	lea si, vals11

	xadd1:
	mov dl, [si]
	mov dh, 00h
	add bx, dx
	inc si
	dec cx
	jnz xadd1
	mov ax, bx

	mov dx, flagcount11
	cmp dx, 1
	jnz x21

	mov divby, 12d
	jmp x31

	x21:
	mov dx, ctr11
	mov divby, dx
	x31:

	wow_divide1 divby 

	call convert_humi
	call num_out1
    jmp endisr2

    presisr2:

    
	mov bx, 00h
	mov cx, 12d
	lea si, vals111

	xadd11:
	mov dl, [si]
	mov dh, 00h
	add bx, dx
	inc si
	dec cx
	jnz xadd11
	mov ax, bx

	mov dx, flagcount111
	cmp dx, 1
	jnz x211

	mov divby, 12d
	jmp x311

	x211:
	mov dx, ctr111
	mov divby, dx
	x311:

	wow_divide2 divby 

	call convert_pres
	call num_out11


endisr2:

iret 
 
;button interrupt
isr0:
	mov updatenow, 01h
iret  

;-----------------------END OF ISRs------------------------;



quit:
hlt

