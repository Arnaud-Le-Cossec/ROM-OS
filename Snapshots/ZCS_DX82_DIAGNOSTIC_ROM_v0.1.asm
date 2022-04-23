;*******************************************
;        ZCS DX82 DIAGNOSTIC ROM v0.1
;         (c)2022 LE COSSEC Arnaud
;         lecossec.arnaud@gmail.com
;   Description : Operating System for the 
;   ZEPHYR COMPUTER SYSTEMS' DX82 computer
;        line, based on the Z80 CPU.
;   The memory map used by this OS is shown
;     in the file attached to this one.
;
;           ALL WRONGS RESERVED
;*******************************************
; Update log :
;

; LABELS ASSIGNATION ***********************
VIA_PORTB = $80
VIA_PORTA = $81
VIA_DDRB = $82
VIA_DDRA = $83
VIA_T1C_L = $84
VIA_T1C_H = $85
VIA_T1L_L = $86
VIA_T1L_H = $87
VIA_ACR = $8B
VIA_PCR = $8C
VIA_IFR = $8D
VIA_IER = $8E

COM1 = %00001000
COM2 = %00000100
ACIA_RW = %10011000
ACIA_RST_ST = %10011001
ACIA_CMD = %10011010
ACIA_CTR = %10011011

KEYBUFFER = $FF26
STACK = $FFF7
KEYPOINTER = $FFF8
RX_BUFFER =$FFFA
NMI_VECTOR = $FFFC
IRQ_VECTOR = $FFFE

TEST_LENGTH = $9C40 ; ~2s at 1Mhz

; SETUP ************************************
.org $0002
 di                                     ; Disable interrupts

 im 1                                   ; Select interrupt Mode 1 (check Z80 documentation)

 ld sp,STACK                            ; Define stack pointer memory start

 jp TESTS                            	; Jump to startup procedures



; INTERRUPTS *******************************

.org $0038                              ; IRQ Handler
 di                                     ;   Disable interrupts
 EXX                                    ;   Save registers
 ld hl,(IRQ_VECTOR)                     ;   Load interrupt vector
 jp (hl)                                ;   Jump to interrupt vector

VIA_TIMER1_IRQ: 						; VIA interrupts 
 in a,(VIA_IFR) 						;	Load VIA interrupt flag
 AND %01000000 							;	Isolate Timer1 interrupt flag
 jp nz,ACIA_RX_IRQ  					;	Check if Timer1 interrupt was triggered
 in a,(VIA_T1C_L) 						;	Clear interrupts
 
 in a,(VIA_PORTB) 					    ; 	Update TEST output to Good
 AND %1111 1101 						;	Clear 'Bad test' flag
 OR $01 								;	Set 'Good test' flag
 out (VIA_PORTB),a
 jp IRQ_Return

ACIA_RX_IRQ:                            ; ACIA receiving interrupt routine
 in a,(ACIA_RST_ST)                     ;   Load ACIA status register
 AND %10000000                          ;   Isolate bit 7 : Data received flag
 jp nz, IRQ_Return                      ;   If not set, return from interrupt
 in a,(ACIA_RW)                         ;   If set, load ACIA receive register
 ld (RX_BUFFER),a                       ;   and store the data into RX_BUFFER in RAM
                                        ;   Then return from interrupt
IRQ_Return:                             ; Generic interrupt retrun procedure
 EXX                                    ;   Retrieve registers
 ei                                     ;   Enable interrupts back
 reti                                   ;   Return from interrupt

; STARTUP **********************************

TESTS:                                	; STARTUP PROCEDURE

;[TEST #0] - VIA PORTB TEST
; LED shifting to left

TEST0_INIT:
 ld a,$FF 								; Set DDRB to output
 out (VIA_DDRB),a
 
 ld a,$00 								; Reset PORTB
 out (VIA_PORTB),a
 ld a,$01
 ld b,$08
TEST0_LOOP: 
 out (VIA_PORTB),a 						; Output [a] on PORTB
 rlca 									; Shift Left [a]
 
 ld de,$2710 							; For ~500ms delay at 1Mhz
TEST0_Delay: 							; de=input value --> 58 Clock periodes : 58µs/cycle @1Mz OR 14.50µs/cycle @4MHz 
 dec de         						; T=6 , 1.50
 ld a,d         						; T=4 , 1.00
 cp $FF         						; T=7 , 1.75
 jp nz,TEST0_Delay    					; T=10 , 2.50
 ld a,e         						; T=4 , 1.00
 cp $FF         						; T=7 , 1.75
 jp nz,TEST0_Delay    					; T=10 , 2.50

 
 djnz TEST0_LOOP 						; Decrement b, if b=0 then continue, otherwise loop again


;[TEST #1] - VIA Beeper

TEST1_INIT:
 ld a,$1F								; Output Test ID and result on PORTB
 out (VIA_PORTB),a

 ld de,TEST_LENGTH 						; For ~2s delay at 1Mhz
TEST1_Delay: 							; de=input value --> 58 Clock periodes : 58µs/cycle @1Mz OR 14.50µs/cycle @4MHz 
 dec de         						; T=6 , 1.50
 ld a,d         						; T=4 , 1.00
 cp $FF         						; T=7 , 1.75
 jp nz,TEST1_Delay    					; T=10 , 2.50
 ld a,e         						; T=4 , 1.00
 cp $FF         						; T=7 , 1.75
 jp nz,TEST1_Delay    					; T=10 , 2.50

;[TEST #2] - VIA Timer (interrupts)

TEST2_INIT:
 ld a,$22								; Output Test ID and result on PORTB
 out (VIA_PORTB),a

 ld a,%11000000 						; Configure VIA ACR register (timer settings) : pulse on PB7 and continuous interrupts
 out (VIA_ACR),a

 ld a,%01000000
 out (VIA_IER),a 						; Configure VIA interrupts on Timer1 only

 ld a,$FF 								; Load Timer1 latches
 out (VIA_T1L_L),a
 out (VIA_T1L_H),a

 ei 									; Enable interrupts for the length of the test

 ld de,TEST_LENGTH 						; For ~2s delay at 1Mhz
TEST2_Delay: 							; de=input value --> 58 Clock periodes : 58µs/cycle @1Mz OR 14.50µs/cycle @4MHz 
 dec de         						; T=6 , 1.50
 ld a,d         						; T=4 , 1.00
 cp $FF         						; T=7 , 1.75
 jp nz,TEST2_Delay    					; T=10 , 2.50
 ld a,e         						; T=4 , 1.00
 cp $FF         						; T=7 , 1.75
 jp nz,TEST2_Delay    					; T=10 , 2.50

 di 									; Disable interrupts


;[TEST #3] - ACIA 

TEST3_INIT:
 ld a,$3A								; Output Test ID and result on PORTB
 out (VIA_PORTB),a

 out (ACIA_RST_ST),a                    ;   Soft reset (value not important)

 ld a,%01101001                         ;   Disable parity, echo mode , enable RX interrupt and enable reciver/transmiter
 out (ACIA_CMD),a

 ld a,%00111000                         ;   1200 bauds, 7 data bits + even parity , 1 stop bit - 
 out (ACIA_CTR),a
 
 in a,(ACIA_CMD)
 cp %01101001
 jp nz,TEST3_NEXT
 
 in a,(VIA_PORTB) 						;	Change 
 AND %11110111
 OR $04
 out (VIA_PORTB),a

TEST3_NEXT:
















 ld hl,$8000                            ;   Memory check from address $8000 to $7EFF
 ld de,$7EFF
 call MemCheck

 ld a,$0C                               ;   Clear screen
 call Char_SEND

 ld hl,MSG                              ;   Send startup message
 ld de,$6A
 call Serial_SEND

 ei                                     ;   Enable interrupts
 jp LOOP_RETURN                         ;   Jump to LOOP_RETURN to start user input scan and processing

; MAIN LOOP ********************************

ACK_LOOP:                               ; ACKNOWLEDGEMENT LOO^P
                                        ;   if RX_BUFFER == $0 , then loop back, otherwise, continue
 ld a,(RX_BUFFER)
 cp $0
 jp z,ACK_LOOP 
                                        ;   So there is data in RX_BUFFER, let's clear it ! (while saving the input)
 ld b,a
 ld a,$0
 ld (RX_BUFFER),a
 ld a,b
                                        ;   test if input is a character , above $20 and below $7F, otherwise its a control character.
 cp $20        
 jp c,KEY_CMD
 cp $7F
 jp nc,KEY_CMD
                                        ;   now that we know that we're dealing with regular characters, let add them to memory
 jp KEY_ADD

KEY_CMD:                                ; KEY_CMD : well, its a control character, but which ?
 cp $7F                                 ;   DEL
 jp z,KEY_DELETE 
 cp $08                                 ;   Backspace
 jp z,KEY_DELETE
 cp $0D                                 ;   Carriage Return
 jp z,KEY_ENTER
 jp ACK_LOOP                            ;   If the control character is not recognized, it is ignored and we go back to the main loop

KEY_ENTER:                              ; KEY_ENTER : Carriage Return has been entered meaning the line is complete and we scan the type of the input 
 ld a,(KEYBUFFER)                       ;   scan the first character in the keyboard bufffer
 cp ">"                                 ;   ">" : command for MONITOR
 jp z,MONITOR
 cp "<"                                 ;   "<" : command for MONITOR-READ BACKWARD
 jp z,MON_READ_BACKWARD
 cp "/"                                 ;   "/" : command for command...wait, what ?
 jp z,INTERPRETER_CMD
 cp "0"                                 ;    "0" : HOME (Soft Reset)
 jp z,$0002
 cp "#"                                 ;   "#" : shotcut for '/RUN'
 jp z,CMD_RUN
 jp SYNTAX_ERROR                        ;   If there is no match, display an error


KEY_DELETE:                             ; KEY_DELETE : delete the current character
 ld hl,(KEYPOINTER)                     ;   load hl with the address of the latest character in the keyboard buffer
 ld a,l                                 ;   load register l to a for further operations
 cp $26                                 ;   if h < $26 we can't delete more
 jp c,ACK_LOOP                          ;   So we ignore the deleting and we go back to the main loop
 ld (hl),$0                             ;   if h > $26, we erase what was at KEYPOINTER
 dec hl
 ld (KEYPOINTER),hl
 ld a,$08                               ;   Move the cursor back
 call Char_SEND
 ld a,$20                               ;   Erasing the character by replacing it with a space
 call Char_SEND
 ld a,$08                               ;   Move the cursor back again
 call Char_SEND
 jp ACK_LOOP                            ;   Erasing completed : Go back to the main loop

KEY_ADD:                                ; KEY_ADD : Adds a character in the keyboard bufferinc KEYPOINTER and add character
 ld b,a                                 ;   Save a (which contains the character that we want to add)
 ld hl,(KEYPOINTER)                     ;   Load KEYPOINTER, the address of the last added character in the buffer
 ld a,l                                 ;   Check if there is room in the buffer for a new charater
 cp $76 
 jp nc,ACK_LOOP                         ;   If l > $77 we can't add more : go back to the main loop
 ld a,b                                 ;   Otherwise retrieve a
 ld (hl),a                              ;   Load the character in the buffer
 inc hl                                 ;   Increment the address of the last added character
 ld (KEYPOINTER),hl                     ;   Save it to KEYPOINTER
 call Char_SEND                         ;   Don't forget to display the character !
 jp ACK_LOOP                            ;   Adding completed : Go back to the main loop

SYNTAX_ERROR:                           ; SYNTAX_ERROR : handles the typing errors of the user
 ld hl,SYNTAX_ERROR_MSG                 ;   Send error message
 ld de,$0D
 call Serial_SEND
LOOP_RETURN:                            ; LOOP_RETURN : Restoration point for programs and commands to re-enter the ACK_LOOP
 ld hl,KEYBUFFER+1                      ;   Load keybuffer+1 
LOOP_RETURN_1:                          ;   Clear KEYBUFFER with 0
 ld (hl),$0 
 inc hl   
 ld a,l 
 cp $76
 jp c,LOOP_RETURN_1

 ld hl,KEYBUFFER                        ;   Initialize KEYPOINTER with the start of the keybuffer
 ld (KEYPOINTER),hl

 ld hl,READY                            ;   Send READY message
 ld de,$8
 call Serial_SEND

 jp ACK_LOOP                            ;   Go back to the main loop

; APPS *************************************

; MISCELLANEOUS ****************************

PRINT_CR_LF:
 ld a,$0D                       		;	carriage return 
 call Char_SEND
 ld a,$0A 								;   Line feed and 
 call Char_SEND
 ret

; Decodes an address written in ASCII
Interpreter_ADDR:                       ; Interpreter_ADDR  de = address output / if a=1 then an error has occured
 inc hl                                 ;   Inc hl
 ld a,(hl)                              ;   Load content of address of KEYBUFFER pointed by hl into a
 cp $20                                 ;   If that value is 'space'
 jp z,Interpreter_ADDR                  ;   Then loop again until we hit a character
 cp $0                                  ;   Otherwise if character = 0; send error
 jp z,Interpreter_ADDR_Error
 call AsciiToHex                        ;   Convert the most significant Byte from ascii to 'regular' binary
 ld d,a                                 ;   Save result to d
 ld a,b                                 ;   Error flag stored in register b
 cp $1                                  ;   If b = 1, Send an error
 jp z,Interpreter_ADDR_Error
 inc hl                                 ;   Otherwise we continue
 call AsciiToHex                        ;   Convert the least significant Byte from ascii to 'regular' binary
 ld e,a                                 ;   Save result to e
 ld a,b                                 ;   Error flag stored in register b
 cp $1                                  ;   If b = 1, Send an error
 jp z,Interpreter_ADDR_Error
 ret                                    ;   Return from subroutine
Interpreter_ADDR_Error:                 ;   Error handeling
 ld a,$1                                ;   If an error occured, register it register a
 ret


; SUBROUTINES ******************************

.org $2000                              ; SERIAL SEND
;*****************THIS SUBROUTINE WORKS !!!!
; -> Sends a string of data to serial port
Serial_SEND:                            ;   Load registers as follow hl = start address , de = length
 ;in a,(ACIA_RST_ST)                    ;   To use instead of delay (remove line featuring (1))
 ;AND %00010000                         ;   when using to orginial 6551 chip (not WDC W65C51 that have bugs and thus require the delay)
 ;jp z,Serial_SEND
 ld bc,de                               ;   Save de to bc (1)
 ld de, $0120                           ;   Set delay between characters sent (1)
 call Delay                             ;   Call Delay (1)
 ld de,bc                               ;   Retrieve de from bc (1)
 ld a,(hl)                              ;   Load data from address pointed by hl
 out (ACIA_RW),a                        ;   Output data to the 6551's transmit register
 
 inc hl                                 ;   Prepare next byte by incrementing hl  
 dec de                                 ;   Decrement de (length)
 ld a,d                                 ;   Check de > 0
 cp $FF   
 jp nz,Serial_SEND                      ;   If so, continue and loop back
 ld a,e   
 cp $FF   
 JP nz,Serial_SEND                      ;       "                   "
 ret                                    ;   Return from subroutine

.org $2080                              ; CHAR SEND
;*****************THIS SUBROUTINE WORKS !!!!
; -> Sends a single byte to serial port
Char_SEND:
 ld b,a ; save a
Char_1:
 ;in a,(ACIA_RST_ST)
 ;AND %00010000
 ;jp z,Char_1
 ld de, $0120
 call Delay
 ld a,b ; retrieve a
 out (ACIA_RW),a
 ret

.org $2100
;*****************THIS SUBROUTINE WORKS !!!!
CharLOAD: ; will watch for a char to be recived
 in a,(ACIA_RST_ST)
 AND %00001000
 jp z,CharLOAD
 in a,(ACIA_RW)
 ret

.org $2180
;*****************THIS SUBROUTINE WORKS !!!!
MemCheck: ; hl = start address / de = lenght of the test 
 ld (hl),%11111111   ; fill RAM
 nop
 ld a,(hl)   ; and check if the is still there
 cp $FF
 JP nz,RAM_ERROR
 ld (hl),$0   ; clear bytes checked
 inc hl
 dec de
 ld a,d
 cp $FF
 jp nz,MemCheck
 ld a,e
 cp $FF
 jp nz,MemCheck
 dec hl
 ret
RAM_ERROR:
 ld hl,RAM_ERROR_MSG ; send startup message
 ld e,$09
RAM_ERROR_1:
 ld bc, $0120
RAM_ERROR_2:
 dec bc             ; T=6 , 1.50
 ld a,b             ; T=4 , 1.00
 cp $FF             ; T=7 , 1.75
 jp nz,RAM_ERROR_2  ; T=10 , 2.50
 ld a,c             ; T=4 , 1.00
 cp $FF             ; T=7 , 1.75
 jp nz,RAM_ERROR_2  ; T=10 , 2.50

 ld a,(hl)
 out (ACIA_RW),a
 
 inc hl   
 dec e   
 ld a,e   
 cp $FF   
 JP nz,RAM_ERROR_1
 ;ld a,%10000000
 ;out (VIA_PORTB),a
 halt

.org $2200
;*****************THIS SUBROUTINE WORKS !!!!
Random: ; RANDOM number generator : a = final result
 ld a,r         ; Load the A register with the refresh register
 ld l,a         ; Copy register A into register L
 ld h,a         ; Copy register A into register H
 ld a,(hl)      ; increases the randomness of the number by forming an address
 ret

.org $2280
;*****************THIS SUBROUTINE WORKS !!!!
Delay: ; de=input value --> 14.50 µs / cycle
 dec de         ; T=6 , 1.50
 ld a,d         ; T=4 , 1.00
 cp $FF         ; T=7 , 1.75
 jp nz,Delay    ; T=10 , 2.50
 ld a,e         ; T=4 , 1.00
 cp $FF         ; T=7 , 1.75
 jp nz,Delay    ; T=10 , 2.50
 ret            ; T=10 , 2.50

.org $2300
;*****************THIS SUBROUTINE WORKS !!!!
BinToASCII: ; this subroutine converts a binary number into an ascii-based hex number, ready to be displayed : a=input, de = output
 push af
 AND %00001111 ;-> look at the first nibble 
 ld hl,HEX_TABLE
 ld c,a
 ld b,$0
 add hl,bc
 ld d,(hl)

 pop af
 AND %11110000 ;-> look at the second nibble 
 rra
 rra
 rra
 rra

 ld hl,HEX_TABLE
 ld c,a
 ld b,$0
 add hl,bc
 ld e,(hl)

 ret
HEX_TABLE:
 .db "0123456789ABCDEF"

.org $2380
;*****************THIS SUBROUTINE WORKS !!!!
AsciiToHex: ; hl = addr of the first char to convert (two of them) ; a = final result ; b = 1 if error ocured
 ld b,$0
 call AsciiToHex_1
 rlca
 rlca
 rlca
 rlca
 ld c,a 
 inc hl
 call AsciiToHex_1
 OR c
 ret
AsciiToHex_1:
 ld a,(hl)
 cp $30
 jp c,AsciiToHex_Error
 cp $3A
 jp nc,AsciiToHex_2
 sub $30
 ret
AsciiToHex_2:
 AND %11011111
 cp $41
 jp c,AsciiToHex_Error
 cp $47
 jp nc,AsciiToHex_Error
 sub $37
 ret
AsciiToHex_Error:
 ld b,$1
 ret









; RANDOM STUFF *****************************
WORKING_MSG:
 .db " WORKING..."
 .db $0D ; carriage return
 .db $0A ; line feed
CANCEL_MSG:
 .db "PRESS ANY KEY TO CANCEL"
 .db $0D ; carriage return
 .db $0A ; line feed
LOADING_MSG:
 .db "LOADING..."
LOADING_DONE_MSG:
 .db "DONE"
SYNTAX_ERROR_MSG:
 .db $0D ; carriage return
 .db $0A ; line feed
 .db "SYNTAX ERROR"
RAM_ERROR_MSG:
 .db "RAM "
ERROR_MSG:
 .db "ERROR"
MSG:
 .db "ZEPHYR COMPUTER SYSTEMS LTD."
 .db $0D ; carriage return
 .db $0A ; line feed
 .db "ROM-OS v1.3 (c)2022 LE COSSEC Arnaud"
 .db $0D ; carriage return
 .db $0A ; line feed
 .db "32,511 BYTES FREE [Snapshot 28/01/22]"
READY:
 .db $0D ; carriage return
 .db $0A ; line feed
 .db "READY"
NEWLINE:
 .db $0D ; carriage return
 .db $0A ; line feed 