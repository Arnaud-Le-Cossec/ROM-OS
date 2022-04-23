;*******************************************
;                ROM-OS v1.4.0
;         (c)2022 LE COSSEC Arnaud
;         lecossec.arnaud@gmail.com
;   Description : Operating System for the 
;   ZEPHYR COMPUTER SYSTEMS' DX82 computer
;        line, based on the Z80 CPU.
;   The memory map used by this OS is shown
;     in the file attached to this one.
;
;*******************************************
; Update log :
;       1.0 - Monitor finished
;       1.1 - Command system introduction
;       1.2 - Commands implementation : /RUN /LOAD /SAVE (none works)
;       1.3 - implement ">j xxxx" -> "jump" command for the monitor
;           - /RUN and /LOAD fixed
;			- /IN implemented but not tested
;		1.4 - complete system rewrite with new memory map
;			[Snapshot_08_02_22] : System boot, Interrupts and MON_READ

;*******************************************
; LABELS ASSIGNATION
;*******************************************
VIA_PORTB     = %10000000
VIA_PORTA     = %10000001
VIA_DDRB      = %10000010
VIA_DDRA      = %10000011
VIA_PCR       = %10001101
VIA_IER       = %100001110

COM1          = %00001000
COM2          = %00000100
ACIA_RW       = %10010000
ACIA_RST_ST   = %10010001
ACIA_CMD      = %10010010
ACIA_CTR      = %10010011


RAM_AVAILABLE = $FF00
COM1_SETTINGS = $FF01 
COM2_SETTINGS = $FF02 
COM_SELECT    = $FF03

KEYBUFFER     = $FF10
STACK         = $FFDF

BC_CACHE       = $FFF0
DE_CACHE       = $FFF2
HL_CACHE       = $FFF4
SYS_SETTINGS_A = $FFF6
SYS_SETTINGS_B = $FFF7
KEYPOINTER     = $FFF8
RX_BUFFER_COM1 = $FFFA
RX_BUFFER_COM2 = $FFFB
NMI_VECTOR     = $FFFC
IRQ_VECTOR     = $FFFE


; WORK RAM
MON_ADDR_CACHE = $FFE0
MON_LINE_CACHE = $FFE2
MON_PRINT_BUFFER = $FFE4

;*******************************************
; SETUP 
;*******************************************

.org $0002
 di 												; Disable interrupts
 im 1 												; Set interrupt mode to 1

 ld sp,STACK   										; Define stack pointer memory start

 ld hl,KEYBUFFER 									; Initialize KEYPOINTER
 ld (KEYPOINTER),hl 

 ld hl,ACIA_RX_IRQ 									; Initialize IRQ_VECTOR
 ld (IRQ_VECTOR),hl 



 jp STARTUP 										; Jump to startup sequence

.org $0035 											; Loop return anchor point
 jp LOOP_RETURN
 
;*******************************************
; INTERRUPT
;*******************************************
.org $0038 ; IRQ
 di 												; Disable interrupts
 EXX 												; Save registers
 ld hl,(IRQ_VECTOR) 								; Load HL with the IRQ Vector (Address of the IRQ handler)
 jp (hl) 											; Jump tp the IRQ handler

ACIA_RX_IRQ: 										; IRQ handler for ACIA
 in a,(ACIA_RST_ST | COM1)	 						;	Check if COM1 generated an interrupt 
 AND %10000000
 jp nz, ACIA_RX_IRQ_Next 							; 	If not, check COM2
 in a,(ACIA_RW | COM1)	 							; 	If so, save RX register into RX buffer
 ld (RX_BUFFER_COM1),a
ACIA_RX_IRQ_Next:
 in a,(ACIA_RST_ST | COM2)	 						;	Check if COM2 generated an interrupt 
 AND %10000000
 jp nz, IRQ_Return 									; 	If not, Return from interrupt
 in a,(ACIA_RW | COM2) 								; 	If so, save RX register into RX buffer
 ld (RX_BUFFER_COM2),a

IRQ_Return:											
 EXX												; Retrieve registers
 ei 												; Enable interrupts back
 reti 												; Retrun from interrupt

;*******************************************
; STARTUP
;*******************************************

STARTUP:
 													; Initialize ACIA - COM1
 out (ACIA_RST_ST | COM1 ),a 						; 	soft reset (value not important)
 ld a,%01101001 									; 	disable parity, echo mode , enable RX interrupt and enable reciver/transmiter
 out (ACIA_CMD | COM1 ),a
 ld a,%00111000										; 	1200 bauds, 7 data bits + even parity , 1 stop bit - 
 ld (COM1_SETTINGS),a 								; 	Save COM1 Settings
 out (ACIA_CTR | COM1),a
 ld a,COM1 											;   Make COM1 the default channel
 ld (COM_SELECT),a
 													; Initialize ACIA - COM2
 out (ACIA_RST_ST | COM2 ),a 						; 	soft reset (value not important)
 ld a,%01101001 									; 	disable parity, echo mode , enable RX interrupt and enable reciver/transmiter
 out (ACIA_CMD | COM2 ),a
 ld a,%00111000										; 	1200 bauds, 7 data bits + even parity , 1 stop bit - 
 ld (COM2_SETTINGS),a 								; 	Save COM2 Settings  
 out (ACIA_CTR | COM2),a
 
 ld hl,$8000 										; Perform memory verification from $8000 to $7EFF (User RAM)
 ld de,$7EFF										; Also clears selected RAM
 call MemCheck
 
 ld a,$C9 											; Emergency RETurn at the end of USER RAM space, in case the user forgot to put one
 ld ($FEFF),a


 ld a,$0C 
 call Char_SEND

 ld hl,MSG ; send startup message
 ld de,$6A
 call Serial_SEND

 ei
 jp LOOP_RETURN


; MAIN LOOP ********************************

ACK_LOOP:                               ; ACKNOWLEDGEMENT LOOP
                                        ;   if RX_BUFFER == $0 , then loop back, otherwise, continue
 ld a,(RX_BUFFER_COM1)
 cp $0
 jp z,ACK_LOOP 
                                        ;   So there is data in RX_BUFFER, let's clear it ! (while saving the input)
 ld b,a
 ld a,$0
 ld (RX_BUFFER_COM1),a
 ld a,b
                                        ;   Check data

 cp $7F 								;	'DEL'
 jp z,KEY_DELETE

 cp $20 								;	Regular ASCII char > $20
 jp nc,KEY_ADD 							

 cp $08                                 ;   Backspace
 jp z,KEY_DELETE

 cp $0D                                 ;   Carriage Return
 jp z,KEY_ENTER

 jp ACK_LOOP                            ;   If the character is not recognized, it is ignored and we loop again

KEY_ENTER:                              ; KEY_ENTER : Carriage Return has been entered meaning the line is complete and we scan the type of the input 
 ld a,(KEYBUFFER)                       ;   scan the first character in the keyboard bufffer
 cp ">"                                 ;   ">" : command for MONITOR
 jp z,MONITOR
 cp "<"                                 ;   "<" : command for MONITOR-READ BACKWARD
 jp z,MON_READ_BACKWARD
 ;cp "/"                                 ;   "/" : command for command...wait, what ?
 ;jp z,INTERPRETER_CMD
 cp "0"                                 ;   "0" : HOME (Soft Reset)
 jp z,$0000
 ;cp "#"                                 ;   "#" : shotcut for '/RUN'
 ;jp z,CMD_RUN
 jp SYNTAX_ERROR                        ;   If there is no match, display an error


KEY_DELETE:                             ; KEY_DELETE : delete the current character
 ld hl,(KEYPOINTER)                     ;   Load hl with the address of the latest character in the keyboard buffer
 ld a,l                                 ;   Load register l to a for further operations
 cp ((KEYBUFFER+1) & $FF)                 ;   If l < $11 we can't delete more
 jp c,ACK_LOOP                          ;   So we ignore the deleting and we go back to the main loop
 ld (hl),$0                             ;   Otherwise, we erase what was at KEYPOINTER
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
 cp ((KEYBUFFER+$50) & $FF) 
 jp nc,ACK_LOOP                         ;   If l >= $5F we can't add more : go back to the main loop
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
 ld hl,KEYBUFFER                        ;   Initialize hl
 ld b,$50
LOOP_RETURN_1:                          ;   Clear KEYBUFFER with 0
 ld (hl),$0 
 inc hl   
 DJNZ LOOP_RETURN_1

 ld hl,KEYBUFFER                        ;   Initialize KEYPOINTER with the start of the keybuffer
 ld (KEYPOINTER),hl

 ld hl,READY                            ;   Send READY message
 ld de,$8
 call Serial_SEND

 jp ACK_LOOP                            ;   Go back to the main loop

; APPS *************************************

; ******************************************
; MONITOR v1.2
; ******************************************
; Commands : (AAAA = address, DD = data) 
; Read memory : >rAAAA 
; Write memory : >wAAAA;DD;DD;DD ...
; Erase memory : >eAAAA,AAAA
; Copy memory : >cAAAA,AAAA;AAAA
; [Newly Added] Jump to address ; >jAAAA

MONITOR:                                ; MONITOR : Monitor command identifiyer
 ld a,(KEYBUFFER+1)                     ;   Read keybuffer 2nd character (the first being ">")
 cp "r"                                 ;   "r" : MONITOR READ
 jp z,MON_READ
 ;cp "w"                                 ;   "w" : MONITOR WRITE
 ;jp z,MON_WRITE
 ;cp "e"                                 ;   "e" : MONITOR ERASE 
 ;jp z,MON_ERASE
 ;cp "c"                                 ;   "c" : MONITOR COPY
 ;jp z,MON_COPY
 ;cp "j"                                 ;   "j" : MONITOR JUMP & EXECUTE
 ;jp z,MON_JUMP
 cp $0                                  ;   null : READ FORWARD
 jp MON_READ_INIT
 jp SYNTAX_ERROR 


; MONITOR READ ****
MON_READ_BACKWARD:                      ; MON_READ_BACKWARD : shortcut "<" to setup the monitor to read the previous page
 ld de,$0100 
 ld hl,(MON_ADDR_CACHE)
 sbc hl,de                              ;   Substract the previous address used by the monitor by $100 (two pages)
 ld (MON_ADDR_CACHE),hl
 jp MON_READ_INIT

MON_READ:                             	; MON_READ : decodes the command's argument
 ld hl,KEYBUFFER+1
 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE),de              	;   Store decoded address in RAM

MON_READ_INIT:
 ld a,$0
 ld (MON_LINE_CACHE),a

MON_READ_LOOP:
 ld a,(MON_LINE_CACHE)
 cp $10
 call nz,MON_READ_LINE
 ld hl,MON_LINE_CACHE
 inc (hl)
 jp LOOP_RETURN


MON_READ_LINE:
 call PRINT_CR_LF 						; New line : Carriage return, Line feed

;[DISPLAY START ADDRESS]
 ld hl,(MON_ADDR_CACHE)              	;   Convert the most significant byte it to hexadecimal (ASCII format)
 push hl 			     				;   Save reading address for later
 ld a,h
 call BinToASCII
 ld (MON_PRINT_BUFFER),de

 ld hl,(MON_ADDR_CACHE)				;   Convert the least significant byte it to hexadecimal (ASCII format)
 ld a,l
 call BinToASCII 
 ld (MON_PRINT_BUFFER+2),de

 ld hl,MON_PRINT_BUFFER
 ld de,$3
 call Serial_SEND                       ;   Display the converted addresses


;[DISPLAY BYTES]
 ld b,$08
MON_READ_LINE_LOOP_1:
 ld a,$20                               ;   send "space"
 call Char_SEND
 ld hl,(MON_ADDR_CACHE)            		;   Load reading address
 ld a,(hl)
 
 call BinToASCII                        ;   Convert read byte to ascii
 ld (MON_PRINT_BUFFER),de
 ld hl,MON_PRINT_BUFFER 				;   Display it
 ld de,$1
 call Serial_SEND  
 
 ld hl,MON_ADDR_CACHE
 inc (hl) 								; Inc reading address
 djnz MON_READ_LINE_LOOP_1				; decrement b, if b=0 then continue, otherwise loop again

;[DISPLAY ASCII] 							Ascii representation of the bytes read
 ld a,$20                               ;   send "space"
 call Char_SEND
 pop hl 								;  Retreive first reading address
 ld b,$08
MON_READ_LINE_LOOP_2:
 ld hl,(MON_ADDR_CACHE)            		;   Load reading address
 ld a,(hl)

 cp $7F                                 ;   If Byte >= $7F
 jp nc,MON_READ_LINE_3                  ;   its not a character and therefore we skip it
 cp $20                                 ;   If Byte < $20
 jp nc,MON_READ_LINE_4                  ;   its not a character and therefore we skip it

MON_READ_LINE_3:
 ld a,"."
MON_READ_LINE_4:
 call Char_SEND

 ld hl,MON_ADDR_CACHE
 inc (hl) 								; Inc reading address
 djnz MON_READ_LINE_LOOP_1				; decrement b, if b=0 then continue, otherwise loop again

;[LINE END]
 ret






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



; TEXT DATA *****************************
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
 .db "ROM-OS v1.4 (c)2022 LE COSSEC Arnaud"
 .db $0D ; carriage return
 .db $0A ; line feed
 .db "32,511 BYTES FREE [Snapshot 15/02/22]"
READY:
 .db $0D ; carriage return
 .db $0A ; line feed
 .db "READY"
NEWLINE:
 .db $0D ; carriage return
 .db $0A ; line feed 


; SUBROUTINES ******************************

.org $2000                              ; SERIAL SEND
;*****************THIS SUBROUTINE WORKS !!!!
; -> Sends a string of data to the selected serial port
Serial_SEND:                            ;   Load registers as follow hl = start address , de = length
 ;in a,(ACIA_RST_ST)                    ;   To use instead of delay (remove line featuring (1))
 ;AND %00010000                         ;   when using to orginial 6551 chip (not WDC W65C51 that have bugs and thus require the delay)
 ;jp z,Serial_SEND
 push de                                ;   Save de to stack (1)
 ld de, $0120                           ;   Set delay between characters sent (1)
 call Delay                             ;   Call Delay (1)
 pop de                                 ;   Retrieve de from stack (1)

 ld a,(COM_SELECT) 						;	Select Port ( ACIA_RW | COM_SELECT )
 OR ACIA_RW
 ld c,a
 ld a,(hl)                              ;   Load data from address pointed by hl

 out (c),a                        ;   Output data to the 6551's transmit register
 
 inc hl                                 ;   Prepare next byte by incrementing hl  
 dec de                                 ;   Decrement de (length)
 ld a,d                                 ;   Check de > 0
 cp $FF   
 jp nz,Serial_SEND                      ;   If so, continue and loop back
 ld a,e   
 cp $FF   
 JP nz,Serial_SEND                      ;       "                   "
 ret 

.org $2080                              ; CHAR SEND
;*****************THIS SUBROUTINE WORKS !!!!
; -> Sends a single byte to selected serial port
Char_SEND:
 push af 								; 	Save a
Char_1:
 ;in a,(ACIA_RST_ST) 					;   To use instead of delay (remove line featuring (1))
 ;AND %00010000 						;   When using to orginial 6551 chip (not WDC W65C51 that have bugs and thus require the delay)
 ;jp z,Char_1
 ld de, $0120 							;   Set delay between characters sent (1)
 call Delay 							;   Call Delay (1)

 ld a,(COM_SELECT) 						;	Select Port ( ACIA_RW | COM_SELECT )
 OR ACIA_RW
 ld c,a

 pop af 								; 	Retrieve a
 out (c),a
 ret

.org $2100
;*****************THIS SUBROUTINE WORKS !!!!
CharLOAD: ; will watch for a char to be recived
 ld a,(COM_SELECT) 						;	Select Port ( ACIA_RST_ST | COM_SELECT )
 OR ACIA_RST_ST
 ld c,a
CharLOAD_LOOP:
 in a,(c)
 AND %00001000 							; 	If recive flag is not 0 (data recived), Loop again
 jp z,CharLOAD

 ld a,(COM_SELECT) 						;	Select Port ( ACIA_RW | COM_SELECT )
 OR ACIA_RW
 ld c,a

 in a,(c) 								;	Recived Char into register a
 ret

.org $2180
;*****************THIS SUBROUTINE WORKS !!!!
MemCheck: ; hl = start address / de = lenght of the test
 ld (hl),%11111111   					; fill RAM
 nop
 ld a,(hl)   							; and check if the data is still there
 cp $FF
 JP nz,RAM_ERROR
 ld (hl),$0   							; clear bytes checked
 inc hl

 dec de
 ld a,d
 cp $FF
 jp nz,MemCheck
 ld a,e
 cp $FF
 jp nz,MemCheck

 ret

RAM_ERROR:
 ld hl,RAM_ERROR_MSG				 	; Send error message
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
 out (ACIA_RW | COM1),a
 
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