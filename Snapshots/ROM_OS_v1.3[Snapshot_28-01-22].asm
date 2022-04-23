;*******************************************
;                ROM-OS v1.3
;         (c)2021 LE COSSEC Arnaud
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
;       1.0 - Monitor finished
;       1.1 - Command system introduction
;       1.2 - Commands implementation : /RUN /LOAD /SAVE (none works)
;       1.3 - implement ">j xxxx" -> "jump" command for the monitor
;           - implement "/in xx" -> "in" command to read peripherals register (26-01-22)
;           - changed CharSEND by CharLOAD in "CMD_LOAD"
;           - "/load" command fixed

; LABELS ASSIGNATION ***********************
VIA_PORTB = $80
VIA_PORTA = $81
VIA_DDRB = $82
VIA_DDRA = $83
VIA_PCR = $8C
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

;Monitor Index
MON_ADDR_CACHE = $FF00
MON_HL_CACHE = $FF20
MON_READ_HL_CACHE = $FF22
MON_LINE_CACHE = $FF24
MON_INDEX_CACHE = $FF25

; SETUP ************************************
.org $0002
 di                                     ; Disable interrupts
 im 1                                   ; Select interrupt Mode 1 (check Z80 documentation)
 ld sp,STACK                            ; Define stack pointer memory start
 ld hl,KEYBUFFER                        ; initialize KEYPOINTER
 ld (KEYPOINTER),hl

 ld hl,ACIA_RX_IRQ                      ; initialize IRQ_VECTOR
 ld (IRQ_VECTOR),hl

 ld hl,$0                               ; initialize MON_READ_HL_CACHE
 ld (MON_READ_HL_CACHE),hl 

 ld a,$C9                               ; emergency RETurn at the end of USER RAM space, in case the user forgot to put one
 ld ($FEFF),a
                                        ; SETUP ACIA
 out (ACIA_RST_ST),a                    ;   Soft reset (value not important)

 ld a,%01101001                         ;   Disable parity, echo mode , enable RX interrupt and enable reciver/transmiter
 out (ACIA_CMD),a

 ld a,%00111000                         ;   1200 bauds, 7 data bits + even parity , 1 stop bit - 
 out (ACIA_CTR),a
 
 jp STARTUP                             ; Jump to startup procedures


.org $0035                              ; fixed LOOP_RETURN anckor
 jp LOOP_RETURN

; INTERRUPTS *******************************

.org $0038                              ; IRQ Handler
 di                                     ;   Disable interrupts
 EXX                                    ;   Save registers
 ld hl,(IRQ_VECTOR)                     ;   Load interrupt vector
 jp (hl)                                ;   Jump to interrupt vector

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

STARTUP:                                ; STARTUP PROCEDURE
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

; ******************************************
; MONITOR v1.1
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
 cp "w"                                 ;   "w" : MONITOR WRITE
 jp z,MON_WRITE
 cp "e"                                 ;   "e" : MONITOR ERASE 
 jp z,MON_ERASE
 cp "c"                                 ;   "c" : MONITOR COPY
 jp z,MON_COPY
 cp "j"                                 ;   "j" : MONITOR JUMP & EXECUTE
 jp z,MON_JUMP
 cp $0                                  ;   null : READ FORWARD
 jp MON_READ_FORWARD
 jp SYNTAX_ERROR 

; MONITOR READ ****
MON_READ_BACKWARD:                      ; MON_READ_BACKWARD : shortcut "<" to setup the monitor to read the previous page
 ld de,$0100 
 ld hl,(MON_READ_HL_CACHE)
 sbc hl,de                              ;   Substract the previous address used by the monitor by $100 (two pages)
 ld (MON_READ_HL_CACHE),hl
 jp MON_READ_FORWARD

MON_READ:                               ; MON_READ : decodes the command's argument
 ld hl,KEYBUFFER+1
 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_READ_HL_CACHE),de              ;   Store decoded address in RAM
MON_READ_FORWARD:                       ;   Jump point to skip address decoding, used for the shortcut ">" to read the next page, and "<" to read the previous page
 ld a,$0
 ld (MON_LINE_CACHE),a                  ;   Initialize line count
 ld hl,NEWLINE                          ;   Line feed and carriage return 
 ld de,$01
 call Serial_SEND
MON_READ_2:                             ;   Loop to display the page
 ld a,(MON_LINE_CACHE)                  ;   Load line count
 cp $10                                 ;   if line count > 16 lines ($10), the page is finished and go back to the main loop
 jp nc,LOOP_RETURN 
 inc a                                  ;   Otherwise inc line count
 ld (MON_LINE_CACHE),a                  ;   Display line
 jp MON_READ_LINE

MON_READ_LINE:                          ; MON_READ_LINE : Displays a line with AAAA DD DD DD DD DD DD DD DD aaaaaaaa (AAAA=address, DD=Data, a=ASCII representing a data byte)
 ld hl,(MON_READ_HL_CACHE)              ;   [DISPLAY START ADDRESS] Read last address displayed
 ld a,h                                 ;   Convert the most significant byte it to hexadecimal (ASCII format)
 call BinToASCII
 ld (MON_ADDR_CACHE),de
 ld hl,(MON_READ_HL_CACHE)
 ld a,l                                 ;   Convert the least significant byte it to hexadecimal (ASCII format)
 call BinToASCII 
 ld (MON_ADDR_CACHE+2),de
 ld hl,MON_ADDR_CACHE
 ld de,$3
 call Serial_SEND                       ;   Display the converted address
 ld a,$0                                ;   Set byte index to zero for the line
 ld (MON_INDEX_CACHE),a             
MON_READ_LINE_1:                        ;   [DISPLAY BYTES] bytes themselves
 ld a,$20                               ;   send "space"
 call Char_SEND
 ld a,(MON_INDEX_CACHE)                 ;   retrieve byte index
 cp $08                                 ;   test if index >= 9
 jp nc,MON_READ_LINE_2                  ;   If so, jump to ascii representation printing
 inc a                                  ;   Increment index
 ld (MON_INDEX_CACHE),a                 ;   Save index
 ld hl,(MON_READ_HL_CACHE)              ;   Load reading address
 ld a,(hl)                              ;   Read byte at the previously specified address
 inc hl                                 ;   Increment reading address
 ld (MON_READ_HL_CACHE),hl              ;   Save reading address
 call BinToASCII                        ;   Convert read byte to ascii
 ld (MON_ADDR_CACHE),de
 ld hl,MON_ADDR_CACHE
 ld de,$1
 call Serial_SEND                       ;   Display converted byte
 jp MON_READ_LINE_1                     ;   Loop again
MON_READ_LINE_2:                        ;   [DISPLAY ASCII] Ascii representation of the bytes read
 ld hl,(MON_READ_HL_CACHE)              ;   Substract the reading address to where it was at the begining of the line
 ld bc,$09
 sbc hl,bc
 ld (MON_READ_HL_CACHE),hl              ;   Save the new address
 ld a,$0                                ;   Set byte index to zero
 ld (MON_INDEX_CACHE),a
MON_READ_LINE_3:
 ld a,(MON_INDEX_CACHE)                 ;   Retrieve byte index
 cp $08                                 ;   Test if index >= 9
 jp nc,MON_READ_LINE_5                  ;   If so jump to line end sequence
 inc a                                  ;   Increment index
 ld (MON_INDEX_CACHE),a                 ;   Save index
 ld hl,(MON_READ_HL_CACHE)              ;   Load reading address
 inc hl                                 ;   Increment reading address
 ld (MON_READ_HL_CACHE),hl              ;   Save reading address
 ld a,(hl)                              ;   Read byte at the previously specified address
 cp $20                                 ;   If Byte < $20 or, ASCII 'space', 
 jp c,MON_READ_LINE_4                   ;   its not a character and therefore we skip it
 cp $7F                                 ;   If Byte >= $7F or ASCII 'delete'
 jp nc,MON_READ_LINE_4                  ;   its not a character and therefore we skip it
 call Char_SEND                         ;   Now we know its a printable ASCII character, we display it
 jp MON_READ_LINE_3                     ;   Repeat the process 8 times
MON_READ_LINE_4:                        ;   When a byte is not indentified as a character, replace it with '.'
 ld a,$2E
 call Char_SEND
 jp MON_READ_LINE_3
MON_READ_LINE_5:                        ;   Prepare next line
 ld hl,(MON_READ_HL_CACHE)              ;   load, increment and save reading address one more time
 inc hl
 ld (MON_READ_HL_CACHE),hl
 ld hl,NEWLINE                          ;   Line feed and carriage return  
 ld de,$01
 call Serial_SEND
 jp MON_READ_2                          ;   Go back to MON_READ_2 for next line

MON_WRITE:                              ; MON_WRITE : write bytes in memory : ">wAAAA;DD;DD;DD;DD..." (AAAA=address, DD=Data)
 ld hl,KEYBUFFER+1                      ;   Decoding the command's address argument
MON_WRITE_1:
 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR

 ld (MON_HL_CACHE),hl
 ld (MON_ADDR_CACHE),de
MON_WRITE_2:                            ;   Writing sequence loop
 ld hl,(MON_HL_CACHE)                   ;   We seek the next byte to write in memory, indicated by ';'. hl is the address in keyboard buffer
 inc hl
 ld a,(hl)
 cp $20                                 ;   If keyboard buffer at hl is 'space', we search further
 jp z,MON_WRITE_2                       
 cp ";"                                 ;   Else if keyboard buffer at hl is different than ';', go back to main loop (ACK_LOOP)
 jp nz,LOOP_RETURN
 
 inc hl                                 ;   But if so, it means that a new byte has to be written
 call AsciiToHex                        ;   Convert user input (ASCII string) into a byte
 ld d,a                                 ;   Save result from register a to d
 ld a,b                                 ;   Error status of AsciiToHex is held into register b - so we tranfer it to a for testing
 cp $1                                  ;   If Error=1, ...
 jp z,SYNTAX_ERROR                      ;   ...Then send an error
 ld (MON_HL_CACHE),hl                   ;   Otherwise save hl for next bytes
 ld hl,(MON_ADDR_CACHE)                 ;   And load the address at which we want to write
 ld (hl),d                              ;   Write byte at that address
 inc hl                                 ;   Increment the address
 ld (MON_ADDR_CACHE),hl                 ;   Save it
 jp MON_WRITE_2                         ;   Loop back to check for next bytes

MON_ERASE:                              ; MON_ERASE : erases an area of memory : ">eSSSS,EEEE" (SSSS=Start address, EEEE=End address)
 ld hl,KEYBUFFER+1                      ;   Decoding the command's address argument
 call Interpreter_ADDR 
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_HL_CACHE),de
MON_ERASE_1:                            ;   We seek the end address
 inc hl
 ld a,(hl)
 cp $20                                 ;   If keyboard buffer at hl is 'space', we search further
 jp z,MON_ERASE_1
 cp ","
 jp nz,SYNTAX_ERROR                     ;   Else if keyboard buffer at hl is different than ',', display error

 call Interpreter_ADDR                  ;   Otherwise run address interpreter once more. the end address is stored in de
 cp $1                                  ;   Error held in register a.
 jp z,SYNTAX_ERROR
 inc de                                 ;   Prepare registers for erasing process
 
 ld hl,(MON_HL_CACHE)                   
 ld b,$0
MON_ERASE_2:                            ;   Erasing loop
 ld (hl),b                              ;   Set data at address pointed by hl at 0
 inc hl                                 ;   Increment hl
 ld a,h                                 ;   Compare hl with de (end address). if hl < de then continue...
 cp d
 jp c,MON_ERASE_2
 ld a,l
 cp e
 jp c,MON_ERASE_2
 jp LOOP_RETURN                         ;   Else go back to main loop (ACK_LOOP)

MON_COPY:                               ; MON_COPY : Copies a block of data to another place in memory : ">cSSSS,EEEE;NNNN" (SSSS=Start address, EEEE=End address, NNNN=New address)
 ld hl,KEYBUFFER+1                      ;   -> Decoding the command's Start address
 call Interpreter_ADDR 
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE),de                 ;   Save Start address
MON_COPY_1:                             ;   -> Decoding the command's End address
 inc hl
 ld a,(hl)
 cp $20                                 ;   If keyboard buffer at hl is 'space', we search further
 jp z,MON_COPY_1
 cp ","                                 ;   Else if keyboard buffer at hl is different than ',', display error
 jp nz,SYNTAX_ERROR                     

 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE+2),de               ;   Save End address
MON_COPY_2:                             ;   -> Decoding the command's New address
 inc hl
 ld a,(hl)
 cp $20                                 ;   If keyboard buffer at hl is 'space', we search further
 jp z,MON_COPY_2
 cp ";"                                 ;   Else if keyboard buffer at hl is different than ';', display error
 jp nz,SYNTAX_ERROR                     

 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE+4),de               ;   Save New address

 ld hl,(MON_ADDR_CACHE+2)               ;   Prepare registers for the copy instruction
 inc hl
 ld de,(MON_ADDR_CACHE)
 sbc hl,de
 ld bc,hl                               ;   bc = End-Start (lenght)
 ld hl,(MON_ADDR_CACHE)                 ;   hl = Start
 ld de,(MON_ADDR_CACHE+4)               ;   de = New

 ldir                                   ;   The magic command...
 jp LOOP_RETURN                         ;   Go back main Loop (ACK_LOOP)

MON_JUMP:                               ; MON_JUMP : jump to a specified address : ">jAAAA" (AAAA=Target Address)
 ld hl,KEYBUFFER+1                      ;   Decode the command's target address
 call Interpreter_ADDR 
 cp $1                                  ;   If Error (held in register a), Display error
 jp z,SYNTAX_ERROR
 ld h,d
 ld l,e
 jp (hl)                                ;   jump at address indicated by hl

; ******************************************
; COMMAND SYSTEM v1.0
; ******************************************
; Command : AAAA = address, DD = data
; Run command : /RUN
; Load command : /Load
; In command : /IN AA (returns value)

INTERPRETER_CMD:                        ; INTERPRETER_CMD : Decode commands
 ld c,$0
 ld hl,KEYBUFFER+1
INTERPRETER_CMD_1:                      ;   Associate the letters of the command in one single byte for easier selection later
 ld a,(hl)                              ;   Load character at hl (in the keyboard buffer)
 cp $0                                  ;   'void' OR 'space' stop this process and we jump to the command selection 
 jp z,INTERPRETER_CMD_2                 
 cp $20
 jp z,INTERPRETER_CMD_2
 AND %11011111                          ;   Make sure we are dealing with upper caps by applying a mask
 sub $41                                ;   Bring ascii character to zero : A=0, B=1, C=2 ...
 rlca                                   ;   a*2
 add a,c                                ;   Add new character to the character sum
 ld c,a                                 ;   save the sum to register c
 inc hl                                 ;   Increment hl 
 jp INTERPRETER_CMD_1                   ;   Loop back

INTERPRETER_CMD_2:                      ;   Character Selection
 ld a,c                                 ;   Load the sum from register c
 cp $64                                 ;   If sum = $64 then its a /RUN command
 jp z,CMD_RUN
 cp $38                                 ;   If sum = $38 then its a /LOAD command
 jp z,CMD_LOAD                          
 cp $56                                 ;   If sum = $56 then its a /SAVE command (not implemented)
 jp z,CMD_SAVE
 cp $52                                 ;   If sum = $52 then its a /SET command (not implemented)
 jp z,CMD_SET
 cp $2A                                 ;   If sum = $2A then its a /IN command
 jp z,CMD_IN
 cp $6A                                 ;   If sum = $6A then its a /OUT command (not implemented)
 jp z,CMD_OUT
 cp $6E                                 ;   If sum = $6E then its a /SWAP command (not implemented)
 jp z,CMD_SWAP
 jp SYNTAX_ERROR                        ;   Else, its an error

; ******************************************
; COMMAND ROUTINES
; ******************************************

CMD_RUN:                                ; /RUN COMMAND
 ld hl,WORKING_MSG                      ;   Send "WORKING" message
 ld de,$0C
 call Serial_SEND

 call $8000                             ;   Call user program via a subroutine - users have to end their program with RETurn command
 jp LOOP_RETURN                         ;   User program terminated, go back to main loop

CMD_LOAD:                               ; /LOAD COMMAND
 ld hl,CANCEL_MSG                       ;   Send "PRESS ANY KEY TO CANCEL" message
 ld de,$18
 call Serial_SEND

 call CharLOAD                          ;   Watch for a character to arrive
 cp $01                                 ;   If a = "Start of Heading" then start receiving
 jr nz,CMD_LOAD_ERROR                   ;   Otherwise send Error

 di                                     ;   Disable interrupts
 ld hl,LOADING_MSG                      ;   Send "LOADING..." message
 ld de,$09
 call Serial_SEND

 ld hl,$8000                            ;   Initiate hl with the start of user program space
 ld b,$00                               ;   Counter to zero
CMD_LOAD_LOOP:                          ;   CMD_LOAD_LOOP : Loading sequence
 call CharLOAD                          ;   Watch for a character to arrive
 ld (hl),a                              ;   store character at address indicated by hl
 inc hl                                 ;   Increment hl (and thus the writing address)
 cp $04                                 ;   Character "End of transmission" needs to be called 8 times to end the loading
 jr nz,CMD_LOAD_LOOP_NEXT               ;   Otherwise jump to CMD_LOAD_NEXT, that reset the counter and loop back to CMD_LOAD_LOOP to load other characters
 inc b                                  ;   If Character = "End of transmission" : increment counter
 ld a,b                                 
 cp $08                                 ;   If counter < 8 
 jr c,CMD_LOAD_LOOP                     ;   Then loop back to CMD_LOAD_LOOP to load other characters
                                        ;   Otherwise the loading sequence is terminated
 ld hl,LOADING_DONE_MSG                 ;   Send "DONE" message
 ld de,$03
 call Serial_SEND
 ei                                     ;   Enable interrupts back
 jp LOOP_RETURN                         ;   Go back to main loop (ACK_LOOP)

CMD_LOAD_LOOP_NEXT:                     ;   CMD_LOAD_NEXT 
 ld b,$00                               ;   Reset the counter and loop back to CMD_LOAD_LOOP to load other characters
 jr CMD_LOAD_LOOP

CMD_LOAD_ERROR:                         ;   CMD_LOAD_ERROR
 ld hl,ERROR_MSG                        ;   Send "ERROR" message
 ld de,$04
 call Serial_SEND
 jp LOOP_RETURN                         ;   Go back to main loop (ACK_LOOP)
 
CMD_SAVE:                               ; /SAVE (not implemented)
 ld a,"S"
 call Char_SEND
 jp LOOP_RETURN
CMD_SET:                                ; /SET (not implemented)
 ld a,"E"
 call Char_SEND
 jp LOOP_RETURN

CMD_IN:                                 ; /IN Command PARTIALLY TESTED !!!!!
 ld hl,KEYBUFFER+4
CMD_IN_SEEK
 inc hl                                 ;   We seek the next byte to write in memory, indicated by ';'. hl is the address in keyboard buffer
 ld a,(hl)
 cp $20                                 ;   If keyboard buffer at hl is 'space', we search further
 jp z,CMD_IN_SEEK
 ;inc hl                                 
 call AsciiToHex                        ;   Convert user input (ASCII string) into a byte
 ld d,a                                 ;   Save result from register a to d
 ld a,b                                 ;   Error status of AsciiToHex is held into register b - so we tranfer it to a for testing
 cp $1                                  ;   If Error=1, ...
 jp z,SYNTAX_ERROR                      ;   ...Then send an error
 ld c,d
 in a,(c)                               ;   Load peripheral data at address indicated by register c in register a
CMD_IN_RETURN:
 call BinToASCII                        ;   Convert read byte to ascii
 ld a," "
 ld (MON_ADDR_CACHE),a                  ;   Insert 'space'
 ld (MON_ADDR_CACHE+1),de
 ld hl,MON_ADDR_CACHE
 ld de,$2
 call Serial_SEND                       ;   Display Byte

 ;ld h,d                                ;   Alternative option
 ;ld l,e 
 ;ld a," "
 ;call Char_SEND
 ;ld a,h 
 ;call Char_SEND
 ;ld a,l 
 ;call Char_SEND

 jp LOOP_RETURN                         ;   Go back to main loop (ACK_LOOP)
CMD_OUT:                                ; /OUT (not implemented)
 ld a,"O"
 call Char_SEND
 jp LOOP_RETURN
CMD_SWAP:                               ; /SWAP (not implemented)
 ld a,"W"
 call Char_SEND
 jp LOOP_RETURN

; MISCELLANEOUS ****************************

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