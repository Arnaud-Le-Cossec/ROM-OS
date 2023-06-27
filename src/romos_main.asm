;*******************************************
;                ROM-OS v1.6.0 - beta
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
;           - /IN implemented but not tested
;       1.4 - complete system rewrite with new memory map
;                       [Snapshot_08_02_22] : System boot, Interrupts and MON_READ
;                       [Snapshot_07_03_22a] : MON_WRITE fixed
;                       [Snapshot_09_04_22a] : Clear RX_BUFFER_COM1 and RX_BUFFER_COM2, avoiding random characters when performing cold startups
;                                                                : xxx1 address bug fixed ! clearing [A] register and thus error bit before return from Interpreter_ADDR
;                                                                : MON_ERASE fixed
;                       [Snapshot_10_04_22a] : MON_COPY Re-implemented
;                       [Snapshot_10_04_22b] : MON_JUMP Re-implemented
;                       [Snapshot_10_04_22c] : CMD_IN Fixed - KEYPOINTER+2 instead of KEYPOINTER+4
;                       [Snapshot_12_04_22a] : SEEK_CHAR created to simplify character research in commands
;                                                                : CMD_OUT implemented
;                       [Snapshot_12_04_22b] : CMD_SWAP implemented
;                       [Snapshot_14_04_22a] : Command interpreting fixed
;                       [Snapshot_14_04_22b] : General optimization
;                                                                : VIA PORTB as output by default
;       1.5 - Serial communication update
;                       [Snapshot_17_04_22a] : Serial transmit auto delay selection added
;                       [Snapshot_18_04_22a] : /COM1 & /COM2 Commands Added - allow user to set the default serial channel
;                                                                : /SET1 & /SET2 Commands Added - allow user to set-up COM1 & COM2
;                       [Snapshot_19_04_22a] : New default serial config : 4800 Bauds; 8bits; No parity
;                       [Snapshot_04_07_22a] : INC_16Bit_MemVal : Increments a 16 bit value in memory, hl=address
;                                            : "> shortcut loops between two pages" bug fixed
;                       [Snapshot_04_07_22b] : /SAVE AAAA,BBBB;C implemented
;                       [Snapshot_24_07_22a] : /LOAD AAAA;C Updated
;                       [Snapshot_09_08_22a] : /LOAD AAAA;C Updated
;       1.5.1 - Release candidate
;                       [Snapshot_27_06/23a] : commenting & ACK_LOOP renamed into MAIN_LOOP                     
;*******************************************
; LABELS ASSIGNATION
;*******************************************
VIA_PORTB      = %10000000  ;$80
VIA_PORTA      = %10000001  ;$81
VIA_DDRB       = %10000010  ;$82
VIA_DDRA       = %10000011  ;$83
VIA_PCR        = %10001101  ;$8D
VIA_IER        = %10001110  ;$8E
 
COM1           = %00001000
COM2           = %00000100
ACIA_RW        = %10010000  ;COM1: $98 | COM2: $94
ACIA_RST_ST    = %10010001  ;COM1: $99 | COM2: $95
ACIA_CMD       = %10010010  ;COM1: $9A | COM2: $96
ACIA_CTR       = %10010011  ;COM1: $9B | COM2: $97
 
 
RAM_AVAILABLE  = $FF00
COM1_SETTINGS  = $FF01 
COM2_SETTINGS  = $FF02 
COM_SELECT     = $FF03
COM1_DELAY     = $FF04
COM2_DELAY     = $FF05
 
KEYBUFFER      = $FF10
STACK          = $FFDF

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
PREVIOUS_COM_CHANNEL = $FFE8
CURRENT_COM_CHANNEL = $FFE9

;*******************************************
; SETUP 
;*******************************************

.org $0002
 di                                     ; Disable interrupts
 im 1                                   ; Set interrupt mode to 1

 ld sp,STACK                            ; Define stack pointer memory start

 ld hl,KEYBUFFER                        ; Initialize KEYPOINTER
 ld (KEYPOINTER),hl 

 ld hl,ACIA_RX_IRQ                      ; Initialize IRQ_VECTOR
 ld (IRQ_VECTOR),hl 



 jp STARTUP                             ; Jump to startup sequence

.org $0035                              ; Loop return anchor point
 jp LOOP_RETURN
 
;*******************************************
; INTERRUPT
;*******************************************
.org $0038 ; IRQ
 di                                     ; Disable interrupts
 EXX                                    ; Save registers
 ld hl,(IRQ_VECTOR)                     ; Load HL with the IRQ Vector (Address of the IRQ handler)
 jp (hl)                                ; Jump tp the IRQ handler
 

ACIA_RX_IRQ:                            ; IRQ handler for ACIA
 in a,(ACIA_RST_ST | COM1)              ;       Check if COM1 generated an interrupt 
 AND %10000000
 jr nz, IRQ_Return                      ;       If not, check COM2
 in a,(ACIA_RW | COM1)                  ;       If so, save RX register into RX buffer
 ld (RX_BUFFER_COM1),a
;ACIA_RX_IRQ_Next:
; in a,(ACIA_RST_ST | COM2)             ;       Check if COM2 generated an interrupt 
; AND %10000000
; jr nz, IRQ_Return                     ;       If not, Return from interrupt
; in a,(ACIA_RW | COM2)                 ;       If so, save RX register into RX buffer
; ld (RX_BUFFER_COM2),a

IRQ_Return:                                                                                     
 EXX                                    ; Retrieve registers
 ei                                     ; Enable interrupts back
 reti                                   ; Retrun from interrupt

;*******************************************
; STARTUP
;*******************************************

STARTUP:
                                        ; Initialize ACIA - COM1
 out (ACIA_RST_ST | COM1 ),a            ;       soft reset (value not important)
 ld a,%01101001                         ;       disable parity, echo mode , enable RX interrupt and enable reciver/transmiter
 out (ACIA_CMD | COM1 ),a
 ld a,%00011100                         ;       4800 bauds; 8 data bits; no parity; 1 stop bit
 ld (COM1_SETTINGS),a                   ;       Save COM1 Settings
 out (ACIA_CTR | COM1),a 
                                        ; Initialize ACIA - COM2
 out (ACIA_RST_ST | COM2 ),a            ;       soft reset (value not important)
 ld a,%01101001                         ;       disable parity, echo mode , enable RX interrupt and enable reciver/transmiter
 out (ACIA_CMD | COM2 ),a
 ld a,%00011100                         ;       4800 bauds; 8 data bits; no parity; 1 stop bit
 ld (COM2_SETTINGS),a                   ;       Save COM2 Settings  
 out (ACIA_CTR | COM2),a
 
 ld a,COM1                              ;   Make COM1 the default channel
 ld (COM_SELECT),a

 ld a,$FF                               ; Set VIA PORTB as output by default
 out (VIA_DDRB),a

 ld hl,$8000                            ; Perform memory verification from $8000 to $7EFF (User RAM)
 ld de,$7EFF                            ; Also clears selected RAM
 call MemCheck
 
 ld a,$C9                               ; Emergency RETurn at the end of USER RAM space, in case the user forgot to put one
 ld ($FEFF),a

 ld a,$00                               ; Clear serial input buffers
 ld (RX_BUFFER_COM1),a
 ld (RX_BUFFER_COM2),a

 ld a,$0C                               ; Clear terminal
 call Char_SEND

 ld hl,MSG                              ; send startup message
 ld de,$6D
 call Serial_SEND

 ei
 jp LOOP_RETURN


; MAIN LOOP ********************************

MAIN_LOOP:                              ; MAIN LOOP
                                        ;   if RX_BUFFER == $0 , then loop back, otherwise, continue
 ld a,(RX_BUFFER_COM1)
 cp $0
 jr z,MAIN_LOOP 
                                        ;   So there is data in RX_BUFFER, let's clear it ! (while saving the input)

 ld b,a
 ld a,$0
 ld (RX_BUFFER_COM1),a
 ld a,b
                                        ;   Check data

 cp $7F                                 ;   'DEL'
 jr z,KEY_DELETE

 cp $20                                 ;   Regular ASCII char > $20
 jr nc,KEY_ADD                                                  

 cp $08                                 ;   Backspace
 jr z,KEY_DELETE

 cp $0D                                 ;   Carriage Return
 jr z,KEY_ENTER

 jr MAIN_LOOP                            ;   If the character is not recognized, it is ignored and we loop again


KEY_ENTER:                              ; KEY_ENTER : Carriage Return has been entered meaning the line is complete and we scan the type of the input 
 ld a,(KEYBUFFER)                       ;   scan the first character in the keyboard bufffer
 cp ">"                                 ;   ">" : command for MONITOR
 jp z,MONITOR
 cp "<"                                 ;   "<" : command for MONITOR-READ BACKWARD
 jp z,MON_READ_BACKWARD
 cp "/"                                 ;   "/" : command for command...wait, what ?
 jp z,INTERPRETER_CMD
 cp "0"                                 ;   "0" : HOME (Soft Reset)
 jp z,$0000
 cp "#"                                 ;   "#" : shotcut for '/RUN'
 jp z,CMD_RUN
 jp SYNTAX_ERROR                        ;   If there is no match, display an error


KEY_DELETE:                             ; KEY_DELETE : delete the current character
 ld hl,(KEYPOINTER)                     ;   Load hl with the address of the latest character in the keyboard buffer
 ld a,l                                 ;   Load register l to a for further operations
 cp ((KEYBUFFER+1) & $FF)               ;   If l < $11 we can't delete more
 jp c,MAIN_LOOP                          ;   So we ignore the deleting and we go back to the main loop
 ld (hl),$0                             ;   Otherwise, we erase what was at KEYPOINTER
 dec hl
 ld (KEYPOINTER),hl
 ld a,$08                               ;   Move the cursor back
 call Char_SEND
 ld a,$20                               ;   Erasing the character by replacing it with a space
 call Char_SEND
 ld a,$08                               ;   Move the cursor back again
 call Char_SEND
 jr MAIN_LOOP                            ;   Erasing completed : Go back to the main loop

KEY_ADD:                                ; KEY_ADD : Adds a character in the keyboard bufferinc KEYPOINTER and add character
 ld b,a                                 ;   Save a (which contains the character that we want to add)
 ld hl,(KEYPOINTER)                     ;   Load KEYPOINTER, the address of the last added character in the buffer
 ld a,l                                 ;   Check if there is room in the buffer for a new charater
 cp ((KEYBUFFER+$50) & $FF) 
 jp nc,MAIN_LOOP                         ;   If l >= $5F we can't add more : go back to the main loop
 ld a,b                                 ;   Otherwise retrieve a
 ld (hl),a                              ;   Load the character in the buffer
 inc hl                                 ;   Increment the address of the last added character
 ld (KEYPOINTER),hl                     ;   Save it to KEYPOINTER
 call Char_SEND                         ;   Don't forget to display the character !
 jr MAIN_LOOP                            ;   Adding completed : Go back to the main loop


SYNTAX_ERROR:                           ; SYNTAX_ERROR : handles the typing errors of the user
 ld hl,SYNTAX_ERROR_MSG                 ;   Send error message
 ld de,$0D
 call Serial_SEND

LOOP_RETURN:                            ; LOOP_RETURN : Restoration point for programs and commands to re-enter the MAIN_LOOP
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

 jp MAIN_LOOP                            ;   Go back to the main loop

; APPS *************************************

; ******************************************
; MONITOR v1.2
; ******************************************
; Commands : (AAAA = address, DD = data) 
; Read memory : >rAAAA 
; Write memory : >wAAAA;DD;DD;DD ...
; Erase memory : >eAAAA,AAAA
; Copy memory : >cAAAA,AAAA;AAAA
; Jump to address ; >jAAAA

MONITOR:                                ; MONITOR : Monitor command identifiyer
 ld a,(KEYBUFFER+1)                     ;   Read keybuffer 2nd character (the first being ">")
 cp "r"                                 ;   "r" : MONITOR READ
 jr z,MON_READ
 cp "w"                                 ;   "w" : MONITOR WRITE
 jp z,MON_WRITE
 cp "e"                                 ;   "e" : MONITOR ERASE 
 jp z,MON_ERASE
 cp "c"                                 ;   "c" : MONITOR COPY
 jp z,MON_COPY
 cp "j"                                 ;   "j" : MONITOR JUMP & EXECUTE
 jp z,MON_JUMP
 cp $0                                  ;   null : READ FORWARD
 jp MON_READ_INIT
 jp SYNTAX_ERROR 


; MONITOR READ ****
MON_READ_BACKWARD:                      ; MON_READ_BACKWARD : shortcut "<" to setup the monitor to read the previous page
 ld de,$0100 
 ld hl,(MON_ADDR_CACHE)
 sbc hl,de                              ;   Substract the previous address used by the monitor by $100 (two pages)
 ld (MON_ADDR_CACHE),hl
 jr MON_READ_INIT

MON_READ:                               ; MON_READ : decodes the command's argument
 ld hl,KEYBUFFER+1
 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE),de                 ;   Store decoded address in RAM

MON_READ_INIT:
 ld a,$0
 ld (MON_LINE_CACHE),a

MON_READ_LOOP:
 ld a,(MON_LINE_CACHE)
 cp $10
 jr nz,MON_READ_LINE
 jp LOOP_RETURN


MON_READ_LINE:
 call PRINT_CR_LF                       ; New line : Carriage return, Line feed

;[DISPLAY START ADDRESS]
 ld hl,(MON_ADDR_CACHE)                 ;   Convert the most significant byte it to hexadecimal (ASCII format)
 push hl                                ;   Save reading address for later
 ld a,h
 call BinToASCII
 ld (MON_PRINT_BUFFER),de

 ld hl,(MON_ADDR_CACHE)                 ;   Convert the least significant byte it to hexadecimal (ASCII format)
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
 ld hl,(MON_ADDR_CACHE)                 ;   Load reading address
 ld a,(hl)
 
 call BinToASCII                        ;   Convert read byte to ascii
 ld (MON_PRINT_BUFFER),de
 ld hl,MON_PRINT_BUFFER                 ;   Display it
 ld de,$1
 call Serial_SEND  
 
 ld hl,MON_ADDR_CACHE
 call INC_16Bit_MemVal                  ; Increment reading address
 djnz MON_READ_LINE_LOOP_1              ; Decrement b, if b=0 then continue, otherwise loop again

;[DISPLAY ASCII]                        Ascii representation of the bytes read
 ld a,$20                               ;   send "space"
 call Char_SEND
 pop hl                                 ;  Retreive first reading address
 ld (MON_ADDR_CACHE),hl
 ld b,$08
MON_READ_LINE_LOOP_2:
 ld hl,(MON_ADDR_CACHE)                 ;   Load reading address
 ld a,(hl)

 cp $7F                                 ;   If Byte >= $7F
 jr nc,MON_READ_LINE_3                  ;   its not a character and therefore we skip it
 cp $20                                 ;   If Byte >= $20
 jr nc,MON_READ_LINE_4                  ;   its a character

MON_READ_LINE_3:
 ld a,"."
MON_READ_LINE_4:
 call Char_SEND
 
 ld hl,MON_ADDR_CACHE
 call INC_16Bit_MemVal                  ; Increment reading address
 djnz MON_READ_LINE_LOOP_2              ; Decrement b, if b=0 then continue, otherwise loop again

;[LINE END]
 ld hl,MON_LINE_CACHE
 inc (hl)
 jr MON_READ_LOOP


; MONITOR WRITE ****
MON_WRITE:                              ; MON_WRITE : write bytes in memory : ">wAAAA;DD;DD;DD;DD..." (AAAA=address, DD=Data)
 ld hl,KEYBUFFER+1                      ;   Decoding the command's address argument
MON_WRITE_1:
 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR

 ld (MON_ADDR_CACHE),de
MON_WRITE_2:                            ;   Writing sequence loop  
 call SEEK_CHAR                         ;   We seek the next byte to write in memory, indicated by ';'. hl is the address in keyboard buffer
 cp ";"                                 ;   Else if keyboard buffer at hl is different than ';', go back to main loop (MAIN_LOOP)
 jp nz,LOOP_RETURN
 
 inc hl                                 ;   But if so, it means that a new byte has to be written
 call AsciiToHex                        ;   Convert user input (ASCII string) into a byte
 bit 0,b                                ;   If Error(b)=1, ...
 jp nz,SYNTAX_ERROR                     ;   ...Then send an error

 ld bc,(MON_ADDR_CACHE)                 ;   And load the address at which we want to write
 ld (bc),a                              ;   Write byte at that address

 push hl
 ld hl,MON_ADDR_CACHE                   ;   Save it
 inc (hl)                               ;   Increment the address
 pop hl
 jr MON_WRITE_2                         ;   Loop back to check for next bytes

; MONITOR ERASE ****
MON_ERASE:                              ; MON_ERASE : erases an area of memory : ">eSSSS,EEEE" (SSSS=Start address, EEEE=End address)
 ld hl,KEYBUFFER+1                      ;   Decoding the command's address argument
 call Interpreter_ADDR 
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE),de
                                                                                ;   interpreting the end address
 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further
 cp ","
 jp nz,SYNTAX_ERROR                     ;   Else if keyboard buffer at hl is different than ',', display error

 call Interpreter_ADDR                  ;   Otherwise run address interpreter once more. the end address is stored in de
 cp $1                                  ;   Error held in register a.
 jp z,SYNTAX_ERROR
                
MON_ERASE_LOOP:                         ;   Erasing loop
 ld hl,(MON_ADDR_CACHE)                  
 ld (hl),$0                             ;   Set data at address pointed by hl at 0
 inc hl                                 ;   Increment hl
 ld (MON_ADDR_CACHE),hl
 sbc hl,de                              ;   Compare hl with de (end address). if hl < de then continue...
 jr nz,MON_ERASE_LOOP
 jp LOOP_RETURN                         ;   Else go back to main loop (MAIN_LOOP)


; MONITOR COPY ****
MON_COPY:                               ; MON_COPY : Copies a block of data to another place in memory : ">cSSSS,EEEE;NNNN" (SSSS=Start address, EEEE=End address, NNNN=New address)
 ld hl,KEYBUFFER+1                      ;   -> Decoding the command's Start address
 call Interpreter_ADDR 
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE),de                 ;   Save Start address
                                                                                ;   -> Decoding the command's End address
 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further
 cp ","                                 ;   Else if keyboard buffer at hl is different than ',', display error
 jp nz,SYNTAX_ERROR                     

 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE+2),de               ;   Save End address
                                                                                ;   -> Decoding the command's New address
 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further
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
 inc bc
 ld hl,(MON_ADDR_CACHE)                 ;   hl = Start
 ld de,(MON_ADDR_CACHE+4)               ;   de = New

 ldir                                   ;   The magic command...
 jp LOOP_RETURN                         ;   Go back main Loop (MAIN_LOOP)


; MONITOR COPY ****
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
 jr z,INTERPRETER_CMD_2                 
 cp $20
 jr z,INTERPRETER_CMD_2
 AND %11011111                          ;   Make sure we are dealing with upper caps by applying a mask
 sub $41                                ;   Bring ascii character to zero : A=0, B=1, C=2 ...
 add a,c                                ;   Add new character to the character sum
 rlca                                   ;   a*2
 ld c,a                                 ;   save the sum to register c
 inc hl                                 ;   Increment hl 
 jr INTERPRETER_CMD_1                   ;   Loop back

INTERPRETER_CMD_2:                      ;   Character Selection
 ld a,c                                 ;   Load the sum from register c
 cp $F2                                 ;   If sum = $F2 then its a /RUN command
 jr z,CMD_RUN
 cp $27                                 ;   If sum = $27 then its a /LOAD command
 jr z,CMD_LOAD                          
 cp $7D                                 ;   If sum = $7D then its a /SAVE command (not implemented)
 jp z,CMD_SAVE
 cp $2D                                 ;   If sum = $2D then its a /SET1 command
 jp z,CMD_SET1
 cp $2F                                 ;   If sum = $2F then its a /SET2 command
 jp z,CMD_SET1
 cp $60                                 ;   If sum = $60 then its a /COM1 command
 jp z,CMD_COM1
 cp $62                                 ;   If sum = $62 then its a /COM2 command
 jp z,CMD_COM1
 cp $3A                                 ;   If sum = $3A then its a /IN command
 jp z,CMD_IN
 cp $E6                                 ;   If sum = $E6 then its a /OUT command
 jp z,CMD_OUT
 cp $58                                 ;   If sum = $58 then its a /SWAP command
 jp z,CMD_BANK
 jp SYNTAX_ERROR                        ;   Else, its an error

; ******************************************
; COMMAND ROUTINES
; ******************************************

 CMD_RUN:                               ; /RUN COMMAND
 ld hl,WORKING_MSG                      ;   Send "WORKING" message
 ld de,$0C
 call Serial_SEND

 call $8000                             ;   Call user program via a subroutine - users have to end their program with RETurn command
 jp LOOP_RETURN                         ;   User program terminated, go back to main loop

CMD_LOAD:                               ; /LOAD AAAA;C COMMAND
 ld hl,KEYBUFFER+4
 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE),de                 ;   Save New address

 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further
 cp ";"                                 ;   Else if keyboard buffer at hl is different than ';', display error
 jp nz,SYNTAX_ERROR   

 call SaveSerialChannel                 ;   Save COM channel

 call ChangeSerialChannel               ;   Prepare serial COM channel change
 cp $FF
 jp z,SYNTAX_ERROR 
 ld b,a                                 ;   New channel saved in register b
 
 ld hl,CANCEL_MSG                       ;   Send "PRESS ANY KEY TO CANCEL" message
 ld de,$1A
 call Serial_SEND

 call CharLOAD                          ;   Watch for a character to arrive
 cp $01                                 ;   If a = "Start of Heading" then start receiving
 jr nz,CMD_LOAD_ERROR                   ;   Otherwise send Error

 di                                     ;   Disable interrupts
 ld hl,LOADING_MSG                      ;   Send "LOADING..." message
 ld de,$09
 call Serial_SEND

 ld a,b                                 ;   Retrieve new COM channel stored in register b
 ld (COM_SELECT),a                      ;   Apply new COM channel

 ld hl,(MON_ADDR_CACHE)                 ;   Initiate hl with the start of user program space
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
 inc b
 ld a,0            
CMD_LOAD_END:                           ;   Erase "End of transmission" Characters
 ld (hl),a 
 dec hl 
 djnz CMD_LOAD_END
 call RetrieveSerialChannel             ;   Restore previous COM channel
 ld hl,LOADING_DONE_MSG                 ;   Send "DONE" message
 ld de,$03
 call Serial_SEND
 ei                                     ;   Enable interrupts back
 jp LOOP_RETURN                         ;   Go back to main loop (MAIN_LOOP)

CMD_LOAD_LOOP_NEXT:                     ;   CMD_LOAD_NEXT 
 ld b,$00                               ;   Reset the counter and loop back to CMD_LOAD_LOOP to load other characters
 jr CMD_LOAD_LOOP

CMD_LOAD_ERROR:                         ;   CMD_LOAD_ERROR
 ld hl,ERROR_MSG                        ;   Send "ERROR" message
 ld de,$04
 call Serial_SEND
 jp LOOP_RETURN                         ;   Go back to main loop (MAIN_LOOP)
 
CMD_SAVE:                               ; /SAVE AAAA,BBBB,C      (AAAA: Start Address, BBBB: End Address, C: COM Channel)
 ld hl,KEYBUFFER+4
 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE),de                 ;   Save New address
 
 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further
 cp ","                                 ;   Else if keyboard buffer at hl is different than ',', display error
 jp nz,SYNTAX_ERROR                     

 call Interpreter_ADDR
 cp $1
 jp z,SYNTAX_ERROR
 ld (MON_ADDR_CACHE+2),de               ;   Save End address

 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further
 cp ";"                                 ;   Else if keyboard buffer at hl is different than ';', display error
 jp nz,SYNTAX_ERROR   

 call SaveSerialChannel                 ;   Save COM channel

 call ChangeSerialChannel
 cp $FF
 jp z,SYNTAX_ERROR 
 ld (COM_SELECT),a

;   Send Transmission Header ****
 ld a,$01                               
 call Char_SEND
;   Send Data *******************
CMD_SAVE_LOOP:
 ld hl,(MON_ADDR_CACHE)                 ;   Retrieve Start address in hl
 ld a,(hl)
 call Char_SEND
 inc hl
 ld (MON_ADDR_CACHE),hl
 ld de,(MON_ADDR_CACHE+2)               ;   Retrieve Start address in de
 sbc hl,de                              ;   Compare hl with de (end address). if hl < de then continue...
 jr nz,CMD_SAVE_LOOP
;   Send Transmission End *******
 ld b,$08
CMD_SAVE_END_LOOP:
 ld a,$04
 push bc
 call Char_SEND
 pop bc
 djnz CMD_SAVE_END_LOOP 

 call RetrieveSerialChannel            ;   Restore previous COM channel
 
 jp LOOP_RETURN



CMD_SET:                                ; /SET1 & /SET2 Commands
 ld hl,KEYBUFFER+4
 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further                                 
 call AsciiToHex                        ;   Convert user input (ASCII string) into a byte
 bit 0,b                                ;   Error status of AsciiToHex is held into register b - so we tranfer it to a for testing
 jp nz,SYNTAX_ERROR                     ;   ...Then send an error
 ret

CMD_SET1:
 call CMD_SET
 out (ACIA_CTR | COM1),a
 ld (COM1_Settings),a
 jp LOOP_RETURN

CMD_SET2:
 call CMD_SET
 out (ACIA_CTR | COM2),a
 ld (COM2_Settings),a
 jp LOOP_RETURN

CMD_COM1:                               ; /COM1
 ld a,COM1                              ; Make COM1 the default channel
 ld (COM_SELECT),a
 jp LOOP_RETURN

CMD_COM2:                               ; /COM2
 ld a,COM2                              ; Make COM2 the default channel
 ld (COM_SELECT),a
 jp LOOP_RETURN

CMD_IN:                                 ; /IN Command
 ld hl,KEYBUFFER+2
 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further                                 
 call AsciiToHex                        ;   Convert user input (ASCII string) into a byte
 bit 0,b                                ;   Error status of AsciiToHex is held into register b - so we tranfer it to a for testing
 jp nz,SYNTAX_ERROR                     ;   ...Then send an error
 ld c,a                                 ;   Save result from register a to c
 in a,(c)                               ;   Load peripheral data at address indicated by register c in register a
 call BinToASCII                        ;   Convert read byte to ascii
 ld a," "
 ld (MON_ADDR_CACHE),a                  ;   Insert 'space'
 ld (MON_ADDR_CACHE+1),de
 ld hl,MON_ADDR_CACHE
 ld de,$2
 call Serial_SEND                       ;   Display Byte
 jp LOOP_RETURN                         ;   Go back to main loop (MAIN_LOOP)


CMD_OUT:                                ; /OUT
 ld hl,KEYBUFFER+3

 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further                                
 call AsciiToHex
 ld e,a                                 ;   Save result from register a to e
 bit 0,b                                ;   Error status of AsciiToHex is held into register b - so we tranfer it to a for testing
 jp nz,SYNTAX_ERROR                     ;   ...Then send an error

 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further                       
 cp ";"                                 ;   Else if keyboard buffer at hl is different than ';', go back to main loop (MAIN_LOOP)
 jp nz,SYNTAX_ERROR
 inc hl                                 ;   But if so, it means that a new byte has to be written
 call AsciiToHex                        ;   Convert user input (ASCII string) into a byte
 bit 0,b                                ;   If Error(b)=1, ...
 jp nz,SYNTAX_ERROR                     ;   ...Then send an error
 
 ld c,e
 out (c),a
 jp LOOP_RETURN


CMD_BANK:                               ; /BANK ( ISSUE II specific)
 ld hl,KEYBUFFER+4
 call SEEK_CHAR                         ;   If keyboard buffer at hl is 'space', we search further                                 
 call AsciiToHex                        ;   Convert user input (ASCII string) into a byte
 bit 0,b                                ;   Error status of AsciiToHex is held into register b - so we tranfer it to a for testing
 jp nz,SYNTAX_ERROR                     ;   ...Then send an error
 out (VIA_PORTB),a                      ;   Send register a to VIA_PORTB (Bank selection)
 jp LOOP_RETURN

; MISCELLANEOUS ****************************

PRINT_CR_LF:
 ld a,$0D                               ;       PRINT_CR_LF Carriage return 
 call Char_SEND
 ld a,$0A                               ;   Line feed and 
 call Char_SEND
 ret

SEEK_CHAR:                              ;  SEEK_CHAR If keyboard buffer at hl is 'space', we search further  
 inc hl
 ld a,(hl)
 cp $20
 jr z,SEEK_CHAR
 ret

INC_16Bit_MemVal:                       ;   hl : address of the 16 bit value that will be incremented
 ld e,(hl)
 inc hl 
 ld d,(hl)
 inc de 
 ld (hl),d 
 dec hl 
 ld (hl),e 
 ret

SaveSerialChannel:                      ;   SaveSerialChannel : Stores current serial channel
 ld a,(COM_SELECT)
 ld (PREVIOUS_COM_CHANNEL),a
 ret

RetrieveSerialChannel:                  ;   RetrieveSerialChannel : Restore previous serial channel
 ld a,(PREVIOUS_COM_CHANNEL)
 ld (COM_SELECT),a
 ret

ChangeSerialChannel:                    ;   Interprets user input to prepare a serial channel change. a(output) = New COM channel
 inc hl 
 ld a,(hl)
 cp '1'
 jr z,ChangeSerialChannel_1
 cp '2'
 jr z,ChangeSerialChannel_2
 ld a,$FF           ; a = 0 : SYNTAX_ERROR
 ret
ChangeSerialChannel_1:
 ld a,COM1                              ;   Change COM channel for COM1
 ret
ChangeSerialChannel_2:
 ld a,COM2                              ;   Change COM channel for COM2
 ret

; Decodes an address written in ASCII
Interpreter_ADDR:                       ; Interpreter_ADDR  de = address output / if a=1 then an error has occured
 inc hl                                 ;   Inc hl
 ld a,(hl)                              ;   Load content of address of KEYBUFFER pointed by hl into a
 cp $20                                 ;   If that value is 'space'
 jr z,Interpreter_ADDR                  ;   Then loop again until we hit a character
 cp $0                                  ;   Otherwise if character = 0; send error
 jr z,Interpreter_ADDR_Error
 call AsciiToHex                        ;   Convert the most significant Byte from ascii to 'regular' binary
 bit 0,b                                ;   If b = 1, Send an error
 jr nz,Interpreter_ADDR_Error
 ld d,a                                 ;   Output
 inc hl                                 ;   Otherwise we continue
 call AsciiToHex                        ;   Convert the least significant Byte from ascii to 'regular' binary
 bit 0,b                                ;   If b = 1, Send an error
 jr nz,Interpreter_ADDR_Error
 ld e,a                                 ;   Output
 ld a,$0                                                                ;       clear error bit
 ret                                    ;   Return from subroutine
Interpreter_ADDR_Error:                 ;   Error handeling
 ld a,$1                                ;   If an error occured, register it register a
 ret



; TEXT DATA *****************************
WORKING_MSG:
 .db " WORKING..."
CANCEL_MSG:
 .db $0D ; carriage return
 .db $0A ; line feed
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
 .db "ROM-OS v1.5.1 (c)2022 LE COSSEC Arnaud"
 .db $0D ; carriage return
 .db $0A ; line feed
 .db "32,511 BYTES FREE [Snapshot 27/06/23a]"
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
 ;AND %00010000                         ;   when using the original 6551 chip (not WDC W65C51 that have bugs and thus require the delay)
 ;jp z,Serial_SEND

 call Serial_Delay                                              ;   Set delay between characters sent (1)

 ld a,(COM_SELECT)                                              ;       Select Port ( ACIA_RW | COM_SELECT )
 OR ACIA_RW
 ld c,a
 ld a,(hl)                              ;   Load data from address pointed by hl

 out (c),a                              ;   Output data to the 6551's transmit register
 
 inc hl                                 ;   Prepare next byte by incrementing hl  
 dec de                                 ;   Decrement de (length)
 ld a,d                                 ;   Check de > 0
 cp $FF   
 jr nz,Serial_SEND                      ;   If so, continue and loop back
 ld a,e   
 cp $FF   
 jr nz,Serial_SEND                      ;       "                   "
 ret 



.org $2080                              ; CHAR SEND
;*****************THIS SUBROUTINE WORKS !!!!
; -> Sends a single byte to selected serial port
Char_SEND:
 push af                                ;       Save a
Char_1:
 ;in a,(ACIA_RST_ST)                    ;   To use instead of delay (remove line featuring (1))
 ;AND %00010000                         ;   When using the original 6551 chip (not WDC W65C51 that have bugs and thus require the delay)
 ;jp z,Char_1

 call Serial_Delay                      ;   Set delay between characters sent (1)

 ld a,(COM_SELECT)                      ;       Select Port ( ACIA_RW | COM_SELECT )
 OR ACIA_RW
 ld c,a

 pop af                                 ;       Retrieve a
 out (c),a
 ret


Serial_Delay:
 push hl
 push de
 ld a,(COM_SELECT)
 cp $08
 ld hl,COM1_Settings
 jr z,Serial_Delay_COM1
 inc hl
Serial_Delay_COM1:
 ld a,(hl)
 AND $0F
 rlca   ; a*2
 ld hl,BAUD_DELAY_TABLE
 add a,l
 ld l,a

 ld d,(hl)
 inc hl
 ld e,(hl)

 call Delay
 pop de
 pop hl
 ret

BAUD_DELAY_TABLE:
 .db $00
 .db $00 ; NULL
 .db $00
 .db $00 ; 50 bauds     (Not valid)
 .db $00
 .db $00 ; 75 bauds     (Not valid)
 .db $00
 .db $00 ; 109.92 bauds (Not valid)
 .db $00
 .db $00 ; 134.58 bauds (Not valid)
 .db $00
 .db $00 ; 150 bauds    (Not valid)
 .db $04
 .db $18 ; 300 bauds
 .db $02
 .db $08 ; 600 bauds
 .db $01
 .db $00 ; 1200 bauds
 .db $00
 .db $B0 ; 1800 bauds
 .db $00
 .db $80 ; 2400 bauds
 .db $00
 .db $52 ; 3600 bauds
 .db $00
 .db $40 ; 4800 bauds
 .db $00
 .db $28 ; 7200 bauds
 .db $00
 .db $1B ; 9600 bauds
 .db $00
 .db $0A ; 19,200 bauds


.org $2100
;*****************THIS SUBROUTINE WORKS !!!!
CharLOAD: ; will watch for a char to be recived
 ld a,(COM_SELECT)                      ;       Select Port ( ACIA_RST_ST | COM_SELECT )
 OR ACIA_RST_ST
 ld c,a
CharLOAD_LOOP:
 in a,(c)
 AND %00001000                          ;       If recive flag is not 0 (data recived), Loop again
 jr z,CharLOAD

 ld a,(COM_SELECT)                      ;       Select Port ( ACIA_RW | COM_SELECT )
 OR ACIA_RW
 ld c,a

 in a,(c)                               ;       Recived Char into register a
 ret




.org $2180
;*****************THIS SUBROUTINE WORKS !!!!
MemCheck: ; hl = start address / de = lenght of the test
 ld (hl),%11111111                                      ; fill RAM
 nop
 ld a,(hl)                              ; and check if the data is still there
 cp $FF
 jp nz,RAM_ERROR
 ld (hl),$0                             ; clear bytes checked
 inc hl

 dec de
 ld a,d
 cp $FF
 jr nz,MemCheck
 ld a,e
 cp $FF
 jr nz,MemCheck

 ret

RAM_ERROR:
 ld hl,RAM_ERROR_MSG                    ; Send error message
 ld e,$09
RAM_ERROR_1:
 ld bc, $0120
RAM_ERROR_2:
 dec bc             ; T=6 , 1.50
 ld a,b             ; T=4 , 1.00
 cp $FF             ; T=7 , 1.75
 jr nz,RAM_ERROR_2  ; T=10 , 2.50
 ld a,c             ; T=4 , 1.00
 cp $FF             ; T=7 , 1.75
 jr nz,RAM_ERROR_2  ; T=10 , 2.50

 ld a,(hl)
 out (ACIA_RW | COM1),a
 
 inc hl   
 dec e   
 ld a,e   
 cp $FF   
 jr nz,RAM_ERROR_1
 ld a,%10101010
 out (VIA_PORTB),a
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

 call BinToASCII_Search ;-> look at the first nibble 
 ld d,(hl)

 pop af                                 ;-> look at the second nibble 
 rra            
 rra
 rra
 rra
 
 call BinToASCII_Search
 ld e,(hl)

 ret

BinToASCII_Search:
 AND $0F  
 ld hl,HEX_TABLE
 add a,l
 ld l,a
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
 jr c,AsciiToHex_Error
 cp $3A
 jr nc,AsciiToHex_2
 sub $30
 ret
AsciiToHex_2:
 AND %11011111
 cp $41
 jr c,AsciiToHex_Error
 cp $47
 jr nc,AsciiToHex_Error
 sub $37
 ret
AsciiToHex_Error:
 ld b,$1
 ret