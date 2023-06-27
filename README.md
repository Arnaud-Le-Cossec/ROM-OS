# ZEPHYR-COMPUTER-SYSTEM-ROM-OS

ROM-OS is the operating system for the DX82 computer. In its current form, it offers a terminal-based interface with a memory monitor (read, write, copy, erase & jump commands) and some commands (/run, /load, /save ...).

## FEATURE ROAD MAP

ROM-OS 1.6 - STARTUP & DIAGNOSTIC UPDATE

[ ] [ADD] Cold/Warm Boots

[ ] [UPDATE] RAM Count : include RAM extension

ROM-OS 1.5 - Serial Update

[x] [UPDATE] Serial program loading

[x] [ADD] Serial program saving

[x] [ADD] /COM1 & /COM2 to switch the default channel

[x] [ADD] Auto delay selection for serial transmit

[x] [ADD] /SET1 & /SET2 for channel settings

[x] [UPDATE] New defalut config : 4800bauds; 8bits ; No parity

[x] [FIX] Serial loading

[x] [ADD] Parity Emulation

[ ] [FIX] /COM2 RX interrupt bug

## UPDATE LOG

1.0 - Monitor finished

1.1 - Command system introduction

1.2 - Commands implementation : /RUN /LOAD /SAVE (none works)

1.3 - implement ">j xxxx" -> "jump" command for the monitor

          - /RUN and /LOAD fixed

	      - /IN implemented but not tested

1.4 - complete system rewrite with new memory map

	[Snapshot_08_02_22]  : System boot, Interrupts and MON_READ

	[Snapshot_07_03_22a] : MON_WRITE fixed

	[Snapshot_09_04_22a] : Clear RX_BUFFER_COM1 and RX_BUFFER_COM2, avoiding random characters when performing cold startups

						           : xxx1 address bug fixed ! clearing [A] register and thus error bit before return from Interpreter_ADDR

						           : MON_ERASE fixed

	[Snapshot_10_04_22a] : MON_COPY Re-implemented

	[Snapshot_10_04_22b] : MON_JUMP Re-implemented

	[Snapshot_10_04_22c] : CMD_IN Fixed - KEYPOINTER+2 instead of KEYPOINTER+4

	[Snapshot_12_04_22a] : SEEK_CHAR created to simplify character research in commands

					           	 : CMD_OUT implemented

	[Snapshot_12_04_22b] : CMD_SWAP implemented

	[Snapshot_14_04_22a] : Command interpreting fixed

	[Snapshot_14_04_22b] : General optimization

						           : VIA PORTB as output by default

1.5 - Serial communication update

	[Snapshot_17_04_22a] : Serial transmit auto delay selection added

	[Snapshot_18_04_22a] : /COM1 & /COM2 Commands Added - allow user to set the default serial channel

						           : /SET1 & /SET2 Commands Added - allow user to set-up COM1 & COM2

	[Snapshot_19_04_22a] : New default serial config : 4800 Bauds; 8bits; No parity

