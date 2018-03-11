;;;; gb-emulator.lisp

(in-package #:gb-emulator)

;;; Tech Specs
;; CPU: 8-bit (Similar to the Z80 processor.)
;; Main RAM: 8K Byte
;; Video RAM: 8K Byte
;; Screen Size 2.6
;; Resolution: 160x144 (20x18 tiles)
;; Max # of sprites: 40
;; Max # sprites/line: 10
;; Max sprite size: 8x16
;; Min sprite size: 8x8
;; Clock Speed: 4.194304 MHz (4.295454 SGB, 4.194/8.388MHz GBC)
;; Horiz Sync: 9198 KHz (9420 KHz for SGB)
;; Vert Sync: 59.73 Hz (61.17 Hz for SGB)
;; Sound: 4 channels with stereo sound

;;; Memory Map

;;Interrupt Enable Register
;;--------------------------- FFFF
;;Internal RAM
;;--------------------------- FF80
;;Empty but unusable for I/O
;;--------------------------- FF4C
;;I/O ports
;;--------------------------- FF00
;;Empty but unusable for I/O
;;--------------------------- FEA0
;;Sprite Attrib Memory (OAM)
;;--------------------------- FE00
;;Echo of 8kB Internal RAM
;;--------------------------- E000
;;8kB Internal RAM
;;--------------------------- C000
;;8kB switchable RAM bank
;;--------------------------- A000
;;8kB Video RAM
;;--------------------------- 8000 ---
;;16kB switchable ROM bank           |
;; --------------------------- 4000  |= 32kB Cartridge
;;16kB ROM bank #0                   |
;; --------------------------- 0000 --

;; NOTE: b=bit B=byte

;;; Reserved Memory Locations
;; 0000 Restart $00 Address (RST $00 calls this address.)
;; 0008 Restart $08 Address (RST $08 calls this address.)
;; 0010 Restart $10 Address (RST $10 calls this address.)
;; 0018 Restart $18 Address (RST $18 calls this address.)
;; 0020 Restart $20 Address (RST $20 calls this address.)
;; 0028 Restart $28 Address (RST $28 calls this address.)
;; 0030 Restart $30 Address (RST $30 calls this address.)
;; 0038 Restart $38 Address (RST $38 calls this address.)
;; 0040 Vertical Blank Interrupt Start Address
;; 0048 LCDC Status Interrupt Start Address
;; 0050 Timer Overflow Interrupt Start Address
;; 0058 Serial Transfer Completion Interrupt Start Address
;; 0060 High-to-Low of P10-P13 Interrupt Start Address

;; 0100-014F: Internal Information Area. See datasheet.pdf for specifics

;;; Powerup Sequence
;; 256-byte program starting at #x0000 is run
;; draws nintendo logo (rom check)

;;; Stop Mode
;; halt processor until button is pressed
;;; Low Power mode

;;; Video
;; Background: 256x256 pixels or 32x32 tiles (8x8 each)
;; Resolution: 160x144 pixels
;; Scroll-x, Scroll-y: coordinates of the background to be displayed in the upper left
;; ... TODO





