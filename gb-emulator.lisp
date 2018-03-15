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
;; Background Tile Map (In VRAM): 32 rows of 32 bytes, each byte is a tile idx
;; Window: window-pos-x window-pos-y control location of the window
;; Tile Data: 8x8 image in 16 bytes
;; Sprites..

;;; Interrupts
;; enabled or disabled
;; interrupt Procedure:
;;    1. When an interrupt is generated, the IF flag will be set.
;;    2. If the IME flag is set & the corresponding IE flag is set, the following 3 steps are performed.
;;       3. Reset the IME flag and prevent all interrupts.
;;       4. The PC (program counter) is pushed onto the stack.
;;       5. Jump to the starting address of the interrupt.


;; Plan of attack
;;  We need:
;;    CPU
;;    video
;;    sound
;;    timer
;;    input
;; In Order:
;;  CPU:
;;    load blargg tests
;;    dis-assemble instructions
;;    display:
;;        registers
;;        previous/this/next instruction
;;        memory: stack
;;        specified memory region
;;    implement instructions (switch statement)
;;  Video
;;  Timer
;;  Input
;;  Sound

(defmacro defvars (&body var-names)
  `(progn
     ,@ (mapcar (lambda (v) `(defvar ,v)) var-names)))

(defun file-bytes (filename)
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence v stream)
      v)))

(defun gb-cpu-instrs-special ()
  (file-bytes (modest-pathnames:application-file-pathname
	       "test-roms/cpu_instrs/individual/01-special.gb"
	       :gb-emulator)))

(defun bios ()
  (file-bytes (modest-pathnames:application-file-pathname
	       "bios.gb"
	       :gb-emulator)))

(defrecord disassembled-instr
  name
  size
  args)

(defun kb (b) (* 1024 b))
(defun memory (b)
  (make-array b :element-type '(unsigned-byte 8)))

;; CPU
(defvars
  *pc* *sp*
  *a* *b*
  *c* *d*
  *e* *f*
  *h* *l*

  ;; Memory Regions
  *bank0-rom*
  *bank1-rom*
  *video-ram*
  *ext-ram*
  *work-ram*
  *sprite-ram*
  *mmap-i/o*
  *z-ram* ;; zero-page (page 255) RAM
  )

(defun load-bios! ()
  (setf (subseq *bank0-rom* 0) (bios)))

(defun init-memory-regions! ()
  (setq *bank0-rom* (memory (kb 16))
	*bank1-rom* (memory (kb 16))
	*video-ram* (memory (kb 8))
	*ext-ram* (memory (kb 8))
	*work-ram* (memory (kb 8))
	*sprite-ram* (memory 160)
	*mmap-i/o* (memory 128)
	*z-ram* (memory 128)))

(defun mem-byte (addr)
  (cond
    ((< addr #x4000)
     ;; bank0
     (aref *bank0-rom* addr))
    ((< addr #x8000)
     ;; bank1
     (aref *bank1-rom* (- addr #x4000)))
    ((< addr #xa000)
     ;; vram
     (aref *video-ram* (- addr #x8000)))
    ((< addr #xc000)
     ;; eram
     (aref *ext-ram* (- addr #xa000)))
    ((< addr #xe000)
     ;; wram
     (aref *work-ram* (- addr #xc000)))
    ((< addr #xfe00)
     ;; wram (copy)
     (aref *work-ram* (- addr #xe000)))
    ((< addr #xff00)
     ;; sprites
     (aref *sprite-ram* (- addr #xfe00)))
    ((< addr #xff80)
     ;; mmap-io
     (aref *mmap-i/o* (- addr #xff00)))
    (t
     ;; zram
     (aref *z-ram* (- addr #xff80)))))

(defun init! ()
  (setq *pc* 0)
  (setq *sp* 0)
  (setq *a* 0)
  (setq *b* 0)
  (setq *c* 0)
  (setq *d* 0)
  (setq *e* 0)
  (setq *f* 0)
  (setq *h* 0)
  (setq *l* 0)
  (init-memory-regions!)
  (load-bios!)
  :done)

(defun carry-set! () (setq *f* (logior #x10 *f*)))
(defun carry-clear! () (setq *f* (logand (lognot #x10) *f*)))

(defun half-carry-set! () (setq *f* (logior #x20 *f*)))
(defun half-carry-clear! () (setq *f* (logand (lognot #x20) *f*)))

(defun subtract-set! () (setq *f* (logior #x40 *f*)))
(defun subtract-clear! () (setq *f* (logand (lognot #x40) *f*)))

(defun zero-set! () (setq *f* (logior #x80 *f*)))
(defun zero-clear! () (setq *f* (logand (lognot #x80) *f*)))

(defun u16 (hi lo)
  (+ lo (ash hi 8)))
(defun byte8 (num)
  (logand #xff num))
(defun byte-lo (u16)
  (byte8 u16))
(defun byte-hi (u16)
  (byte8 (ash u16 -8)))

(defun sign8 (byte)
  "1 means sign is negative"
  (logand 1 (ash byte -7)))
(defun zero8 (byte)
  "1 if zero"
  (if (zerop byte) 1 0))
(defun carry8 (num)
  (if (> num (byte8 num)) 1 0))
(defun carry16 (num)
  (if (> num (logand #xffff num)) 1 0))

(defun bc () (u16 *b* *c*))
(defun de () (u16 *d* *e*))
(defun hl () (u16 *h* *l*))
(defun af () (u16 *a* *f*))

(defun set-bc! (msb lsb)
  (setq *b* msb
	*c* lsb))
(defun set-de! (msb lsb)
  (setq *d* msb
	*e* lsb))
(defun set-hl! (msb lsb)
  (setq *h* msb
	*l* lsb))
(defun set-af! (msb lsb)
  (setq *a* msb
	*f* lsb))
(defun set-reg! (reg msb lsb)
  (ecase reg
    (:bc (set-bc! msb lsb))
    (:de (set-de! msb lsb))
    (:hl (set-hl! msb lsb))
    (:sp (setq *sp* (u16 msb lsb)))
    (:af (set-af! msb lsb))))

(defun dest-reg (reg)
  (ecase reg
    (:a *a*)
    (:b *b*)
    (:c *c*)
    (:d *d*)
    (:e *e*)
    (:f *f*)
    (:h *h*)
    (:l *l*)))
(defun set-dest-reg! (reg byte)
  (ecase reg
    (:a (setq *a* byte))
    (:b (setq *b* byte))
    (:c (setq *c* byte))
    (:d (setq *d* byte))
    (:e (setq *e* byte))
    (:f (setq *f* byte))
    (:h (setq *h* byte))
    (:l (setq *l* byte))))

(defun mem-pc-byte ()
  (mem-byte *pc*))

(defun perform-alu-op! (alu-op dest-reg)
  (ecase alu-op
    (:add (error "not implemented"))
    (:adc (error "not implemented"))
    (:sub (error "not implemented"))
    (:sbc (error "not implemented"))
    (:and (error "not implemented"))
    (:xor
     (setq *a* (logxor *a* (dest-reg dest-reg)))
     
     (carry-clear!)
     (subtract-clear!)
     (half-carry-clear!)
     (if (zero8 *a*)
	 (zero-set!)
	 (zero-clear!)))
    (:or (error "not implemented"))
    (:cp (error "not implemented"))))

;; TODO: Categorize instructions

(defun bits-match? (bp1 bp2 either-mask)
  (= (logior bp1 either-mask) (logior bp2 either-mask)))

(defun extract-bits (bits low-idx length)
  "Bits are indexed high to low: e.g. 76543210"
  (logand (ash bits (- low-idx))
	  (1- (ash 1 length))))

(defparameter *dest-regs* #(:b :c :d :e :h :l :hl :a))
(defparameter *regs* #(:bc :de :hl :sp))
(defparameter *stack-regs* #(:bc :de :hl :af))
(defparameter *alu-ops* #(:add :adc :sub :sbc :and :xor :or :cp))

(defvar *disassembled-instr*)
(defun exec-instr! ()
  (declare (optimize debug))
  (let ((b1 (mem-pc-byte)))
    (cond
      ((= b1 #x00)
       (let ((size 1))
	 (setq *disassembled-instr* (make-disassembled-instr :nop size ()))
	 (incf *pc* size)))

      ((bits-match? b1
		    #b00000001
		    #b00110000)
       (let ((size 3)
	     (msb (mem-byte (+ *pc* 2)))
	     (lsb (mem-byte (* *pc* 1)))
	     (reg (aref *regs* (extract-bits b1 4 2))))
	 (setq *disassembled-instr*
	       (make-disassembled-instr :ld
					size
					(alist :reg reg :msb msb :lsb lsb)))
	 (set-reg! reg msb lsb)
	 (incf *pc* size)))

      ((bits-match? b1
		    #b10000000
		    #b00111111)
       (let ((size 1)
	     (dest-reg (aref *dest-regs* (extract-bits b1 0 3)))
	     (alu-op (aref *alu-ops* (extract-bits b1 3 3))))
	 (setq *disassembled-instr*
	       (make-disassembled-instr alu-op
					size
					(alist :dest-reg dest-reg)))

	 (perform-alu-op! alu-op dest-reg)
	 (incf *pc* size)))
      
      (t
       (error "Z80 Opcode Not Implemented: ~4,'0B ~4,'0B #x~x"
	      (logand (ash b1 -4) #xf)
	      (logand b1 #xf)
	      b1))))
  *disassembled-instr*)

(exec-instr!)
