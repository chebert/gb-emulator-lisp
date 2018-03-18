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

(defun bin (&rest args)
  (format nil "(~{#b~8,'0b~^ ~})" args))
(defun hex (&rest args)
  (format nil "(~{#x~4,'0x~^ ~})" args))

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
  b1 b2 b3
  size
  cycle-count
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

  *bios-run?*
  ;; Memory Regions
  *bios-rom*
  *bank0-rom*
  *bank1-rom*
  *video-ram*
  *ext-ram*
  *work-ram*
  *sprite-ram*
  *mmap-i/o*
  *z-ram* ;; zero-page (page 255) RAM
  )

(defun init-memory-regions! ()
  (setq *bios-rom* (bios)
	*bank0-rom* (memory (kb 16))
	*bank1-rom* (memory (kb 16))
	*video-ram* (memory (kb 8))
	*ext-ram* (memory (kb 8))
	*work-ram* (memory (kb 8))
	*sprite-ram* (memory 160)
	*mmap-i/o* (memory 128)
	*z-ram* (memory 128)))

(defun mem-byte (addr)
  (cond
    ((and (not *bios-run?*) (< addr #x100))
     (aref *bios-rom* addr))
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

(defun mem-byte-set! (addr byte)
  (cond
    ((and (not *bios-run?*) (< addr #x100))
     (setf (aref *bios-rom* addr) byte))
    ((< addr #x4000)
     ;; bank0
     (setf (aref *bank0-rom* addr) byte))
    ((< addr #x8000)
     ;; bank1
     (setf (aref *bank1-rom* (- addr #x4000)) byte))
    ((< addr #xa000)
     ;; vram
     (setf (aref *video-ram* (- addr #x8000)) byte))
    ((< addr #xc000)
     ;; eram
     (setf (aref *ext-ram* (- addr #xa000)) byte))
    ((< addr #xe000)
     ;; wram
     (setf (aref *work-ram* (- addr #xc000)) byte))
    ((< addr #xfe00)
     ;; wram (copy)
     (setf (aref *work-ram* (- addr #xe000)) byte))
    ((< addr #xff00)
     ;; sprites
     (setf (aref *sprite-ram* (- addr #xfe00)) byte))
    ((< addr #xff80)
     ;; mmap-io
     (setf (aref *mmap-i/o* (- addr #xff00)) byte))
    (t
     ;; zram
     (setf (aref *z-ram* (- addr #xff80)) byte))))

(defun init! ()
  (setq *bios-run?* nil)
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
  :done)

(defun stack-push! (byte)
  (push :sp *affected-regs*)
  (decf *sp*)
  (mem-byte-set! *sp* byte))
(defun stack-top ()
  (mem-byte *sp*))
(defun stack-pop! ()
  (push :sp *affected-regs*)
  (let ((b (stack-top)))
    (incf *sp*)
    b))

(defun set-pc! (adr)
  (push :pc *affected-regs*)
  (setq *pc* adr))
(defun inc-pc! (amt)
  (set-pc! (+ *pc* amt)))

;; F: ZNHC0000
(defvar *affected-regs*)
(defvar *affected-flags*)

(defun carry-set! ()
  (push :f *affected-regs*)
  (push :carry *affected-flags*)
  (setq *f* (logior #x10 *f*)))
(defun carry-set? () (not (carry-clear?)))
(defun carry-clear! ()
  (push :f *affected-regs*)
  (push :carry *affected-flags*)
  (setq *f* (logand (lognot #x10) *f*)))
(defun carry-clear? () (zerop (logand #x10 *f*)))
(defun carry-bit () (if (carry-clear?) 0 1))

(defun half-carry-set! ()
  (push :f *affected-regs*)
  (push :half-carry *affected-flags*)
  (setq *f* (logior #x20 *f*)))
(defun half-carry-set? () (not (half-carry-clear?)))
(defun half-carry-clear! ()
  (push :f *affected-regs*)
  (push :half-carry *affected-flags*)
  (setq *f* (logand (lognot #x20) *f*)))
(defun half-carry-clear? () (zerop (logand #x20 *f*)))

(defun negative-set! ()
  (push :f *affected-regs*)
  (push :negative *affected-flags*)
  (setq *f* (logior #x40 *f*)))
(defun negative-set? () (not (negative-clear?)))
(defun negative-clear! ()
  (push :f *affected-regs*)
  (push :negative *affected-flags*)
  (setq *f* (logand (lognot #x40) *f*)))
(defun negative-clear? () (zerop (logand #x40 *f*)))

(defun zero-set! ()
  (push :f *affected-regs*)
  (push :zero *affected-flags*)
  (setq *f* (logior #x80 *f*)))
(defun zero-set? () (not (zero-clear?)))
(defun zero-clear! ()
  (push :f *affected-regs*)
  (push :zero *affected-flags*)
  (setq *f* (logand (lognot #x80) *f*)))
(defun zero-clear? () (zerop (logand #x80 *f*)))

(defun s8 (u8)
  (if (>= u8 #x80)
      (- u8 #x100)
      u8))
(defun s16 (u16)
  (if (>= u16 #x8000)
      (- u16 #x10000)
      u16))
(defun u16 (hi lo)
  (+ lo (ash hi 8)))
(defun byte8 (num)
  (logand #xff num))
(defun byte-lo (u16)
  (byte8 u16))
(defun byte-hi (u16)
  (byte8 (ash u16 -8)))
(defun lsb (u16) (byte-lo u16))
(defun msb (u16) (byte-hi u16))

(defun carry8 (num)
  (if (> num (byte8 num)) 1 0))
(defun carry16 (num)
  (if (> num (logand #xffff num)) 1 0))

(defun bc () (u16 *b* *c*))
(defun de () (u16 *d* *e*))
(defun hl () (u16 *h* *l*))
(defun af () (u16 *a* *f*))

(defun incr-hl! ()
  (let ((hl (1+ (hl))))
    (set-hl! (byte-hi hl)
	     (byte-lo hl))))
(defun decr-hl! ()
  (let ((hl (1- (hl))))
    (set-hl! (byte-hi hl)
	     (byte-lo hl))))

(defun set-bc! (msb lsb)
  (appendf *affected-regs* (list :b :c :bc))
  (setq *b* msb
	*c* lsb))
(defun set-de! (msb lsb)
  (appendf *affected-regs* (list :d :e :de))
  (setq *d* msb
	*e* lsb))
(defun set-hl! (msb lsb)
  (appendf *affected-regs* (list :h :l :hl))
  (setq *h* msb
	*l* lsb))
(defun set-af! (msb lsb)
  (appendf *affected-regs* (list :a :f :af))
  (setq *a* msb
	*f* lsb))
(defun set-reg! (reg msb lsb)
  (ecase reg
    (:bc (set-bc! msb lsb))
    (:de (set-de! msb lsb))
    (:hl (set-hl! msb lsb))
    (:sp (push :sp *affected-regs*)
	 (setq *sp* (u16 msb lsb)))
    (:af (set-af! msb lsb))))
(defun reg (reg)
  (ecase reg
    (:bc (bc))
    (:de (de))
    (:hl (hl))
    (:sp *sp*)
    (:af (af))))

(defun dest-reg (reg)
  (ecase reg
    (:a *a*)
    (:b *b*)
    (:c *c*)
    (:d *d*)
    (:e *e*)
    (:f *f*)
    (:h *h*)
    (:l *l*)
    (:hl (mem-byte (hl)))))
(defun set-dest-reg! (reg byte)
  (ecase reg
    (:a
     (push :a *affected-regs*)
     (setq *a* byte))
    (:b
     (push :b *affected-regs*)
     (setq *b* byte))
    (:c
     (push :c *affected-regs*)
     (setq *c* byte))
    (:d
     (push :d *affected-regs*)
     (setq *d* byte))
    (:e
     (push :e *affected-regs*)
     (setq *e* byte))
    (:f
     (push :f *affected-regs*)
     (setq *f* byte))
    (:h
     (push :h *affected-regs*)
     (setq *h* byte))
    (:l
     (push :l *affected-regs*)
     (setq *l* byte))
    (:hl (mem-byte-set! (hl) byte))))

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
     (set-dest-reg! :a (logxor *a* (dest-reg dest-reg)))
     
     (carry-clear!)
     (negative-clear!)
     (half-carry-clear!)
     (if (zerop *a*)
	 (zero-set!)
	 (zero-clear!)))
    (:or (error "not implemented"))
    (:cp (error "not implemented"))))

(defun bit-test! (n dest-reg)
  (let ((res (not (zerop (logand (ash 1 n) (dest-reg dest-reg))))))
    (if res
	(zero-set!)
	(zero-clear!))
    ;; carry not affected
    (negative-clear!)
    (half-carry-set!)))

(defun test-cond (cnd)
  (ecase cnd
    (:not-zero (zero-clear?))
    (:zero (zero-set?))
    (:not-carry (carry-clear?))
    (:carry (carry-set?))))

(defun half-carry? (byte addend)
  (not (zerop (logand #x10
		      (+ (logand #xf byte)
			 (logand #xf addend))))))
(defun half-borrow? (byte subtracthend)
  (minusp (- (logand #xf byte) (logand #xf subtracthend))))

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
(defparameter *conditions* #(:not-zero :zero :not-carry :carry))

(defun arg-val-string (arg)
  (case (car arg)
    (:adr (format nil "#x~4,'0X" (cdr arg)))
    (:n (format nil "~A (#x~2,'0X)" (cdr arg) (cdr arg)))
    (t (format nil "~S" (cdr arg)))))

(defun args-string (args)
  (format nil "~a"
	  (mapcar (lambda (arg)
		    (cons (format nil "~S" (car arg)) (arg-val-string arg)))
		  args)))

(defun disassembled-instr-string (&optional (instr *disassembled-instr*))
  "Pretty-print of instr into a string."
  (typecase instr
    (disassembled-instr
     (format nil "~A ~A (~A bytes)"
	     (name instr)
	     (args-string (args instr))
	     (size instr)))
    (t
     (apply #'bin instr))))

(defun extract-reg (byte low-idx)
  (aref *regs* (extract-bits byte low-idx 2)))

(defun ld-reg-imm16? (b1)
  (bits-match? b1
	       #b00000001
	       #b00110000))

(defun ld-reg-imm16! (b1 b2 b3)
  ;; no flags
  (let ((size 3)
	(msb b3)
	(lsb b2)
	(reg (extract-reg b1 4)))
    (setq *disassembled-instr*
	  (make-disassembled-instr :ld
				   b1 b2 b3
				   size
				   12
				   (alist :reg reg :adr (u16 msb lsb))))
    (set-reg! reg msb lsb)
    (inc-pc! size)))

(defun nop? (b1)
  (= b1 #x00))
(defun nop! (b1 b2 b3)
  ;; no flags
  (let ((size 1)
	(cycle-count 4))
    (setq *disassembled-instr*
	  (make-disassembled-instr :nop b1 b2 b3 size cycle-count ()))
    (inc-pc! size)))

(defun alu-op-d? (b1)
  (bits-match? b1
	       #b10000000
	       #b00111111))
(defun alu-op-d! (b1 b2 b3)
  (let* ((size 1)
	 (dest-reg (extract-dest-reg b1 0))
	 (alu-op (aref *alu-ops* (extract-bits b1 3 3)))
	 (cycle-count (if (eq dest-reg :hl)
			  8
			  4)))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   alu-op
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :dest-reg
		  (cons dest-reg (dest-reg dest-reg)))))

    (perform-alu-op! alu-op dest-reg)
    (inc-pc! size)))

(defun ld-hl-a? (b1)
  (bits-match? b1
	       #b00100010
	       #b00011000))
(defun ld-hl-a! (b1 b2 b3)
  ;; no flags
  (let ((size 1)
	(op-type (extract-bits b1 4 1))
	(dir (extract-bits b1 3 1))
	(cycle-count 8))
    (ecase dir
      (0
       ;; from a into hl
       (mem-byte-set! (hl) *a*))
      (1
       ;; from hl into a
       (set-dest-reg! :a (mem-byte (hl)))))
    (ecase op-type
      (0 (incr-hl!))
      (1 (decr-hl!)))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   (ecase op-type
	     (0 :ldi)
	     (1 :ldd))
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :dir (ecase dir
			 (0 :a-to-hl)
			 (1 :hl-to-a)))))
    (inc-pc! size)))

(defun jr-cond-n? (b1)
  (bits-match? b1
	       #b00100000
	       #b00011000))
(defun jr-cond-n! (b1 b2 b3)
  ;; no flags
  (let ((size 2)
	(n (s8 b2))
	(cnd (aref *conditions* (extract-bits b1 3 2)))
	(cycle-count 8))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :jr
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :cond cnd :n n :adr (+ *pc* n size))))
    (cond
      ((test-cond cnd)
       (inc-pc! (+ n size)))
      (t
       (inc-pc! size)))))

(defun ld-dest-n? (b1)
  (bits-match? b1
	       #b00000110
	       #b00111000))
(defun ld-dest-n! (b1 b2 b3)
  ;; no flags
  (let* ((size 2)
	 (n b2)
	 (dest-reg (extract-dest-reg b1 3))
	 (cycle-count (if (eq dest-reg :hl)
			  12
			  8)))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :ld
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :n n
		  :dest-reg
		  (cons dest-reg (dest-reg dest-reg)))))
    (set-dest-reg! dest-reg n)
    (inc-pc! size)))

(defun byte-bit (byte idx)
  (logand 1 (ash byte (- idx))))
(defun bit7 (byte) (byte-bit byte 7))

(defun rotate-left-through-carry (byte carry-bit)
  (logand #xff (logior (ash byte 1) carry-bit)))
(defun rotate-left (byte)
  (logand #xff (logior (ash byte 1) (bit7 byte))))

(defun rotate-left-carry-bit (old-byte)
  (bit7 old-byte))

(defun not-implemented (fmt &rest args)
  (apply #'warn fmt args)
  (setq *disassembled-instr* (list (mem-byte *pc*)
				   (mem-byte (+ *pc* 1))
				   (mem-byte (+ *pc* 2)))))

(defun extract-dest-reg (byte low-idx)
  (aref *dest-regs* (extract-bits byte low-idx 3)))

(defun 16-bit-op? (b1)
  (= b1 #b11001011))
(defun 16-bit-op! (b1 b2 b3)
  (declare (optimize debug))
  ;; Two-byte opcode
  (cond
    ((bits-match? b2 #b00000000 #b00001111)
     ;; RDirC r
     ;; rotate r (carry=old bit 7)
     (not-implemented "not implemented"))

    ((bits-match? b2 #b00010000 #b00001111)
     ;; RDir r
     ;; rotate r through carry (carry=old bit 7)
     (let* ((size 2)
	    (dir (extract-bits b2 3 1))
	    (dest-reg (extract-dest-reg b2 0))
	    (byte (dest-reg dest-reg))
	    (cycle-count (if (eq dest-reg :hl)
			     16
			     8)))
       (setq *disassembled-instr*
	     (make-disassembled-instr
	      (ecase dir
		(0 ;; left
		 :rl)
		(1 ;; right
		 :rr))
	      b1 b2 b3
	      size
	      cycle-count
	      (alist :dest-reg (cons dest-reg (dest-reg dest-reg)))))
       (let ((res (ecase dir
		    (0 (rotate-left-through-carry byte (carry-bit))))))
	 (set-dest-reg! dest-reg res)
	 (ecase dir
	   (0 (if (zerop (rotate-left-carry-bit byte))
		  (carry-clear!)
		  (carry-set!))))
	 (negative-clear!)
	 (half-carry-clear!)
	 (if (zerop res)
	     (zero-set!)
	     (zero-clear!)))
       (inc-pc! size)))

    ((bits-match? b2 #b00100000 #b00001111)
     (not-implemented "not implemented"))
    ((bits-match? b2 #b00110000 #b00001111)
     (not-implemented "not implemented"))
    ((bits-match? b2 #b01000000 #b00111111)
     ;; BIT n, dest
     (let* ((size 2)
	    (n (extract-bits b2 3 3))
	    (dest-reg (extract-dest-reg b2 0))
	    (cycle-count (if (eq dest-reg :hl)
			     16
			     8)))
       (setq *disassembled-instr*
	     (make-disassembled-instr
	      :bit
	      b1 b2 b3
	      size
	      cycle-count
	      (alist :n n
		     :dest-reg (cons dest-reg (dest-reg dest-reg)))))
       (bit-test! n dest-reg)
       (inc-pc! size)))
    ((bits-match? b2 #b10000000 #b00111111)
     (not-implemented "not implemented"))
    ((bits-match? b2 #b11000000 #b00111111)
     (not-implemented "not implemented"))))

(defun ld-a-c? (b1)
  (bits-match? b1
	       #b11100010
	       #b00010000))
(defun ld-a-c! (b1 b2 b3)
  ;; no flags
  (let ((size 1)
	(dir (extract-bits b1 4 1))
	(adr (+ #xff00 *c*)))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :ld
	   b1 b2 b3
	   size
	   8
	   (alist :dir (ecase dir
			 (0 :register-to-memory)
			 (1 :memory-to-register))
		  :a *a*
		  :c (mem-byte adr)
		  :adr adr)))

    (ecase dir
      (0 ;; A -> (C)
       (mem-byte-set! adr *a*))
      (1 ;; (C) -> A
       (set-dest-reg! :a (mem-byte adr))))
    (inc-pc! size)))

(defun ld-a-n? (b1)
  (bits-match? b1
	       #b11100000
	       #b00010000))
(defun ld-a-n! (b1 b2 b3)
  ;; no flags
  (let ((size 2)
	(dir (extract-bits b1 4 1))
	(adr (+ #xff00 b2)))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :ld
	   b1 b2 b3
	   size
	   12
	   (alist :dir (ecase dir
			 (0 :register-to-memory)
			 (1 :memory-to-register))
		  :a *a*
		  :n b2
		  :adr adr)))

    (ecase dir
      (0 ;; A -> (n)
       (mem-byte-set! adr *a*))
      (1 ;; (n) -> A
       (set-dest-reg! :a (mem-byte adr))))
    (inc-pc! size)))

(defun inc-dest? (b1)
  (bits-match? b1
	       #b00000100
	       #b00111000))
(defun inc-dest! (b1 b2 b3)
  (let* ((size 1)
	 (dest-reg (extract-dest-reg b1 3))
	 (cycle-count (if (eq dest-reg :hl)
			  12
			  4)))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :inc
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :dest-reg
		  (cons dest-reg (dest-reg dest-reg)))))
    (let* ((byte (dest-reg dest-reg))
	   (result (1+ byte)))
      (set-dest-reg! dest-reg result)
      (if (zerop result)
	  (zero-set!)
	  (zero-clear!))
      (negative-clear!)
      ;; carry not affected
      (if (half-carry? byte 1)
	  (half-carry-set!)
	  (half-carry-clear!)))
    (inc-pc! size)))

(defun dec-dest? (b1)
  (bits-match? b1
	       #b00000101
	       #b00111000))
(defun dec-dest! (b1 b2 b3)
  (let* ((size 1)
	 (dest-reg (extract-dest-reg b1 3))
	 (cycle-count (if (eq dest-reg :hl)
			  12
			  4)))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :dec
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :dest-reg
		  (cons dest-reg (dest-reg dest-reg)))))
    (let* ((byte (dest-reg dest-reg))
	   (result (1- byte)))
      (set-dest-reg! dest-reg result)
      (if (zerop result)
	  (zero-set!)
	  (zero-clear!))
      (negative-set!)
      (if (half-borrow? byte 1)
	  (half-carry-clear!)
	  ;; Set if no borrow
	  (half-carry-set!))
      ;; carry not affected
      )
    (inc-pc! size)))

(defun ld-r1-r2? (b1)
  (bits-match? b1
	       #b01000000
	       #b00111111))
(defun ld-r1-r2! (b1 b2 b3)
  ;; no flags
  (let* ((size 1)
	 (r1 (extract-dest-reg b1 3))
	 (r2 (extract-dest-reg b1 0))
	 (cycle-count (if (or (eq r1 :hl)
			      (eq r2 :hl))
			  8
			  4)))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :ld
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :from-reg (cons r2 (dest-reg r2))
		  :dest-reg (cons r1 (dest-reg r1)))))
    (set-dest-reg! r1 (dest-reg r2))
    (inc-pc! size)))

(defun ld-a-r? (b1)
  (bits-match? b1
	       #b00000010
	       #b00011000))
(defun ld-a-r! (b1 b2 b3)
  ;; no flags
  (let* ((size 1)
	 (reg (extract-reg b1 4))
	 (adr (reg reg))
	 (dir (extract-bits b1 3 1))
	 (cycle-count 8))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :ld
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :reg reg
		  :adr adr
		  :dir (aref #(:a-to-r :r-to-a) dir))))
    (ecase dir
      (0 ;; a -> (r)
       (mem-byte-set! adr *a*))
      (1 ;; (r) -> a
       (set-dest-reg! :a (mem-byte adr))))
    (inc-pc! size)))

(defun call-n? (b1)
  (= b1 #b11001101))
(defun call-n! (b1 b2 b3)
  ;; no flags
  (let* ((size 3)
	 (lsb b2)
	 (msb b3)
	 (adr (u16 msb lsb))
	 (cycle-count 12))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   :call
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :adr adr)))
    (let ((next-pc (+ *pc* size)))
      (stack-push! (lsb next-pc))
      (stack-push! (msb next-pc))
      (set-pc! adr))))

(defun push/pop-r? (b1)
  (bits-match? b1
	       #b11000001
	       #b00110100))
(defun push/pop-r! (b1 b2 b3)
  ;; no flags
  (let* ((size 1)
	 (reg (extract-reg b1 4))
	 (type (extract-bits b1 2 1))
	 (cycle-count (ecase type
			(0 ;;pop
			 12)
			(1 ;;push
			 16))))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   (ecase type
	     (0 :pop)
	     (1 :push))
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :reg reg)))
    (ecase type
      (0
       (let ((c (stack-pop!))
	     (b (stack-pop!)))
	 (set-bc! b c)))
      (1
       (stack-push! *b*)
       (stack-push! *c*)))
    (inc-pc! size)))

;; TODO: Breakpoints
;; TODO: show memory
;; TODO: create disassembly
;; TODO: show next instruction

(defun rotate-a-carry? (b1)
  (bits-match? b1
	       #b00010111
	       #b00001000))
(defun rotate-a-carry! (b1 b2 b3)
  (let* ((size 1)
	 (dir (extract-bits b1 3 1))
	 (cycle-count 4))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   (aref #(:rla :rra) dir)
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :a *a*)))
    (ecase dir
      (0 ;; rla
       (let* ((byte *a*)
	      (res (rotate-left-through-carry byte (carry-bit))))
	 (set-dest-reg! :a res)
	 (if (zerop res)
	     (zero-set!)
	     (zero-clear!))
	 (negative-clear!)
	 (half-carry-clear!)
	 (if (rotate-left-carry-bit byte)
	     (carry-set!)
	     (carry-clear!))))
      (1 (not-implemented "RRA not implemented")))
    (inc-pc! size)))

;; 0010 0011
(defun inc/dec-reg? (b1)
  (bits-match? b1
	       #b00000011
	       #b00111000))
(defun inc/dec-reg! (b1 b2 b3)
  ;; no flags 
  (let* ((size 1)
	 (reg (extract-reg b1 4))
	 (adr (reg reg))
	 (byte (mem-byte adr))
	 (inc? (zerop (extract-bits b1 3 1)))
	 (cycle-count 8))
    (setq *disassembled-instr*
	  (make-disassembled-instr
	   (if inc? :inc :dec)
	   b1 b2 b3
	   size
	   cycle-count
	   (alist :reg reg)))
    (let ((result (if inc?
		      (1+ byte)
		      (1- byte))))
      (mem-byte-set! adr result))
    (inc-pc! size)))

(defvar *disassembled-instr*)
(defun exec-instr! ()
  (setq *affected-regs* ()
	*affected-flags* ())
  (let ((b1 (mem-pc-byte))
	(b2 (mem-byte (+ *pc* 1)))
	(b3 (mem-byte (+ *pc* 2))))
    (cond
      ((nop? b1) (nop! b1 b2 b3))

      ;; Loads
      ((ld-reg-imm16? b1) (ld-reg-imm16! b1 b2 b3))
      ((ld-hl-a? b1) (ld-hl-a! b1 b2 b3))
      ((ld-a-c? b1) (ld-a-c! b1 b2 b3))
      ((ld-a-n? b1) (ld-a-n! b1 b2 b3))
      ((ld-r1-r2? b1) (ld-r1-r2! b1 b2 b3))
      ((ld-a-r? b1) (ld-a-r! b1 b2 b3))
      ((ld-dest-n? b1) (ld-dest-n! b1 b2 b3))

      ;; Stack ops
      ((push/pop-r? b1) (push/pop-r! b1 b2 b3))

      ;; Arithmetic/Bit ops
      ((alu-op-d? b1) (alu-op-d! b1 b2 b3))
      ((inc-dest? b1) (inc-dest! b1 b2 b3))
      ((dec-dest? b1) (dec-dest! b1 b2 b3))
      ((inc/dec-reg? b1) (inc/dec-reg! b1 b2 b3))
      ((16-bit-op? b1) (16-bit-op! b1 b2 b3))
      ((rotate-a-carry? b1) (rotate-a-carry! b1 b2 b3))

      ;; Jumps/Calls
      ((jr-cond-n? b1) (jr-cond-n! b1 b2 b3))
      ((call-n? b1) (call-n! b1 b2 b3))

      (t
       (not-implemented "#x~4,'0x Z80 Opcode Not Implemented: ~4,'0B ~4,'0B #x~x"
			*pc*
			(logand (ash b1 -4) #xf)
			(logand b1 #xf)
			b1))))
  :done)

(defparameter *breakpoints* '(#x28 #xa3))
(defun continue-exec-instr! ()
  (let ((done? nil))
    (loop until done?
       do
	 (let ((pc *pc*))
	   (exec-instr!)
	   (push-instr! pc *disassembled-instr*)
	   (setq done? (or (not (typep *disassembled-instr*
				       'disassembled-instr))
			   (member pc *breakpoints*)))))))

;; TODO: save disassembled instructions, and match them in automated test
;; TODO: set/clear flags function

(defparameter *tetris-filename* "roms/Tetris (World).gb")

(defun load-rom! (filename)
  (let ((rom-bytes 
	 (file-bytes (modest-pathnames:application-file-pathname
		      filename
		      :gb-emulator))))
    (setq *bank0-rom* (subseq rom-bytes 0 (kb 16)))
    (setq *bank1-rom* (subseq rom-bytes (kb 16)))))

#+nil
(progn
  (init!)
  (load-rom! *tetris-filename*)
  (let (pc)
    (dotimes (i 28)
      (setq pc *pc*)
      (exec-instr!)
      (push-instr! pc *disassembled-instr*))
    (format t "~&#x~4,'0x: ~A" pc (disassembled-instr-string))))

(defvar *instr-history*)
(defun push-instr! (pc disassembled-instr)
  (setq *instr-history*
	(subseq (push (cons pc disassembled-instr) *instr-history*)
		0 (min 20 (length *instr-history*)))))

(defun instruction-e-list ()
  (e-list
   (mapcar
    (lambda (instr)
      (format nil "~&#x~4,'0x: ~A"
	      (car instr)
	      (typecase (cdr instr)
		(disassembled-instr (name (cdr instr)))
		(t "Not Implemented"))))
    *instr-history*)
   :id :disassembled-instrs))

(defun byte-text (val 16-bit? base)
  (ecase base
    (:binary
     (if 16-bit?
	 (format nil "b~4,'0b ~4,'0b ~4,'0b ~4,'0b"
		 (logand #xf (ash val -12))
		 (logand #xf (ash val -8))
		 (logand #xf (ash val -4))
		 (logand #xf val))
	 (format nil "b~4,'0b ~4,'0b "
		 (logand #xf (ash val -4))
		 (logand #xf val))))
    (:decimal
     (if 16-bit?
	 (format nil " ~5,' d (~6,' d)" val (s16 val))
	 (format nil " ~3,' d (~4,' d) " val (s8 val))))
    (:hex
     (if 16-bit?
	 (format nil "x~4,'0x" val)
	 (format nil "x~2,'0x  " val)))))

(defun reg-text-e (reg-name val 16-bit? base)
  (let ((aff? (member reg-name *affected-regs*)))
    (e-text
     :text
     (format nil "~2A: ~A" reg-name (byte-text val 16-bit? base))
     :color (if aff?
		(modest-drawing:red)
		(modest-drawing:black)))))

(defun flag-text-e ()
  (hbox
   :elements
   (list
    (e-text
     :text "F : ")
    (e-text
     :text (if (zero-set?) "Z" "z")
     :color (if (member :zero *affected-flags*)
		(modest-drawing:red)
		(modest-drawing:black)))
    (e-text
     :text (if (negative-set?) "N" "n")
     :color (if (member :negative *affected-flags*)
		(modest-drawing:red)
		(modest-drawing:black)))
    (e-text
     :text (if (half-carry-set?) "H" "h")
     :color (if (member :half-carry *affected-flags*)
		(modest-drawing:red)
		(modest-drawing:black)))
    (e-text
     :text (if (carry-set?) "C" "c")
     :color (if (member :carry *affected-flags*)
		(modest-drawing:red)
		(modest-drawing:black))))))

(defvar *reg-base* :hex)
(defparameter *reg-bases* '(:hex :binary :decimal))

(defun cpu-regs-e ()
  ;; Create element
  (e-collapsable
   (vbox
    :elements
    (list (reg-text-e :pc *pc* t :hex)
	  (reg-text-e :sp *sp* t :hex)
	  (flag-text-e)
	  (hbox
	   :elements
	   (list
	    (reg-text-e :a *a* nil *reg-base*)
	    (reg-text-e :af (af) t *reg-base*)))
	  (hbox
	   :elements
	   (list
	    (reg-text-e :b *b* nil *reg-base*)
	    (reg-text-e :bc (bc) t *reg-base*)))
	  (reg-text-e :c *c* nil *reg-base*)
	  (hbox
	   :elements
	   (list
	    (reg-text-e :d *d* nil *reg-base*)
	    (reg-text-e :de (de) t *reg-base*)))
	  (reg-text-e :e *e* nil *reg-base*)
	  (hbox
	   :elements
	   (list
	    (reg-text-e :h *h* nil *reg-base*)
	    (reg-text-e :hl (hl) t *reg-base*)))
	  (reg-text-e :l *l* nil *reg-base*)
	  (e-radio-button '("Hex" "Bin" "Dec")
			  :id :reg-base
			  :selected-option-idx
			  (position *reg-base* *reg-bases*))))
   :id :cpu
   :text "CPU"))

(defvar *gui-state*)

(defun gui-state-replace-element! (e)
  (multiple-value-bind (gui assets) (modest-gui:replace-element!
				     (assets *gui-state*)
				     (gui *gui-state*)
				     e)
    (setq *gui-state* (modest-gui:copy-gui-state *gui-state*
						 :gui gui
						 :assets assets))))
(defun gui-end-frame! (events)
  (modest-gui:events-processed! events *gui-state*)
  (setq *gui-state*
	(modest-gui:gui-state-assets-replaced! *gui-state*)))

(defun selected-instr ()
  (when (plusp (length *instr-history*))
    (let ((instrs-e (modest-gui:find-element (gui *gui-state*)
					     :disassembled-instrs)))
      (nth (modest-gui:selected-idx instrs-e) *instr-history*))))

(defun selected-disassembled-instr-e ()
  (let ((instr (selected-instr)))
    (e-text
     :id :disassembled-instr-text
     :text (format nil "Instr: ~A"
		   (if instr
		       (disassembled-instr-string (cdr instr))
		       "")))))

(defun main! ()
  (ssdl:with-init "GameBoy" 960 640
    (init!)
    (setq *instr-history* ())
    (setq *affected-regs* ()
	  *affected-flags* ())
    (load-rom! *tetris-filename*)

    (modest-gui:init-event-handlers!)
    (let* ((assets (modest-drawing:assets-loaded! () (list *font-asset*)))
	   (gui (vbox
		 :elements
		 (list
		  (hbox
		   :elements
		   (list
		    (e-collapsable
		     (vbox
		      :elements
		      (list
		       (e-scroll-view
			(instruction-e-list)
			:window-dims (make-v 200 240))
		       (hbox
			:elements
			(list
			 (e-button :id :step-button :text "Step")
			 (e-button :id :continue-button :text "Continue")))))
		     :text "Disassembly"
		     :collapsed? nil)
		    (cpu-regs-e)))
		  (selected-disassembled-instr-e)))))

      (setq *gui-state* (modest-gui:gui-state-created!
			 (modest-gui:make-gui-state gui assets ())))
      (unwind-protect
	   (progn
	     (modest-gui:register-handler!
	      'modest-gui:clicked-event
	      :step-button
	      (lambda (gui-state event)
		(declare (ignore gui-state event))
		(let ((pc *pc*))
		  (exec-instr!)
		  (push-instr! pc *disassembled-instr*))

		(gui-state-replace-element! (instruction-e-list))
		(gui-state-replace-element! (selected-disassembled-instr-e))
		(gui-state-replace-element! (cpu-regs-e))
		*gui-state*))

	     (modest-gui:register-handler!
	      'modest-gui:clicked-event
	      :continue-button
	      (lambda (gui-state event)
		(declare (ignore gui-state event))
		(continue-exec-instr!)

		(gui-state-replace-element! (instruction-e-list))
		(gui-state-replace-element! (selected-disassembled-instr-e))
		(gui-state-replace-element! (cpu-regs-e))
		*gui-state*))

	     (modest-gui:register-handler!
	      'modest-gui:changed-event
	      :reg-base
	      (lambda (gui-state event)
		(declare (ignore gui-state))

		(setq *reg-base* (nth
				  (modest-gui:selected-option-idx
				   (modest-gui:element event))
				  *reg-bases*))
		(gui-state-replace-element! (instruction-e-list))
		(gui-state-replace-element! (selected-disassembled-instr-e))
		(gui-state-replace-element! (cpu-regs-e))
		*gui-state*))

	     (modest-gui:register-handler!
	      'modest-gui:changed-event
	      :disassembled-instrs
	      (lambda (gui-state event)
		(declare (ignore gui-state event))

		(gui-state-replace-element! (selected-disassembled-instr-e))
		*gui-state*))
	     
	     (ssdl:enable-text-input)
	     (modest-gui:main-loop (input frames)
	       (let ((drawings ())
		     (gui-events ()))
		 (appendf drawings (modest-gui:cursor-drawings input))
		 
		 (setq *gui-state*
		       (modest-gui:gui-state-update-applied!
			*gui-state* input 0 (v0) drawings gui-events))

		 (modest-gui:draw-drawings! drawings)

		 (gui-end-frame! gui-events))))
	(modest-gui:gui-state-assets-freed! *gui-state*))
      (values))))

(defparameter *font-asset*
  (modest-drawing:make-font-asset
   :font-text
   nil
   (namestring
    (modest-pathnames:application-file-pathname
     "FiraMono-Regular.otf" :gb-emulator))
   14))
