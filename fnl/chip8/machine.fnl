(local bit (require :bit))
(local {: view} (require :fennel))
(local {:format fmt} string)
(local {: write-bytes : read-bytes
        : nibbles->word : word->nibbles} (require :chip8.memory))

(fn dump-memory [memory ?offset ?len]
  (let [start (or ?offset 0)
        end (- (+ start (or ?len memory.size)) 1)
        mem (fcollect [i start end 2]
              (bit.tohex (bit.bor (bit.lshift (. memory.block i) 8)
                                  (. memory.block (+ i 1)))
                         4))
        step 4]
    (print)
    (for [i 1 (length mem) step]
      (let [block (fcollect [ii 0 (- step 1)] (. mem (+ i ii)))]
        (print (unpack block))))))

(fn dump-bytes [bytes]
  (let [mem (icollect [_ b (ipairs bytes)]
              (bit.tohex b 2))
        step 4]
    (for [i 1 (length mem) step]
      (let [block (fcollect [ii 0 (- step 1)] (. mem (+ i ii)))]
        (print (unpack block))))))

(fn load-rom [machine path]
  (case-try
    (print path) _
    (io.open path :rb) fd
    (fd:read :*a) blob
    (fd:close) _
    (let [bytes [(string.byte blob 1 (length blob))]]
      (machine:write-bytes machine.pc bytes)
      ; (print machine.memory.size)
      ; (dump machine.memory 0x0200)
      machine)
    (catch
      (nil e) (print e))))

(fn font-bytes []
  [0xF0 0x90 0x90 0x90 0xF0 ;; 0
   0x20 0x60 0x20 0x20 0x70 ;; 1
   0xF0 0x10 0xF0 0x80 0xF0 ;; 2
   0xF0 0x10 0xF0 0x10 0xF0 ;; 3
   0x90 0x90 0xF0 0x10 0x10 ;; 4
   0xF0 0x80 0xF0 0x10 0xF0 ;; 5
   0xF0 0x80 0xF0 0x90 0xF0 ;; 6
   0xF0 0x10 0x20 0x40 0x40 ;; 7
   0xF0 0x90 0xF0 0x90 0xF0 ;; 8
   0xF0 0x90 0xF0 0x10 0xF0 ;; 9
   0xF0 0x90 0xF0 0x90 0x90 ;; A
   0xE0 0x90 0xE0 0x90 0xE0 ;; B
   0xF0 0x80 0x80 0x80 0xF0 ;; C
   0xE0 0x90 0x90 0x90 0xE0 ;; D
   0xF0 0x80 0xF0 0x80 0xF0 ;; E
   0xF0 0x80 0xF0 0x80 0x80]) ;; F

(fn CALL-ML-NNN [machine]
  "Execute machine language subroutine at address NNN"
  ;; 0NNN
  (error "native execution ont supported"))

(fn CLS [machine]
  "Clear the screen"
  ;; 00E0
  (for [i 0 (- machine.video.size 1)]
    (tset machine.video.block i 0))
  (machine:inc-pc))

(fn RET [machine]
  "Return from a subroutine"
  ;; 00EE
  (let [addr (machine:stack-pop)]
    (set machine.pc addr)))

(fn JP-NNN [machine n1 n2 n3]
  "Jump to address NNN"
  ;; 1NNN
  (let [addr (nibbles->word 0 n1 n2 n3)]
    (set machine.pc addr)
    machine))

(fn CALL-NNN [machine n1 n2 n3]
  "Execute subroutine starting at address NNN"
  ;; 2NNN
  (let [addr (nibbles->word 0 n1 n2 n3)
        next-pc (+ machine.pc 0x02)]
    (machine:stack-push next-pc)
    (set machine.pc addr)
    machine))

(fn SE-VX-NN [machine vx n1 n2]
  "Skip the following instruction if the value of register VX equals NN"
  ;; 3XNN
  (let [num (nibbles->word 0 0 n1 n2)
        v-val (machine:read-register vx)]
    (if (= num v-val)
      (machine:inc-pc 2)
      (machine:inc-pc 1))))

(fn SNE-VX-NN [machine vx n1 n2]
  "Skip the following instruction if the value of register VX is not equal to NN"
  ;; 4XNN
  (let [num (nibbles->word 0 0 n1 n2)
        v-val (machine:read-register vx)]
    (if (= num v-val)
      (machine:inc-pc 1)
      (machine:inc-pc 2))))

(fn SE-VX-VY [machine vx vy]
  "Skip the following instruction if the value of register VX is equal to the value of register VY"
  ;; 5XY0
  (if (= (machine:read-register vx) (machine:read-register vy))
    (machine:inc-pc 2)
    (machine:inc-pc 1)))

(fn LD-VX-NN [machine vx n1 n2]
  "Store number NN in register VX"
  ;; 6XNN
  (let [num (nibbles->word 0 0 n1 n2)]
    (machine:write-register vx num)
    (machine:inc-pc)))

(fn ADD-VX-NN [machine vx n1 n2]
  "Add the value NN to register VX"
  ;; 7XNN
  (let [a (nibbles->word 0 0 n1 n2)
        b (machine:read-register vx)]
    (machine:write-register vx (+ a b))
    (machine:inc-pc)))

(fn LD-VX-VY [machine vx vy]
  "Store the value of register VY in register VX"
  ;; 8XY0
  (machine:write-register vx (machine:read-register vy))
  (machine:inc-pc))

(fn OR-VX-VY [machine vx vy]
  "Set VX to VX OR VY"
  ;; 8XY1
  (let [x (machine:read-register vx)
        y (machine:read-register vy)]
    (machine:write-register vx (bit.bor x y))
    (machine:inc-pc)))

(fn AND-VX-VY [machine vx vy]
  "Set VX to VX AND VY"
  ;; 8XY2
  (let [x (machine:read-register vx)
        y (machine:read-register vy)]
    (machine:write-register vx (bit.band x y))
    (machine:inc-pc)))

(fn XOR-VX-VY [machine vx vy]
  "Set VX to VX XOR VY"
  ;; 8XY3
  (let [x (machine:read-register vx)
        y (machine:read-register vy)]
    (machine:write-register vx (bit.bxor x y))
    (machine:inc-pc)))

(fn ADD-VX-VY [machine vx vy]
  "Add the value of register VY to register VX, set VF to 01 if a carry occurs"
  ;; 8XY4
  (let [x (machine:read-register vx)
        y (machine:read-register vy)
        sum (+ x y)]
    (machine:write-register vx sum)
    (machine:write-register 0xF (if (< 0xFF sum) 1 0))
    (machine:inc-pc)))

(fn SUB-VX-VY [machine vx vy]
  "Subtract the value of register VY from register VX, set VF to 00 if a borrow occurs"
  ;; 8XY5
  (let [x (machine:read-register vx)
        y (machine:read-register vy)
        sum (- x y)]
    (machine:write-register vx (if (< sum 0) (+ sum 256) sum))
    (machine:write-register 0xF (if (< sum 0) 0 1))
    (machine:inc-pc)))

(fn SHR-VX-VY [machine vx vy]
  "Store the value of register VY shifted right one bit in register VX, set VF to the least significant bit prior to the shift"
  ;; 8XY6
  (let [y (machine:read-register vy)
        lsb (bit.band y 0x01)
        shifted (bit.rshift y 1)]
    (machine:write-register vx shifted)
    (machine:write-register 0xF lsb)
    (machine:inc-pc)))

(fn SUBN-VX-VY [machine vx vy]
  "Set register VX to the value of VY minus VX, set VF to 00 if a borrow occurs"
  ;; 8XY7
  (let [x (machine:read-register vx)
        y (machine:read-register vy)
        sum (- y x)]
    (machine:write-register vx (if (< sum 0) (+ sum 256) sum))
    (machine:write-register 0xF (if (< sum 0) 0 1))
    (machine:inc-pc)))

(fn SHL-VX-VY [machine vx vy]
  "Store the value of register VY shifted left one bit in register VX, set VF to the most significant bit prior to the shift"
  ;; 8XYE
  (let [y (machine:read-register vy)
        msb (bit.rshift y 7)
        shifted (bit.band (bit.lshift y 1) 0xFF)]
    (machine:write-register vx shifted)
    (machine:write-register 0xF msb)
    (machine:inc-pc)))

(fn SNE-VX-VY [machine vx vy]
  "Skip the following instruction if the value of register VX is not equal to the value of register VY"
  ;; 9XY0
  (let [x (machine:read-register vx)
        y (machine:read-register vy)]
    (if (= x y)
      (machine:inc-pc)
      (machine:inc-pc 2))))

(fn LD-I-NNN [machine n1 n2 n3]
  "Store memory address NNN in register I"
  ;; ANNN
  (let [addr (nibbles->word 0 n1 n2 n3)]
    (machine:write-index addr)
    (machine:inc-pc)))

(fn JP-V0-NNN [machine n1 n2 n3]
  "Jump to address NNN + V0"
  ;; BNNN
  (let [base-addr (nibbles->word 0 n1 n2 n3)
        v0 (machine:read-register 0)
        addr (+ base-addr v0)]
    (set machine.pc (bit.band addr 0xFFFF))))

(fn RND-VX-NN [machine vx n1 n2]
  "Set VX to a random number with a mask of NN"
  ;; CXNN
  (let [mask (nibbles->word 0 0 n1 n2)
        num (-> (math.random 0x00 0xFF)
                (bit.band mask))]
    (machine:write-register vx num)
    (machine:inc-pc)))

(fn DRW-VX-VY-N [machine vx vy n]
  "Draw a sprite at position VX, VY with N bytes of sprite data starting at the address stored in I, set VF to 01 if any set pixels are changed to unset, and 00 otherwise"
  ;; DXYN
  (fn update-video-byte [mem-loc new-byte]
    (let [[old-byte] (read-bytes machine.video mem-loc 1)
          xored (bit.bxor old-byte new-byte)
          unset? (not= old-byte (bit.band old-byte xored))]
      (if unset? (machine:write-register 0xF 0x1))
      (write-bytes machine.video mem-loc [xored])))

  ;; default to "nothing unset", checked and set in update-video-byte
  (machine:write-register 0xF 0)

  (let [addr (machine:read-index)
        ;; x and y are "bit relative", but we render "byte relative",
        ;; eg x = 5, w = 8, we must update bits 5 6 7 8 in byte 1 and 1 2 3 4
        ;; in byte 2
        ;; TODO w/h should probably come from machine.video
        x (% (machine:read-register vx) 64)
        y (% (machine:read-register vy) 32)
        stride (/ 64 8)
        n (math.min n (- 32 y))
        bytes (machine:read-bytes addr n)]
    (for [i 0 (- (length bytes) 1)]
      (let [byte (. bytes (+ i 1))
            y-byte (+ y i)
            bit-misalignment (% x 8)
            first-x-byte (math.floor (/ x 8))
            mem-loc-1 (+ first-x-byte (* y-byte stride))
            ;; When aligned, we actually write the second byte out anyway
            ;; but its existing-byte ^ 00000000, effectively no-op.
            mem-loc-2 (+ mem-loc-1 1)
            left-byte (bit.rshift byte bit-misalignment)
            right-byte (-> (bit.lshift byte (- 8 bit-misalignment))
                           (bit.band 0xFF))]
        (update-video-byte mem-loc-1 left-byte)
        ;; The second byte cannot ever be on the first column, so if it is,
        ;; we can know it's extended past the screen bounds and we should
        ;; clip it.
        (if (not= 0 (% mem-loc-2 8))
          (update-video-byte mem-loc-2 right-byte))))
    (machine:inc-pc)))

(fn SKP-VX []
  "Skip the following instruction if the key corresponding to the hex value currently stored in register VX is pressed"
  ;; EX9E
  )

(fn SKNP-VX []
  "Skip the following instruction if the key corresponding to the hex value currently stored in register VX is not pressed"
  ;; EXA1
  )

(fn LD-VX-DT []
  "Store the current value of the delay timer in register VX"
  ;; FX07
  )

(fn LD-VX-K []
  "Wait for a keypress and store the result in register VX"
  ;; FX0A
  )

(fn LD-DT-VX []
  "Set the delay timer to the value of register VX"
  ;; FX15
  )

(fn LD-ST-VX []
  "Set the sound timer to the value of register VX"
  ;; FX18
  )

(fn ADD-I-VX []
  "Add the value stored in register VX to register I"
  ;; FX1E
  )

(fn LD-F-VX []
  "Set I to the memory address of the sprite data corresponding to the hexadecimal digit stored in register VX"
  ;; FX29
  )

(fn LD-B-VX []
  "Store the binary-coded decimal equivalent of the value stored in register VX at addresses I, I + 1, and I + 2"
  ;; FX33
  )

(fn LD-I-VX []
  "Store the values of registers V0 to VX inclusive in memory starting at address I, set I to I + X + 1 after operation"
  ;; FX55
  )

(fn LD-VX-I []
  "Fill registers V0 to VX inclusive with the values stored in memory starting at address I, set I to I + X + 1 after operation"
  ;; FX65
  )

(fn step [machine]
  (let [[instruction] (machine:read-words machine.pc 1)
        [n1 n2 n3 n4] (word->nibbles instruction)]
    ; (print "executing ins" (bit.tohex instruction 4))
    (case (values n1 n2 n3 n4)
      (0x0 0x0 0xE 0x0) (CLS machine)
      (0x0 0x0 0xE 0xE) (RET machine)
      (0x1 n1 n2 n3) (JP-NNN machine n1 n2 n3)
      (0x2 n1 n2 n3) (CALL-NNN machine n1 n2 n3)
      (0x3 vx n1 n2) (SE-VX-NN machine vx n1 n2)
      (0x4 vx n1 n2) (SNE-VX-NN machine vx n1 n2)
      (0x5 vx vy 0x0) (SE-VX-VY machine vx vy)
      (0x6 vx n1 n2) (LD-VX-NN machine vx n1 n2)
      (0x7 vx n1 n2) (ADD-VX-NN machine vx n1 n2)
      (0x8 vx vy 0x0) (LD-VX-VY machine vx vy)
      (0x8 vx vy 0x1) (OR-VX-VY machine vx vy)
      (0x8 vx vy 0x2) (AND-VX-VY machine vx vy)
      (0x8 vx vy 0x3) (XOR-VX-VY machine vx vy)
      (0x8 vx vy 0x4) (ADD-VX-VY machine vx vy)
      (0x8 vx vy 0x5) (SUB-VX-VY machine vx vy)
      (0x8 vx vy 0x6) (SHR-VX-VY machine vx vy)
      (0x8 vx vy 0x7) (SUBN-VX-VY machine vx vy)
      (0x8 vx vy 0xE) (SHL-VX-VY machine vx vy)
      (0x9 vx vy 0x0) (SNE-VX-VY machine vx vy)
      ; (0x8 vx vy 0x6) (error)
      (0xA n1 n2 n3) (LD-I-NNN machine n1 n2 n3)
      (0xB n1 n2 n3) (JP-V0-NNN machine n1 n2 n3)
      (0xC vx n1 n2) (RND-VX-NN machine vx n1 n2)
      (0xD vx vy n1) (DRW-VX-VY-N machine vx vy n1)

      _ (error (fmt "unknown instruction 0x%s" (bit.tohex instruction 4))))))


(fn new []
  "Create a blank CHIP8 machine"
  (let [{:new new-memory
         : read-bytes : write-bytes
         : read-words : write-words} (require :chip8.memory)
        machine {:pc 0x0200
                 ;; Stack starts 2 past stack head, but the first push will -2 it.
                 :sp 0x0200 
                 ;; These could indeed just be a table, but by using
                 ;; the memory interface we get some simpler type coercion.
                 :v (new-memory 0xF)
                 :i (new-memory 0x2)
                 :memory (new-memory 0x1000)
                 ;; each byte of video memory is a 8-pixel strip, one bit per pixel
                 ;; so we can compress our width by 8, but our height is one byte
                 ;; per pixel.
                 :video (new-memory (* (/ 64 8) 32))
                 :mhz 500
                 :step step

                 :load-rom (fn [m ...] (load-rom m ...))

                 :inc-pc (fn [m ?distance] (set m.pc (+ m.pc (* 2 (or ?distance 1)))))

                 :stack-push (fn stack-push [m word]
                               (set m.sp (- m.sp 0x2))
                               (m:write-words m.sp [word]))
                 :stack-pop (fn [m]
                              (let [[word] (m:read-words m.sp 1)]
                                (set m.sp (+ m.sp 0x2))
                                word))

                 :read-index (fn [m] (. (read-words m.i 0 1) 1))
                 :write-index (fn [m v] (write-words m.i 0 [v]))

                 :read-register (fn [m v] (. m.v.block v))
                 :write-register (fn [m v n] (tset m.v.block v n))
                 :write-bytes (fn [m ...] (write-bytes m.memory ...))
                 :read-bytes (fn [m ...] (read-bytes m.memory ...))
                 :read-words (fn [m ...] (read-words m.memory ...))
                 :write-words (fn [m ...] (write-words m.memory ...))}]
    (machine:write-bytes 0 (font-bytes))
    machine))

{: new
 : step}
