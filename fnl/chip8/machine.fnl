(local bit (require :bit))
(local {: view} (require :fennel))
(local {:format fmt} string)
(local {: write-bytes : read-bytes
        : nibbles->word : word->nibbles} (require :chip8.memory))

(local bootloader [0x59 0x4f 0x55 0x20
                   0x43 0x41 0x4e 0x20
                   0x50 0x4c 0x41 0x59
                   0x20 0x57 0x49 0x54
                   0x48 0x20 0x55 0x53
                   0x00 0x00 0x00 0x00])

(local font-bytes [0xF0 0x90 0x90 0x90 0xF0 ;; 0
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

(local font-mem-loc 0x0050) ;; - 0x009f
(local keyboard-mem-loc 0x100)

(fn tobin [n len]
  (faccumulate [s "" i 0 (- len 1)]
    (let [b (-> (bit.rshift n i) (bit.band 0x1))]
      (.. b s))))

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
    (if machine.quirks.vf-reset
      (machine:write-register 0xf 0))
    (machine:write-register vx (bit.bor x y))
    (machine:inc-pc)))

(fn AND-VX-VY [machine vx vy]
  "Set VX to VX AND VY"
  ;; 8XY2
  (let [x (machine:read-register vx)
        y (machine:read-register vy)]
    (if machine.quirks.vf-reset
      (machine:write-register 0xf 0))
    (machine:write-register vx (bit.band x y))
    (machine:inc-pc)))

(fn XOR-VX-VY [machine vx vy]
  "Set VX to VX XOR VY"
  ;; 8XY3
  (let [x (machine:read-register vx)
        y (machine:read-register vy)]
    (if machine.quirks.vf-reset
      (machine:write-register 0xf 0))
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
  (let [val (if machine.quirks.shifting
              (machine:read-register vx)
              (machine:read-register vy))
        lsb (bit.band val 0x01)
        shifted (bit.rshift val 1)]
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
  (let [val (if machine.quirks.shifting
              (machine:read-register vx)
              (machine:read-register vy))
        msb (bit.rshift val 7)
        shifted (bit.band (bit.lshift val 1) 0xFF)]
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
        vx-or-v0 (if machine.quirks.jumping
                   (machine:read-register n1)
                   (machine:read-register 0))
        addr (+ base-addr vx-or-v0)]
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

(fn SKP-VX [machine vx]
  "Skip the following instruction if the key corresponding to the hex value currently stored in register VX is pressed"
  ;; EX9E
  (let [k (machine:read-register vx)
        want-mask (bit.lshift 1 k)
        [have-mask] (machine:read-words keyboard-mem-loc 1)
        test (bit.band want-mask have-mask)]
    (machine:inc-pc)
    (if (< 0 test) (machine:inc-pc))))

(fn SKNP-VX [machine vx]
  "Skip the following instruction if the key corresponding to the hex value currently stored in register VX is not pressed"
  ;; EXA1
  (let [k (machine:read-register vx)
        dont-want-mask (bit.lshift 1 k)
        [have-mask] (machine:read-words keyboard-mem-loc 1)
        test (bit.band dont-want-mask have-mask)]
    (machine:inc-pc)
    (if (= 0 test) (machine:inc-pc))))

(fn LD-VX-DT [machine vx]
  "Store the current value of the delay timer in register VX"
  ;; FX07
  (machine:write-register vx machine.delay.t)
  (machine:inc-pc))

(fn LD-VX-K [machine vx]
  "Wait for a keypress and store the result in register VX"
  ;; FX0A
  (let [k (machine:read-register vx)
        want-mask (bit.lshift 1 k)
        [have-mask] (machine:read-words keyboard-mem-loc 1)
        test (bit.band want-mask have-mask)]
    (if (< 0 test) (machine:inc-pc))))

(fn LD-DT-VX [machine vx]
  "Set the delay timer to the value of register VX"
  ;; FX15
  (let [t (machine:read-register vx)]
    (machine.delay:set t machine.rtc-ms)
    (machine:inc-pc)))

(fn LD-ST-VX [machine vx]
  "Set the sound timer to the value of register VX"
  ;; FX18
  (let [t (machine:read-register vx)]
    (machine.sound:set t machine.rtc-ms)
    (machine:inc-pc)))

(fn ADD-I-VX [machine vx]
  "Add the value stored in register VX to register I"
  ;; FX1E
  (let [x (machine:read-register vx)
        i (machine:read-index)]
    (machine:write-index (+ i x))
    (machine:inc-pc)))

(fn LD-F-VX [machine vx]
  "Set I to the memory address of the sprite data corresponding to the hexadecimal digit stored in register VX"
  ;; FX29
  (let [c (bit.band 0x0F (machine:read-register vx))
        addr (+ font-mem-loc (* c 5))]
    (machine:write-index addr)
    (machine:inc-pc)))

(fn LD-B-VX [machine vx]
  "Store the binary-coded decimal equivalent of the value stored in register VX at addresses I, I + 1, and I + 2"
  ;; FX33
  (let [num (machine:read-register vx)
        hunds (-> (/ num 100)
                  (math.floor))
        tens (-> (% num 100)
                 (/ 10)
                 (math.floor))
        ones (-> (% num 10)
                 (math.floor))]
    (machine:write-bytes (machine:read-index) [hunds tens ones])
    (machine:inc-pc)))

(fn LD-I-VX [machine vx]
  "Store the values of registers V0 to VX inclusive in memory starting at address I, set I to I + X + 1 after operation"
  ;; FX55
  (let [addr (machine:read-index)
        bytes (fcollect [i 0 vx]
                (machine:read-register i))]
    (machine:write-bytes addr bytes)
    (if (not machine.quirks.memory)
      (machine:write-index (+ addr vx 1)))
    (machine:inc-pc)))

(fn LD-VX-I [machine vx]
  "Fill registers V0 to VX inclusive with the values stored in memory starting at address I, set I to I + X + 1 after operation"
  ;; FX65
  (let [addr (machine:read-index)
        bytes (machine:read-bytes addr (+ vx 1))]
    (for [i 0 vx]
      (machine:write-register i (. bytes (+ i 1))))
    (machine:write-index (+ addr vx 1))
    (machine:inc-pc)))

(var last-ins 0)
(fn fetch-decode-execute [machine]
  (let [[instruction] (machine:read-words machine.pc 1)
        _ (set last-ins instruction)
        [n1 n2 n3 n4] (word->nibbles instruction)]
    (case (values n1 n2 n3 n4)
      (0x0 0x0 0xE 0x0) (CLS machine)
      (0x0 0x0 0xE 0xE) (RET machine)
      ;; while 0NNN is supposed to execute "native machine instructions at nnn"
      ;; it seems that modern implementations treat this the same as 1NNN.
      (0x0 n1 n2 n3) (JP-NNN machine n1 n2 n3)
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
      (0xA n1 n2 n3) (LD-I-NNN machine n1 n2 n3)
      (0xB n1 n2 n3) (JP-V0-NNN machine n1 n2 n3)
      (0xC vx n1 n2) (RND-VX-NN machine vx n1 n2)
      (0xD vx vy n1) (DRW-VX-VY-N machine vx vy n1)
      (0xE vx 0x9 0xE) (SKP-VX machine vx)
      (0xE vx 0xA 0x1) (SKNP-VX machine vx)
      (0xF vx 0x0 0x7) (LD-VX-DT machine vx)
      (0xF vx 0x0 0xA) (LD-VX-K machine vx)
      (0xF vx 0x1 0x5) (LD-DT-VX machine vx)
      (0xF vx 0x1 0x8) (LD-ST-VX machine vx)
      (0xF vx 0x1 0xE) (ADD-I-VX machine vx)
      (0xF vx 0x2 0x9) (LD-F-VX machine vx)
      (0xF vx 0x3 0x3) (LD-B-VX machine vx)
      (0xF vx 0x5 0x5) (LD-I-VX machine vx)
      (0xF vx 0x6 0x5) (LD-VX-I machine vx)

      _ (error (fmt "unknown instruction 0x%s" (bit.tohex instruction 4))))))

(fn tick-timers [machine]
  (machine.delay:tick machine.rtc-ms)
  (machine.sound:tick machine.rtc-ms))

(var suspended-ms 0) ;; todo this will delay the first steps
(fn step [machine ?delta-ms]
  ;; We run at a fixed rate (machine.hz), but we're stepped by an external
  ;; system, so that system will pass in the ms time since we last stepped
  ;; and its up to us to decide whether we should do anything or wait until
  ;; we are called again.
  ;;
  ;; If no delta-ms is given, then we force a step at the threshold-ms rate.

  (local threshold-ms (math.floor (/ 1000 machine.hz)))
  (local ?delta-ms (math.floor (or ?delta-ms threshold-ms)))
  ;; We accumulate suspended-ms until its over some threshold then drain it.
  ;; We may step multiple times if the deltas are high enough.
  (set suspended-ms (+ suspended-ms ?delta-ms))

  (when (<= threshold-ms suspended-ms)
    (set machine.rtc-ms (+ machine.rtc-ms threshold-ms))
    (tick-timers machine)
    (fetch-decode-execute machine)
    (set suspended-ms (- suspended-ms threshold-ms))
    (step machine 0)))

(fn new-timer []
  (var last-ms 0)
  (var interval-ms (math.floor (* (/ 1 60) 1000))) ;; timers are always 60hz
  {:t 0x00
   :set (fn [timer time now-ms]
          (set last-ms now-ms)
          (set timer.t (bit.band time 0xFF)))
   :tick (fn [timer now-ms]
           (when (< 0 timer.t)
             (let [delta (- now-ms last-ms)]
               (when (<= interval-ms delta)
                 (let [steps (math.floor (/ delta interval-ms))]
                   (set timer.t (math.max 0 (- timer.t steps)))
                   (set last-ms (+ last-ms (* steps interval-ms))))))))})

(fn merge-table [t-into t-from]
  ;; so we can do (or false x) and get false if the user wanted
  (fn unless-nil  [x y] (if (= nil x) y x))
  (accumulate [t t-into k v (pairs t-from)]
    (case (type v)
      :table (do
               (assert (or (= nil (. t-into k)) (= :table (type (. t-into k))))
                       (string.format "%s must be table or nil" k))
               (doto t (tset k (merge-table (unless-nil (. t-into k) {}) v))))
      _ (doto t (tset k (unless-nil (. t-into k) v))))))

(λ new [?opts]
  "Create a blank CHIP8 machine
  
  keyboard function: pass 1 16 bit word where each bit maps to a key, 0->F. if
  a bit is on, the key is down. the first bit is key 0, the 16th bit is key f.
  "
  (let [{:new new-memory
         : read-bytes : write-bytes
         : read-words : write-words} (require :chip8.memory)
        defaults {:devices {:keyboard #nil
                            :video #nil
                            :audio #nil}
                  :mhz 0.5
                  :compatibility :CHIP-8}
        opts (merge-table (or ?opts {}) defaults)
        quirks (case opts.compatibility
                 ;; quirk names rom timendus chip-8 test suite flag rom
                 :CHIP-8 {:vf-reset true
                          :memory false
                          :shifting false
                          :jumping false}
                 :SUPER-CHIP-1.0 {:vf-reset false
                                  :memory true
                                  :shifting true
                                  :jumping true}
                 :XO-CHIP {:vf-reset false
                           :memory true
                           :shifting true
                           :jumping true}
                 _ (error (string.format "unknown compatibility %s"
                                         opts.compatibility)))
        machine {:pc 0x0200
                 ;; Stack starts 2 past stack head, but the first push will -2 it.
                 :sp 0x0200
                 ;; These could indeed just be a table, but by using
                 ;; the memory interface we get some simpler type coercion
                 ;; and glory glory hallelujah, zero based indexes.
                 :v (new-memory 0xF)
                 :i (new-memory 0x2)
                 :memory (new-memory 0x1000)

                 ;; each byte of video memory is a 8-pixel strip, one bit per pixel
                 ;; so we can compress our width by 8, but our height is one byte
                 ;; per pixel.
                 :video (new-memory (* (/ 64 8) 32))

                 :rtc-ms 0x0
                 :delay (new-timer)
                 :sound (new-timer)

                 :hz (* opts.mhz 1000)
                 :step step

                 :quirks quirks

                 ;;TODO should reset machine, or really just not be a machine
                 ;;method and create a new machine each time.
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

    (opts.devices.keyboard (fn [key-word]
                             (machine:write-words keyboard-mem-loc [key-word])))
    (opts.devices.video (fn []
                          (values
                            (read-bytes machine.video 0 machine.video.size)
                            {:width 64 :height 32})))

    (machine:write-bytes 0x0 bootloader)
    (machine:write-bytes font-mem-loc font-bytes)

    machine))

{: new
 : step
 : dump-bytes
 : dump-memory}
