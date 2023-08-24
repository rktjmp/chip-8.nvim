(local t (require :faith))
(local bit (require :bit))
(local M (require :chip8.machine))
(local {: read-words : read-bytes} (require :chip8.memory))

(local tests {})

(macro == [a b ?msg]
  `(t.= ,a ,b ,?msg))

(macro test [test-name [machine-name] ...]
  `(fn ,(sym (.. :tests.test- test-name)) []
    (let [,machine-name (M.new)]
      ,...)))

(test :new [m]
  (== m.pc 0x0200)
  (== m.sp 0x0200)
  ; (== 16 (length m.v))
  ; (== 0 (accumulate [t 0 _ v (ipairs m.v)] (+ t v)))
  ; (== 0 (accumulate [t 0 _ v (ipairs m.gfx)] (+ t v)))
  ; (== 0 (accumulate [t 0 _ v (ipairs m.memory)] (+ t v)))
  )

(test :ret [m]
  "Return from a subroutine"
  (m:write-bytes m.pc [0x00 0x00   ;; 0x0200
                       0x00 0xEE   ;; 0x0202
                       0x00 0x00   ;; 0x0204
                       0x00 0x00]) ;; 0x0206
  (m:stack-push 0x0200)
  (set m.pc 0x0202)

  (m:step)
  (== m.pc 0x0200))

(test :jp-nnn [m]
  "Jump to address NNN"
  ;; setup
  (m:write-bytes m.pc [0x14 0x32])

  ;; check
  (m:step)
  (== m.pc 0x0432 "the pc has jumped")
  (== m.sp 0x0200 "the stack remains unaltered"))

(test :call-nnn [m]
  "Execute subroutine starting at address NNN"
  ;; setup
  (== m.pc 0x0200)
  (m:write-bytes m.pc [0x24 0x32])

  ;; check
  (m:step)
  (== m.pc 0x0432 "the pc has jumped")
  (== m.sp 0x01FE "the stack pointer has moved down 2 bytes")
  (== [0x0202] (m:read-words m.sp 1)
      "the stack pointer will return to the next instruction"))

(test :se-vx-nn [m]
  "Skip the following instruction if the value of register VX equals NN"
  ;; setup
  (== m.pc 0x0200)
  (m:write-bytes m.pc [0x31 0x42   ;; 0x0200
                       0x00 0x00   ;; 0x0202
                       0x31 0x00   ;; 0x0204
                       0x00 0x01]) ;; 0x0206
  (m:write-register 0x1 0x42)

  ;; check
  (m:step)
  (== m.pc 0x0204 "skipped 0x0202")
  (m:step)
  (== m.pc 0x0206))

(test :sne-vx-nn [m]
  "Skip the following instruction if the value of register VX is not equal to NN"
  ;; setup
  (== m.pc 0x0200)
  (m:write-bytes m.pc [0x41 0x00   ;; 0x0200
                       0x00 0x00   ;; 0x0202
                       0x41 0x42   ;; 0x0204
                       0x00 0x01]) ;; 0x0206
  (m:write-register 0x1 0x42)

  ;; check
  (m:step)
  (== m.pc 0x0204 "skipped 0x0202")
  (m:step)
  (== m.pc 0x0206))

(test :sne-vx-vy [m]
  "Skip the following instruction if the value of register VX is equal to the value of register VY"
  ;; setup
  (== m.pc 0x0200)
  (m:write-bytes m.pc [0x51 0x20   ;; 0x0200
                       0x00 0x00   ;; 0x0202
                       0x51 0x30   ;; 0x0204
                       0x00 0x01]) ;; 0x0206
  (m:write-register 0x1 0x42)
  (m:write-register 0x2 0x42)
  (m:write-register 0x3 0x00)

  ;; check
  (m:step)
  (== m.pc 0x0204 "skipped 0x0202")
  (m:step)
  (== m.pc 0x0206))

(test :ld-vx-nn [m]
  "Store number NN in register VX"
  (m:write-words m.pc [0x6120])
  (m:step)
  (== 0x20 (m:read-register 1)))

(test :add-vx-nn [m]
  "Add the value NN to register VX"
  (m:write-register 1 0x20)
  (m:write-words m.pc [0x7120])
  (== 0x20 (m:read-register 1))
  (m:step)
  (== 0x40 (m:read-register 1)))

(test :ld-vx-vy [m]
  "Store the value of register VY in register VX"
  ;; 8XY0
  (m:write-register 1 0x00)
  (m:write-register 2 0x20)
  (m:write-words m.pc [0x8120])
  (m:step)
  (== 0x20 (m:read-register 1))
  (== 0x20 (m:read-register 2)))

(test :or-vx-vy [m]
  "Set VX to VX OR VY"
  ;; 8XY1
  (m:write-register 1 0x40)
  (m:write-register 2 0x02)
  (m:write-words m.pc [0x8121])
  (m:step)
  (== 0x42 (m:read-register 1)))

(test :and-vx-vy [m]
  "Set VX to VX AND VY"
  ;; 8XY2
  (m:write-register 1 0x40)
  (m:write-register 2 0x42)
  (m:write-words m.pc [0x8122])
  (m:step)
  (== 0x40 (m:read-register 1)))

(test :xor-vx-vy [m]
  "Set VX to VX XOR VY"
  ;; 8XY3
  (m:write-register 1 0x40)
  (m:write-register 2 0x42)
  (m:write-words m.pc [0x8123])
  (m:step)
  (== 0x02 (m:read-register 1)))

(test :add-vx-vy [m]
  "Add the value of register VY to register VX, set VF to 01 if a carry occurs"
  ;; 8XY4
  (m:write-register 1 0x40)
  (m:write-register 2 0x40)
  (m:write-words m.pc [0x8124])
  (m:step)
  (== 0x80 (m:read-register 1))
  (== 0x0 (m:read-register 0xF))

  (m:write-register 1 0xFE)
  (m:write-register 2 0x01)
  (m:write-words m.pc [0x8124])
  (m:step)
  (== 0xFF (m:read-register 1))
  (== 0x0 (m:read-register 0xF))

  (m:write-register 1 0xFF)
  (m:write-register 2 0x02)
  (m:write-words m.pc [0x8124])
  (m:step)
  (== 0x1 (m:read-register 1))
  (== 0x1 (m:read-register 0xF)))

(test :sub-vx-vy [m]
  "Subtract the value of register VY from register VX, set VF to 00 if a borrow occurs"
  ;; 8XY5
  (m:write-register 1 0x40)
  (m:write-register 2 0x10)
  (m:write-words m.pc [0x8125])
  (m:step)
  (== 0x30 (m:read-register 1))
  (== 0x1 (m:read-register 0xF))

  (m:write-register 1 0x40)
  (m:write-register 2 0x40)
  (m:write-words m.pc [0x8125])
  (m:step)
  (== 0x0 (m:read-register 1))
  (== 0x1 (m:read-register 0xF))

  (m:write-register 1 0x00)
  (m:write-register 2 0x40)
  (m:write-words m.pc [0x8125])
  (m:step)
  (== 0xC0 (m:read-register 1))
  (== 0x0 (m:read-register 0xF)))

(test :shr-vx-vy [m]
  "Store the value of register VY shifted right one bit in register VX, set VF to the least significant bit prior to the shift"
  ;; 8XY6
  (m:write-register 1 0x00)
  (m:write-register 2 0x02)
  (m:write-register 0xF 0x00)
  (m:write-words m.pc [0x8126])
  (m:step)
  (== 0x01 (m:read-register 1))
  (== 0x00 (m:read-register 0xF))

  (m:write-register 1 0x00)
  (m:write-register 2 0x03)
  (m:write-register 0xF 0x00)
  (m:write-words m.pc [0x8126])
  (m:step)
  (== 0x01 (m:read-register 1))
  (== 0x01 (m:read-register 0xF)))

(test :subn-vx-vy [m]
  "Set register VX to the value of VY minus VX, set VF to 00 if a borrow occurs"
  ;; 8XY7
  (m:write-register 1 0x10)
  (m:write-register 2 0x40)
  (m:write-words m.pc [0x8127])
  (m:step)
  (== 0x30 (m:read-register 1))
  (== 0x1 (m:read-register 0xF))

  (m:write-register 1 0x40)
  (m:write-register 2 0x40)
  (m:write-words m.pc [0x8127])
  (m:step)
  (== 0x0 (m:read-register 1))
  (== 0x1 (m:read-register 0xF))

  (m:write-register 1 0x40)
  (m:write-register 2 0x00)
  (m:write-words m.pc [0x8127])
  (m:step)
  (== 0xC0 (m:read-register 1))
  (== 0x0 (m:read-register 0xF)))

(test :shl-vx-vy [m]
  "Store the value of register VY shifted left one bit in register VX, set VF to the most significant bit prior to the shift"
  ;; 8XYE
  (m:write-register 1 0x00)
  (m:write-register 2 0xFF)
  (m:write-register 0xF 0x00)
  (m:write-words m.pc [0x812E])
  (m:step)
  (== 0xFE (m:read-register 1))
  (== 0x01 (m:read-register 0xF))

  (m:write-register 1 0x00)
  (m:write-register 2 0x7F)
  (m:write-register 0xF 0x00)
  (m:write-words m.pc [0x812E])
  (m:step)
  (== 0xFE (m:read-register 1))
  (== 0x00 (m:read-register 0xF)))

(test :sne-vx-vy [m]
  "Skip the following instruction if the value of register VX is not equal to the value of register VY"
  ;; 9XY0
  (== m.pc 0x0200)
  (m:write-words m.pc [0x9120 ;; 0x0200
                       0x9130 ;; 0x0202
                       0xFFFF ;; 0x0204
                       0x0000]) ;; 0x0206
  (m:write-register 0x1 0x42)
  (m:write-register 0x2 0x42)
  (m:write-register 0x3 0xFF)
  (m:step)
  (== 0x0202 m.pc)
  (m:step)
  (== 0x0206 m.pc))

(test :ld-i-nnn [m]
  "Store memory address NNN in register I"
  ;; ANNN
  (m:write-words m.pc [0xA123])
  (m:step)
  (== 0x0123 (m:read-index)))

(test :jp-v0-nnn [m]
  "Jump to address NNN + V0"
  ;; BNNN
  (== m.pc 0x0200)
  (m:write-words m.pc [0xB400])
  (m:write-register 0x0 0xFF)
  (m:step)
  (== 0x04FF m.pc))

(test :rnd-vx-nn [m]
  "Set VX to a random number with a mask of NN"
  ;; CXNN
  (m:write-words m.pc [0xC0F0])
  (m:step)
  (local x (m:read-register 0))
  (== true (< 0x0F (m:read-register 0) 0x100)))

(test :drw-vx-vy-n-aligned [m]
  "Draw a sprite at position VX, VY with N bytes of sprite data starting at the address stored in I, set VF to 01 if any set pixels are changed to unset, and 00 otherwise"
  ;; DXYN

  ;; aligned draw
  (m:write-register 0 16)
  (m:write-register 1 1)
  (m:write-index 0x500)
  (m:write-bytes 0x500 [0xAB 0xCE])
  (m:write-words 0x200 [0xD012])
  (m:step)
  (== [0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00
       0x00 0x00 0xAB 0x00 0x00 0x00 0x00 0x00
       0x00 0x00 0xCE 0x00 0x00 0x00 0x00 0x00
       0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00]
      (read-bytes m.video 0 (* 4 8)))
  (== 0x0 (m:read-register 0xF)))

(test :drw-vx-vy-n-misaligned [m]
  ;; misaligned draw
  (m:write-register 0 4)
  (m:write-register 1 0)
  (m:write-index 0x500)
  (m:write-bytes 0x500 [0xAB 0xAB 0xAB])
  (m:write-words 0x200 [0xD013])
  (m:step)
  (== [0x0a 0xb0 0x00 0x00 0x00 0x00 0x00 0x00
       0x0a 0xb0 0x00 0x00 0x00 0x00 0x00 0x00
       0x0a 0xb0 0x00 0x00 0x00 0x00 0x00 0x00]
      (read-bytes m.video 0 (* 3 8)))
  (== 0x0 (m:read-register 0xF)))

(test :drw-vx-vy-n-clipped-y [m]
  ;; clipped tail
  (m:write-register 0 0)
  (m:write-register 1 30)
  (m:write-index 0x500)
  (m:write-bytes 0x500 [0xAB 0xCE 0xDF])
  (m:write-words 0x200 [0xD013])
  (m:step)
  (== [0xAB 0x00 0x00 0x00 0x00 0x00 0x00 0x00
       0xCE 0x00 0x00 0x00 0x00 0x00 0x00 0x00]
      (read-bytes m.video (* 30 8) (* 2 8)))
  (== 0x0 (m:read-register 0xF)))

(test :drw-vx-vy-n-clipped-x [m]
  ;; clipped edge
  (m:write-register 0 60) ;; 4 bits left 4 bits overflow
  (m:write-register 1 0)
  (m:write-index 0x500)
  (m:write-bytes 0x500 [0xAB 0xCE])
  (m:write-words 0x200 [0xD013])
  (m:step)
  (== [0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x0a
       0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x0c
       0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00]
      (read-bytes m.video (* 0 8) (* 3 8)))
  (== 0x0 (m:read-register 0xF)))

(test :drw-vx-vy-n-unset-flag [m]
  ;; on-off flag
  (m:write-register 0 0) ;; 4 bits left 4 bits overflow
  (m:write-register 1 0)
  (m:write-index 0x500)
  (m:write-words 0x200 [0xD011 0xD011 0xD011])

  ;; first write
  (m:write-bytes 0x500 [0xF0])
  (m:step)
  (== [0xF0 0x00 0x00 0x00 0x00 0x00 0x00 0x00]
      (read-bytes m.video 0 8))
  (== 0x0 (m:read-register 0xF))

  ;; uneffective write
  (m:write-bytes 0x500 [0x0F])
  (m:step)
  (== [0xFF 0x00 0x00 0x00 0x00 0x00 0x00 0x00]
      (read-bytes m.video 0 8))
  (== 0x0 (m:read-register 0xF))

  ;; effective write
  (m:write-bytes 0x500 [0x01])
  (m:step)
  (== [0xFE 0x00 0x00 0x00 0x00 0x00 0x00 0x00]
      (read-bytes m.video 0 8))
  (== 0x1 (m:read-register 0xF)))

(test :skp-vx [m]
  "Skip the following instruction if the key corresponding to the hex value currently stored in register VX is pressed"
  ;; EX9E
  nil)

(test :sknp-vx [m]
  "Skip the following instruction if the key corresponding to the hex value currently stored in register VX is not pressed"
  ;; EXA1
  nil)

(test :ld-vx-dt [m]
  "Store the current value of the delay timer in register VX"
  ;; FX07
  nil)

(test :ld-vx-k [m]
  "Wait for a keypress and store the result in register VX"
  ;; FX0A
  nil)

(test :ld-dt-vx [m]
  "Set the delay timer to the value of register VX"
  ;; FX15
  nil)

(test :ld-st-vx [m]
  "Set the sound timer to the value of register VX"
  ;; FX18
  nil)

(test :add-i-vx [m]
  "Add the value stored in register VX to register I"
  ;; FX1E
  nil)

(test :ld-f-vx [m]
  "Set I to the memory address of the sprite data corresponding to the hexadecimal digit stored in register VX"
  ;; FX29
  nil)

(test :ld-b-vx [m]
  "Store the binary-coded decimal equivalent of the value stored in register VX at addresses I, I + 1, and I + 2"
  ;; FX33
  nil)

(test :ld-i-vx [m]
  "Store the values of registers V0 to VX inclusive in memory starting at address I, set I to I + X + 1 after operation"
  ;; FX55
  nil)

(test :ld-vx-i [m]
  "Fill registers V0 to VX inclusive with the values stored in memory starting at address I, set I to I + X + 1 after operation"
  ;; FX65
  nil)

tests
