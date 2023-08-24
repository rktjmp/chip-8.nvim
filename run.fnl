(local bit (require :bit))
(local {: view} (require :fennel))
(local {: new : dump-bytes : dump-memory} (require :chip8.machine))
(local {: read-bytes} (require :chip8.memory))

(fn tobin [n len]
  (faccumulate [s "" i 0 (- len 1)]
    (let [b (-> (bit.rshift n i) (bit.band 0x1))]
      (.. b s))))

(fn block-draw [video-ram]
  (local lines [])
  (for [row-num 0 31]
    (let [row (read-bytes video-ram (* row-num (/ 64 8)) (/ 64 8))
          str (accumulate [line (string.format "%02d:" row-num) byte-num byte (ipairs row)]
                (faccumulate [chunk line bit-pos 0 7]
                  (if (< 0  (-> (bit.lshift 1 (- 8 bit-pos 1))
                                (bit.band byte 0xFF)))
                    (.. chunk "█")
                    (.. chunk " "))))]
      (table.insert lines str)))
  (-> (table.concat lines "\n") (print)))

(fn half-block-draw [video-ram]
  (let [all-bytes (read-bytes video-ram 0 (* (/ 64 8) 32))
        ;; indexing by zero makes striding simpler
        bit-list (accumulate [all {:idx 0} _ byte (ipairs all-bytes)]
                   (do
                     (for [bit-pos 0 7]
                       (tset all all.idx (if (< 0 (-> (bit.lshift 1 (- 8 bit-pos 1))
                                                      (bit.band byte 0xFF)))
                                           1 0))
                       (set all.idx (+ 1 all.idx)))
                     all))
        _ (set bit-list.idx nil)
        bits-per-row 64
        chars []]
    (for [r 0 31 2] ; 32 rows, but we squeeze 2 rows per char
      (for [c 0 63 1] ;; 64 cols, squeeze 1 per char
        (let [b1 (. bit-list (+ (* (+ r 0) 64) c))
              b2 (. bit-list (+ (* (+ r 1) 64) c))
              c (case (values b1 b2)
                  (0 0) " "
                  (1 0) "▀"
                  (0 1) "▄"
                  (1 1) "█")]
          (table.insert chars c))))
    (let [lines (fcollect [row 0 (- (/ 32 2) 1)]
                  (faccumulate [line "" col 0 (- (/ 64 1) 1)]
                    (.. line (. chars (+ (* row (/ 64 1)) col 1)))))]
      (print (table.concat lines "\n")))))

(fn braille-draw [video-ram]
  (let [all-bytes (read-bytes video-ram 0 (* (/ 64 8) 32))
        ;; indexing by zero makes striding simpler
        bit-list (accumulate [all {:idx 0} _ byte (ipairs all-bytes)]
                   (do
                     (for [bit-pos 0 7]
                       (tset all all.idx (if (< 0 (-> (bit.lshift 1 (- 8 bit-pos 1))
                                                      (bit.band byte 0xFF)))
                                           1 0))
                       (set all.idx (+ 1 all.idx)))
                     all))
        _ (set bit-list.idx nil)
        bits-per-row 64
        chars []]
    (for [r 0 31 4] ; 32 rows, but we squeeze 4 rows per char
      (for [c 0 63 2] ;; 64 cols, squeeze 2 per char
        ;; braille has the binary mapping
        ;; 1 4
        ;; 2 5
        ;; 3 6
        ;; 7 8
        (let [
              b1 (. bit-list (+ (* (+ r 0) 64) c))
              b4 (. bit-list (+ (* (+ r 0) 64) (+ c 1)))
              b2 (. bit-list (+ (* (+ r 1) 64) c))
              b5 (. bit-list (+ (* (+ r 1) 64) (+ c 1)))
              b3 (. bit-list (+ (* (+ r 2) 64) c))
              b6 (. bit-list (+ (* (+ r 2) 64) (+ c 1)))
              b7 (. bit-list (+ (* (+ r 3) 64) c))
              b8 (. bit-list (+ (* (+ r 3) 64) (+ c 1)))
              num (bit.bor (bit.lshift b8 7)
                           (bit.lshift b7 6)
                           (bit.lshift b6 5)
                           (bit.lshift b5 4)
                           (bit.lshift b4 3)
                           (bit.lshift b3 2)
                           (bit.lshift b2 1)
                           (bit.lshift b1 0))
              code-point (bit.bor 0x2800 num)
              third-byte (-> (bit.band code-point 0x3F) ;; last 6 bits
                             (bit.bor 0x80)) ;; prepend 0b10
              second-byte (-> (bit.rshift code-point 6)
                              (bit.band 0x3F)
                              (bit.bor 0x80)) ;; prepend 0b10
              first-byte (-> (bit.rshift code-point 12)
                             (bit.band 0x3F)
                             (bit.bor 0xE0)) ;; prepend 0b1110
              unicode-bytes (if (< 0 num) [first-byte second-byte third-byte] 0)]
          (table.insert chars unicode-bytes))))
    (let [lines (fcollect [row 0 (- (/ 32 4) 1)]
                  (faccumulate [line "" col 0 (- (/ 64 2) 1)]
                    (case (. chars (+ (* row (/ 64 2)) col 1))
                      0 (.. line " ")
                      [a b c] (.. line (string.char a b c)))))]
      (print (table.concat lines "\n")))))


; (let [m (new)]
;   (m:write-index 0x0)
;   (m:write-register 0x0 0x0)
;   (m:write-register 0x1 0x0)
;   (m:write-words 0x200 [0xD015])
;   (m:step)
;   (block-draw m.video)
;   (braille-draw m.video)
;   )

(let [m (new)]
  (m:load-rom "./chip8-test-suite/bin/3-corax+.ch8")
  (for [i 1 180]
    (m:step))
  (block-draw m.video)
  (half-block-draw m.video)
  (braille-draw m.video))

