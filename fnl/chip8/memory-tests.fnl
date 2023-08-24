(local t (require :faith))
(local M (require :chip8.memory))

(local tests {})

(macro == [a b ?msg]
  `(t.= ,a ,b ,?msg))

(macro test [test-name [arg-name] ...]
  `(fn ,(sym (.. :tests.test- test-name)) []
    (let [,arg-name (M.new 8)]
      ,...)))

(test :write-bytes-read-bytes [m]
  (M.write-bytes m 3 [0x1 0x02 0x03 0x04])
  (== 0x00 (. m.block 0))
  (== 0x01 (. m.block 3))
  (== 0x02 (. m.block 4))
  (== 0x03 (. m.block 5))
  (== 0x04 (. m.block 6))
  (== 0x00 (. m.block 7))
  (== [1 2 3 4] (M.read-bytes m 3 4))
  ;; stride
  (== [1 3] (M.read-bytes m 3 2 2))
  (M.write-bytes m 0 [0x00 0x00 0x00 0x00]) ;; reset
  (M.write-bytes m 0 [0xFF 0xFF] 2)
  (== [0xFF 0x00 0xFF 0x00] (M.read-bytes m 0 4)))

(test :write-words-read-words [m]
  (M.write-words m 3 [0xFACE])
  (== 0xFA (. m.block 3))
  (== 0xCE (. m.block 4))
  (local [word1 word2] (M.read-words m 3 2))
  (== 0xFACE word1)
  (== 0x0000 word2)

  (M.write-words m 3 [0x0102 0x0304])
  (== 0x00 (. m.block 0))
  (== 0x01 (. m.block 3))
  (== 0x02 (. m.block 4))
  (== 0x03 (. m.block 5))
  (== 0x04 (. m.block 6))
  (== 0x00 (. m.block 7))
  (local [word1 word2] (M.read-words m 3 2))
  (== 0x0102 word1)
  (== 0x0304 word2)

  (M.write-words m 3 [0x8 0x9])
  (== 0x00 (. m.block 0))
  (== 0x00 (. m.block 3))
  (== 0x08 (. m.block 4))
  (== 0x00 (. m.block 5))
  (== 0x09 (. m.block 6))
  (== 0x00 (. m.block 7))

  ; (M.write-words m 3 [0xDECA 0xB0D1 0xFBAD 0xFEED])
  ; (== [0xDECA 0xFBAD] (M.read-words m 3 2 2))
  )

(test :nibbles->byte [m]
 (== 0x42 (M.nibbles->byte 0x4 0x2)))

(test :nibbles->word [m]
 (== 0xFACE (M.nibbles->word 0xF 0xA 0xC 0xE))
 (== 0x333 (M.nibbles->word 0x0 0x3 0x3 0x3)))

(test :word->nibbles [m]
 (== [0x01 0x02 0x03 0x04]
     (M.word->nibbles 0x1234 true)))

tests
