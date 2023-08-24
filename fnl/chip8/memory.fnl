(macro assert-word [x]
  `(assert (<= 0 ,x 0xFFFF) "expected word"))

(local {: view} (require :fennel))
(local ffi (require :ffi))

(fn dump [memory ?offset ?len]
  (print :block (view (fcollect [i (or ?offset 0)
                                 (or ?len (- memory.size 1))]
                        (. memory.block i)))))
(fn new [size]
  (let [block (ffi.new "uint8_t[?]" size)]
    (for [i 0 (- size 1)]
      (tset block i 0))
    {: size
     : block}))

(fn read-bytes [memory offset count ?stride]
  (assert (<= (+ offset count) memory.size)
          "Error: read-bytes attempt to read past memory limit")
  (let [stride (or ?stride 1)]
    (fcollect [i 0 (- count 1)]
      (. memory.block (+ offset (* i stride))))))

(fn write-bytes [memory offset bytes ?stride]
  (assert (<= (+ offset (length bytes)) memory.size)
          "Error: write-bytes attempt to write past memory limit")
  (let [stride (or ?stride 1)]
    (for [i 0 (- (length bytes) 1)]
      (tset memory.block (+ offset (* i stride)) (. bytes (+ i 1))))))

(fn read-words [memory offset count]
  (fcollect [i offset (- (+ offset (* count 2)) 1) 2]
    (bit.bor (bit.lshift (. memory.block i) 8)
              (. memory.block (+ i 1)))))

(fn write-words [memory offset words]
  (each [i w (ipairs words)]
    (let [index (* 2 (- i 1))]
      (tset memory.block
            (+ offset index)
            (bit.band (bit.rshift w 8) 0xFF))
      (tset memory.block
            (+ offset index 1)
            (bit.band w 0xFF))))
  memory)

(fn nibbles->word [n1 n2 n3 n4]
  "join 4x4 into 16 bits"
  (bit.bor
    (bit.lshift n1 12)
    (bit.lshift n2 8)
    (bit.lshift n3 4)
    n4))

(fn nibbles->byte [n1 n2]
  "join 2x4 into 8 bits"
  (bit.bor (bit.lshift n1 4) n2))

(fn word->nibbles [word]
  "split 16 bits into 4x4"
  [(-> (bit.rshift word 12)
       (bit.band 0x000F))
   (-> (bit.rshift word 8)
       (bit.band 0x000F))
   (-> (bit.rshift word 4)
       (bit.band 0x000F))
   (-> (bit.rshift word 0)
       (bit.band 0x000F))])

(fn byte->nibbles [byte]
  "split 8 bits into 2x4"
  [(-> (bit.rshift byte 4)
       (bit.band 0x000F))
   (-> (bit.rshift byte 0)
       (bit.band 0x000F))])

{: new
 : read-bytes
 : write-bytes
 : read-words
 : write-words
 : nibbles->byte
 : nibbles->word
 : word->nibbles}
