(set package.path (.. "./fnl/?.lua;" package.path))
(local bit (require :bit))
(local {: view} (require :fennel))
(local {: new : dump-bytes : dump-memory} (require :chip8.machine))

(var read-video-ram nil)
(var poke-keyboard-mem nil)

(var keys-down 0x0)

(fn key-pressed [key]
  (set keys-down (bit.bor keys-down (bit.lshift 1 key)))
  (poke-keyboard-mem keys-down)
  (vim.defer_fn #(do
                   ;; unset after 16ms since we cant catch "release"
                   (set keys-down (bit.bxor keys-down (bit.lshift 1 key)))
                   (poke-keyboard-mem keys-down))
                (* 16 4)))


(local smear 30)
(local crt {:current (doto (fcollect [i 0 (* 32 64)] 0) (tset 0 0))
            :phosphor (fcollect [i 1 smear]
                         (doto (fcollect [i 0 (* 32 64)] 0) (tset 0 0)))})

(fn draw [tbuf]
  (let [(all-bytes {: width : height}) (read-video-ram)
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
        chars []]

    (local previous-frame (doto (fcollect [i 0 (* 32 64)] 0) (tset 0 0)))
    (each [_ ghost (ipairs crt.phosphor)]
      (for [i 0 (- (* 32 64) 1)]
        (tset previous-frame i (bit.bor (. previous-frame i) (. ghost i)))))
    ; (print (vim.inspect previous-frame))

    (for [i smear 2 -1]
      (tset crt.phosphor i (. crt.phosphor (- i 1))))
    (set crt.current bit-list)
    (tset crt.phosphor 1 crt.current)

    (for [r 0 31 2] ; 32 rows, but we squeeze 2 rows per char
      (for [c 0 63 1] ;; 64 cols, squeeze 1 per char
        (let [bright-top (. crt.current (+ (* (+ r 0) 64) c))
              bright-bot (. crt.current (+ (* (+ r 1) 64) c))
              dim-top (. previous-frame (+ (* (+ r 0) 64) c))
              dim-bot (. previous-frame (+ (* (+ r 1) 64) c))
              bright-mask (-> (bit.bor (bit.lshift bright-top 1) bright-bot) (bit.lshift 4))
              dim-mask (-> (bit.bor (bit.lshift dim-top 1) dim-bot))
              mask (bit.bor bright-mask dim-mask)
              char (case mask
                  0x00 " "

                  0x01 "🮏"
                  0x10 "▄"
                  0x11 "▄"
                  0x12 "🮒"
                  0x13 "🮒"

                  0x02 "🮎"
                  0x20 "▀"
                  0x22 "▀"
                  0x21 "🮑"
                  0x23 "🮑"

                  0x03 "🮐"
                  0x30 "█"
                  0x31 "█"
                  0x32 "█"
                  0x33 "█"

                  _ (do 
                      (print :x (bit.tohex mask 2))

                      "x"))]

          (table.insert chars char))
        ))
    (let [lines (fcollect [row 0 (- (/ 32 2) 1)]
                  (faccumulate [line "" col 0 (- (/ 64 1) 1)]
                    (.. line (. chars (+ (* row (/ 64 1)) col 1)))))]
      (vim.api.nvim_buf_set_option tbuf :modifiable true)
      (vim.api.nvim_buf_set_lines tbuf 0 -1 true lines)
      (vim.cmd :redraw)
      (vim.api.nvim_buf_set_option tbuf :modifiable false))))

(fn run [path]
  (let [m (new {:mhz 1 :devices {:video #(set read-video-ram $1)
                                 :keyboard #(set poke-keyboard-mem $1)}})
        buf (vim.api.nvim_create_buf false true)
        win (vim.api.nvim_open_win buf true
                                   {:width 64
                                    :height (/ 32 2) ;; /2 for half block renderer
                                    :relative :editor
                                    :row 2
                                    :col 2
                                    :style :minimal
                                    :border :rounded
                                    :title "CHIP-8"})
        augroup-name (.. "chip-8-augroup-" buf)
        augroup (vim.api.nvim_create_augroup augroup-name {:clear true})]
    ;(m:load-rom "./chip8-test-suite/bin/5-quirks.ch8")
    ; (m:load-rom "/home/soup/projects/chip8.nvim/chip-8-games/Chip-8-Demos/1-Demos/Boot Super Chip8x (by Ersanio)(2018).ch8")
    (m:load-rom path)
    (vim.api.nvim_buf_set_option buf :filetype :chip8)
    (vim.api.nvim_buf_set_option buf :modifiable false)
    (vim.api.nvim_buf_set_option buf :bufhidden :wipe)
    (each [key keycode (pairs {:1 0x1 :2 0x2 :3 0x3 :4 0xC
                               :q 0x4 :w 0x5 :f 0x6 :p 0xD
                               :a 0x7 :r 0x8 :s 0x9 :t 0xE
                               :z 0xA :x 0x0 :c 0xB :d 0xF})]
      (vim.api.nvim_buf_set_keymap buf :n key "" {:callback #(key-pressed keycode)}))
    (vim.api.nvim_buf_call buf #(vim.cmd :mapclear))
    (var time (vim.loop.now))
    (fn tick []
      (when (vim.api.nvim_buf_is_valid buf)
        (let [now (vim.loop.now)
              delta (- now time)]
          (set time now)
          (m:step delta)
          (draw buf))
        (vim.defer_fn tick (/ 1000 60))))
    (vim.defer_fn tick (/ 1000 60))

    true))

(let [{: find_files} (require :telescope.builtin)
      actions (require :telescope.actions)
      action-state (require :telescope.actions.state)
      {: get_dropdown} (require :telescope.themes)]

  (find_files (get_dropdown {:prompt_title "CHIP-8 ROM"
                             :previewer false
               :attach_mappings   (fn [buf map]
                                    (actions.select_default:replace
                                      (fn []
                                        (let [[selection] (action-state.get_selected_entry)]
                                          (actions.close buf)
                                          (run selection))))
                                    (values true))})))
