command! -nargs=1 -complete=dir CHIP8Open
      \ :lua require("chip8")["open-picker"](<q-args>)
