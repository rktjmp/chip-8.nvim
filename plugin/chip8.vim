command! -nargs=1 -complete=dir Chip8Open
      \ :lua require("chip8")["open-picker"](<q-args>)
