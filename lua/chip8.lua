local bit = require("bit")
local _local_1_ = require("chip8.machine")
local new = _local_1_["new"]
local dump_bytes = _local_1_["dump-bytes"]
local dump_memory = _local_1_["dump-memory"]
local read_video_ram = nil
local poke_keyboard_mem = nil
local keys_down = 0
local function key_pressed(key)
  keys_down = bit.bor(keys_down, bit.lshift(1, key))
  poke_keyboard_mem(keys_down)
  local function _2_()
    keys_down = bit.bxor(keys_down, bit.lshift(1, key))
    return poke_keyboard_mem(keys_down)
  end
  return vim.defer_fn(_2_, (16 * 4))
end
local smear = 1
local crt
local _4_
do
  local _3_
  do
    local tbl_17_auto = {}
    local i_18_auto = #tbl_17_auto
    for i = 0, (32 * 64) do
      local val_19_auto = 0
      if (nil ~= val_19_auto) then
        i_18_auto = (i_18_auto + 1)
        do end (tbl_17_auto)[i_18_auto] = val_19_auto
      else
      end
    end
    _3_ = tbl_17_auto
  end
  _3_[0] = 0
  _4_ = _3_
end
local _6_
do
  local tbl_17_auto = {}
  local i_18_auto = #tbl_17_auto
  for i = 1, smear do
    local val_19_auto
    do
      local _7_
      do
        local tbl_17_auto0 = {}
        local i_18_auto0 = #tbl_17_auto0
        for i0 = 0, (32 * 64) do
          local val_19_auto0 = 0
          if (nil ~= val_19_auto0) then
            i_18_auto0 = (i_18_auto0 + 1)
            do end (tbl_17_auto0)[i_18_auto0] = val_19_auto0
          else
          end
        end
        _7_ = tbl_17_auto0
      end
      _7_[0] = 0
      val_19_auto = _7_
    end
    if (nil ~= val_19_auto) then
      i_18_auto = (i_18_auto + 1)
      do end (tbl_17_auto)[i_18_auto] = val_19_auto
    else
    end
  end
  _6_ = tbl_17_auto
end
crt = {current = _4_, phosphor = _6_}
local ns = vim.api.nvim_create_namespace("some-chip-8-ns")
local function draw(tbuf)
  local all_bytes, _let_10_ = read_video_ram()
  local _let_11_ = _let_10_
  local width = _let_11_["width"]
  local height = _let_11_["height"]
  local bit_list
  do
    local all = {idx = 0}
    for _, byte in ipairs(all_bytes) do
      for bit_pos = 0, 7 do
        local _12_
        if (0 < bit.band(bit.lshift(1, (8 - bit_pos - 1)), byte, 255)) then
          _12_ = 1
        else
          _12_ = 0
        end
        all[all.idx] = _12_
        all.idx = (1 + all.idx)
      end
      all = all
    end
    bit_list = all
  end
  local _
  bit_list.idx = nil
  _ = nil
  local chars = {}
  local previous_frame
  do
    local _14_
    do
      local tbl_17_auto = {}
      local i_18_auto = #tbl_17_auto
      for i = 0, (32 * 64) do
        local val_19_auto = 0
        if (nil ~= val_19_auto) then
          i_18_auto = (i_18_auto + 1)
          do end (tbl_17_auto)[i_18_auto] = val_19_auto
        else
        end
      end
      _14_ = tbl_17_auto
    end
    _14_[0] = 0
    previous_frame = _14_
  end
  for _0, ghost in ipairs(crt.phosphor) do
    for i = 0, ((32 * 64) - 1) do
      previous_frame[i] = bit.bor(previous_frame[i], ghost[i])
    end
  end
  for i = smear, 2, -1 do
    crt.phosphor[i] = crt.phosphor[(i - 1)]
  end
  crt.current = bit_list
  crt.phosphor[1] = crt.current
  for r = 0, 31, 2 do
    for c = 0, 63, 1 do
      local bright_top = crt.current[(((r + 0) * 64) + c)]
      local bright_bot = crt.current[(((r + 1) * 64) + c)]
      local dim_top = previous_frame[(((r + 0) * 64) + c)]
      local dim_bot = previous_frame[(((r + 1) * 64) + c)]
      local bright_mask = bit.lshift(bit.bor(bit.lshift(bright_top, 1), bright_bot), 4)
      local dim_mask = bit.bor(bit.lshift(dim_top, 1), dim_bot)
      local mask = bit.bor(bright_mask, dim_mask)
      local char
      if (mask == 0) then
        char = " "
      elseif (mask == 1) then
        char = "\240\159\174\143"
      elseif (mask == 16) then
        char = "\226\150\132"
      elseif (mask == 17) then
        char = "\226\150\132"
      elseif (mask == 18) then
        char = "\240\159\174\146"
      elseif (mask == 19) then
        char = "\240\159\174\146"
      elseif (mask == 2) then
        char = "\240\159\174\142"
      elseif (mask == 32) then
        char = "\226\150\128"
      elseif (mask == 34) then
        char = "\226\150\128"
      elseif (mask == 33) then
        char = "\240\159\174\145"
      elseif (mask == 35) then
        char = "\240\159\174\145"
      elseif (mask == 3) then
        char = "\240\159\174\144"
      elseif (mask == 48) then
        char = "\226\150\136"
      elseif (mask == 49) then
        char = "\226\150\136"
      elseif (mask == 50) then
        char = "\226\150\136"
      elseif (mask == 51) then
        char = "\226\150\136"
      elseif true then
        local _0 = mask
        print("x", bit.tohex(mask, 2))
        char = "x"
      else
        char = nil
      end
      local b_char
      if (bright_mask == 0) then
        b_char = " "
      elseif (bright_mask == 16) then
        b_char = "\226\150\132"
      elseif (bright_mask == 32) then
        b_char = "\226\150\128"
      elseif (bright_mask == 48) then
        b_char = "\226\150\136"
      else
        b_char = nil
      end
      local d_char
      if (dim_mask == 0) then
        d_char = " "
      elseif (dim_mask == 1) then
        d_char = "\226\150\132"
      elseif (dim_mask == 2) then
        d_char = "\226\150\128"
      elseif (dim_mask == 3) then
        d_char = "\226\150\136"
      else
        d_char = nil
      end
      table.insert(chars, {b_char, d_char, char})
    end
  end
  vim.api.nvim_buf_clear_namespace(tbuf, ns, 0, 16)
  for row = 0, ((32 / 2) - 1) do
    for col = 0, ((64 / 1) - 1) do
      local _let_19_ = chars[((row * 64) + col + 1)]
      local b_char = _let_19_[1]
      local d_char = _let_19_[2]
      local char = _let_19_[3]
      local vtext = {}
      vim.api.nvim_buf_set_extmark(tbuf, ns, row, col, {virt_text = {{" ", "Normal"}}, virt_text_pos = "overlay"})
      if (" " ~= b_char) then
        vim.api.nvim_buf_set_extmark(tbuf, ns, row, col, {virt_text = {{b_char, "Normal"}}, virt_text_pos = "overlay"})
      else
      end
      if (" " ~= d_char) then
        vim.api.nvim_buf_set_extmark(tbuf, ns, row, col, {virt_text = {{d_char, "Normal"}}, virt_text_pos = "overlay"})
      else
      end
    end
  end
  return nil
end
local function run(path, _3foptions)
  local options = vim.tbl_extend("keep", (_3foptions or {}), {mhz = 1, compatibility = "CHIP-8", keys = {["1"] = 1, ["2"] = 2, ["3"] = 3, ["4"] = 12, q = 4, w = 5, e = 6, r = 13, a = 7, s = 8, d = 9, f = 14, z = 10, x = 0, c = 11, v = 15}})
  local _
  if ("string" == type(options.keys)) then
    local ks = vim.split(options.keys, "")
    local m
    do
      local _22_ = {}
      _22_[ks[1]] = 1
      _22_[ks[2]] = 2
      _22_[ks[3]] = 3
      _22_[ks[4]] = 12
      _22_[ks[5]] = 4
      _22_[ks[6]] = 5
      _22_[ks[7]] = 6
      _22_[ks[8]] = 13
      _22_[ks[9]] = 7
      _22_[ks[10]] = 8
      _22_[ks[11]] = 9
      _22_[ks[12]] = 14
      _22_[ks[13]] = 10
      _22_[ks[14]] = 0
      _22_[ks[15]] = 11
      _22_[ks[16]] = 15
      m = _22_
    end
    options.keys = m
    _ = nil
  else
    _ = nil
  end
  local _0 = print(vim.inspect(options))
  local m
  local function _24_(_241)
    read_video_ram = _241
    return nil
  end
  local function _25_(_241)
    poke_keyboard_mem = _241
    return nil
  end
  m = new({mhz = options.mhz, devices = {video = _24_, keyboard = _25_}})
  local buf = vim.api.nvim_create_buf(false, true)
  local win
  local _26_
  if options["rom-name"] then
    _26_ = ("CHIP-8 :: " .. options["rom-name"])
  else
    _26_ = "CHIP-8"
  end
  win = vim.api.nvim_open_win(buf, true, {width = 64, height = (32 / 2), relative = "editor", row = ((vim.api.nvim_win_get_height(0) / 2) - 8), col = ((vim.api.nvim_win_get_width(0) / 2) - 32), style = "minimal", border = "rounded", title = _26_})
  local augroup_name = ("chip-8-augroup-" .. buf)
  local augroup = vim.api.nvim_create_augroup(augroup_name, {clear = true})
  m["load-rom"](m, path)
  vim.api.nvim_buf_set_option(buf, "filetype", "chip8")
  local function _28_()
    local tbl_17_auto = {}
    local i_18_auto = #tbl_17_auto
    for i = 1, 32 do
      local val_19_auto
      do
        local s = ""
        for i0 = 1, 64 do
          s = (s .. " ")
        end
        val_19_auto = s
      end
      if (nil ~= val_19_auto) then
        i_18_auto = (i_18_auto + 1)
        do end (tbl_17_auto)[i_18_auto] = val_19_auto
      else
      end
    end
    return tbl_17_auto
  end
  vim.api.nvim_buf_set_lines(buf, 0, -1, true, _28_())
  vim.api.nvim_buf_set_option(buf, "modifiable", false)
  vim.api.nvim_buf_set_option(buf, "bufhidden", "wipe")
  local function _30_()
    return vim.cmd(string.format(":mapclear <buffer>", buf))
  end
  vim.api.nvim_buf_call(buf, _30_)
  for key, keycode in pairs(options.keys) do
    local function _31_()
      return key_pressed(keycode)
    end
    vim.api.nvim_buf_set_keymap(buf, "n", key, "", {callback = _31_})
  end
  local time = vim.loop.now()
  local function tick()
    if vim.api.nvim_buf_is_valid(buf) then
      do
        local now = vim.loop.now()
        local delta = (now - time)
        time = now
        m:step(delta)
        draw(buf)
      end
      return vim.defer_fn(tick, (1000 / 60))
    else
      return nil
    end
  end
  vim.defer_fn(tick, (1000 / 60))
  return true
end
local function open_picker(rom_dir, _3foptions)
  local _let_33_ = require("telescope.builtin")
  local find_files = _let_33_["find_files"]
  local actions = require("telescope.actions")
  local action_state = require("telescope.actions.state")
  local _let_34_ = require("telescope.themes")
  local get_dropdown = _let_34_["get_dropdown"]
  local function _35_(buf, map)
    local function _36_()
      local _let_37_ = action_state.get_selected_entry()
      local selection = _let_37_[1]
      actions.close(buf)
      return run(vim.fs.normalize((rom_dir .. "/" .. selection)), _3foptions)
    end
    do end (actions.select_default):replace(_36_)
    return true
  end
  return find_files(get_dropdown({prompt_title = "CHIP-8 ROM", cwd = rom_dir, attach_mappings = _35_, previewer = false}))
end
return {["open-picker"] = open_picker, run = run}