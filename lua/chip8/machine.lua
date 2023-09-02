local bit = require("bit")
local _local_1_ = require("fennel")
local view = _local_1_["view"]
local _local_2_ = string
local fmt = _local_2_["format"]
local _local_3_ = require("chip8.memory")
local write_bytes = _local_3_["write-bytes"]
local read_bytes = _local_3_["read-bytes"]
local nibbles__3eword = _local_3_["nibbles->word"]
local word__3enibbles = _local_3_["word->nibbles"]
local bootloader = {89, 79, 85, 32, 67, 65, 78, 32, 80, 76, 65, 89, 32, 87, 73, 84, 72, 32, 85, 83, 0, 0, 0, 0}
local font_bytes = {240, 144, 144, 144, 240, 32, 96, 32, 32, 112, 240, 16, 240, 128, 240, 240, 16, 240, 16, 240, 144, 144, 240, 16, 16, 240, 128, 240, 16, 240, 240, 128, 240, 144, 240, 240, 16, 32, 64, 64, 240, 144, 240, 144, 240, 240, 144, 240, 16, 240, 240, 144, 240, 144, 144, 224, 144, 224, 144, 224, 240, 128, 128, 128, 240, 224, 144, 144, 144, 224, 240, 128, 240, 128, 240, 240, 128, 240, 128, 128}
local font_mem_loc = 80
local keyboard_mem_loc = 256
local function tobin(n, len)
  local s = ""
  for i = 0, (len - 1) do
    local b = bit.band(bit.rshift(n, i), 1)
    s = (b .. s)
  end
  return s
end
local function dump_memory(memory, _3foffset, _3flen)
  local start = (_3foffset or 0)
  local _end = ((start + (_3flen or memory.size)) - 1)
  local mem
  do
    local tbl_17_auto = {}
    local i_18_auto = #tbl_17_auto
    for i = start, _end, 2 do
      local val_19_auto = bit.tohex(bit.bor(bit.lshift(memory.block[i], 8), memory.block[(i + 1)]), 4)
      if (nil ~= val_19_auto) then
        i_18_auto = (i_18_auto + 1)
        do end (tbl_17_auto)[i_18_auto] = val_19_auto
      else
      end
    end
    mem = tbl_17_auto
  end
  local step = 4
  print()
  for i = 1, #mem, step do
    local block
    do
      local tbl_17_auto = {}
      local i_18_auto = #tbl_17_auto
      for ii = 0, (step - 1) do
        local val_19_auto = mem[(i + ii)]
        if (nil ~= val_19_auto) then
          i_18_auto = (i_18_auto + 1)
          do end (tbl_17_auto)[i_18_auto] = val_19_auto
        else
        end
      end
      block = tbl_17_auto
    end
    print(unpack(block))
  end
  return nil
end
local function dump_bytes(bytes)
  local mem
  do
    local tbl_17_auto = {}
    local i_18_auto = #tbl_17_auto
    for _, b in ipairs(bytes) do
      local val_19_auto = bit.tohex(b, 2)
      if (nil ~= val_19_auto) then
        i_18_auto = (i_18_auto + 1)
        do end (tbl_17_auto)[i_18_auto] = val_19_auto
      else
      end
    end
    mem = tbl_17_auto
  end
  local step = 4
  for i = 1, #mem, step do
    local block
    do
      local tbl_17_auto = {}
      local i_18_auto = #tbl_17_auto
      for ii = 0, (step - 1) do
        local val_19_auto = mem[(i + ii)]
        if (nil ~= val_19_auto) then
          i_18_auto = (i_18_auto + 1)
          do end (tbl_17_auto)[i_18_auto] = val_19_auto
        else
        end
      end
      block = tbl_17_auto
    end
    print(unpack(block))
  end
  return nil
end
local function load_rom(machine, path)
  local function _8_(...)
    local _9_, _10_ = ...
    if true then
      local _ = _9_
      local function _11_(...)
        local _12_, _13_ = ...
        if (nil ~= _12_) then
          local fd = _12_
          local function _14_(...)
            local _15_, _16_ = ...
            if (nil ~= _15_) then
              local blob = _15_
              local function _17_(...)
                local _18_, _19_ = ...
                if true then
                  local _0 = _18_
                  local bytes = {string.byte(blob, 1, #blob)}
                  machine["write-bytes"](machine, machine.pc, bytes)
                  return machine
                elseif ((_18_ == nil) and (nil ~= _19_)) then
                  local e = _19_
                  return print(e)
                else
                  return nil
                end
              end
              return _17_(fd:close())
            elseif ((_15_ == nil) and (nil ~= _16_)) then
              local e = _16_
              return print(e)
            else
              return nil
            end
          end
          return _14_(fd:read("*a"))
        elseif ((_12_ == nil) and (nil ~= _13_)) then
          local e = _13_
          return print(e)
        else
          return nil
        end
      end
      return _11_(io.open(path, "rb"))
    elseif ((_9_ == nil) and (nil ~= _10_)) then
      local e = _10_
      return print(e)
    else
      return nil
    end
  end
  return _8_(print(path))
end
local function CALL_ML_NNN(machine)
  return error("native execution ont supported")
end
local function CLS(machine)
  for i = 0, (machine.video.size - 1) do
    machine.video.block[i] = 0
  end
  return machine["inc-pc"](machine)
end
local function RET(machine)
  local addr = machine["stack-pop"](machine)
  machine.pc = addr
  return nil
end
local function JP_NNN(machine, n1, n2, n3)
  local addr = nibbles__3eword(0, n1, n2, n3)
  machine.pc = addr
  return machine
end
local function CALL_NNN(machine, n1, n2, n3)
  local addr = nibbles__3eword(0, n1, n2, n3)
  local next_pc = (machine.pc + 2)
  machine["stack-push"](machine, next_pc)
  machine.pc = addr
  return machine
end
local function SE_VX_NN(machine, vx, n1, n2)
  local num = nibbles__3eword(0, 0, n1, n2)
  local v_val = machine["read-register"](machine, vx)
  if (num == v_val) then
    return machine["inc-pc"](machine, 2)
  else
    return machine["inc-pc"](machine, 1)
  end
end
local function SNE_VX_NN(machine, vx, n1, n2)
  local num = nibbles__3eword(0, 0, n1, n2)
  local v_val = machine["read-register"](machine, vx)
  if (num == v_val) then
    return machine["inc-pc"](machine, 1)
  else
    return machine["inc-pc"](machine, 2)
  end
end
local function SE_VX_VY(machine, vx, vy)
  if (machine["read-register"](machine, vx) == machine["read-register"](machine, vy)) then
    return machine["inc-pc"](machine, 2)
  else
    return machine["inc-pc"](machine, 1)
  end
end
local function LD_VX_NN(machine, vx, n1, n2)
  local num = nibbles__3eword(0, 0, n1, n2)
  machine["write-register"](machine, vx, num)
  return machine["inc-pc"](machine)
end
local function ADD_VX_NN(machine, vx, n1, n2)
  local a = nibbles__3eword(0, 0, n1, n2)
  local b = machine["read-register"](machine, vx)
  machine["write-register"](machine, vx, (a + b))
  return machine["inc-pc"](machine)
end
local function LD_VX_VY(machine, vx, vy)
  machine["write-register"](machine, vx, machine["read-register"](machine, vy))
  return machine["inc-pc"](machine)
end
local function OR_VX_VY(machine, vx, vy)
  local x = machine["read-register"](machine, vx)
  local y = machine["read-register"](machine, vy)
  if machine.quirks["vf-reset"] then
    machine["write-register"](machine, 15, 0)
  else
  end
  machine["write-register"](machine, vx, bit.bor(x, y))
  return machine["inc-pc"](machine)
end
local function AND_VX_VY(machine, vx, vy)
  local x = machine["read-register"](machine, vx)
  local y = machine["read-register"](machine, vy)
  if machine.quirks["vf-reset"] then
    machine["write-register"](machine, 15, 0)
  else
  end
  machine["write-register"](machine, vx, bit.band(x, y))
  return machine["inc-pc"](machine)
end
local function XOR_VX_VY(machine, vx, vy)
  local x = machine["read-register"](machine, vx)
  local y = machine["read-register"](machine, vy)
  if machine.quirks["vf-reset"] then
    machine["write-register"](machine, 15, 0)
  else
  end
  machine["write-register"](machine, vx, bit.bxor(x, y))
  return machine["inc-pc"](machine)
end
local function ADD_VX_VY(machine, vx, vy)
  local x = machine["read-register"](machine, vx)
  local y = machine["read-register"](machine, vy)
  local sum = (x + y)
  machine["write-register"](machine, vx, sum)
  local function _30_()
    if (255 < sum) then
      return 1
    else
      return 0
    end
  end
  machine["write-register"](machine, 15, _30_())
  return machine["inc-pc"](machine)
end
local function SUB_VX_VY(machine, vx, vy)
  local x = machine["read-register"](machine, vx)
  local y = machine["read-register"](machine, vy)
  local sum = (x - y)
  local function _31_()
    if (sum < 0) then
      return (sum + 256)
    else
      return sum
    end
  end
  machine["write-register"](machine, vx, _31_())
  local function _32_()
    if (sum < 0) then
      return 0
    else
      return 1
    end
  end
  machine["write-register"](machine, 15, _32_())
  return machine["inc-pc"](machine)
end
local function SHR_VX_VY(machine, vx, vy)
  local val
  if machine.quirks.shifting then
    val = machine["read-register"](machine, vx)
  else
    val = machine["read-register"](machine, vy)
  end
  local lsb = bit.band(val, 1)
  local shifted = bit.rshift(val, 1)
  machine["write-register"](machine, vx, shifted)
  machine["write-register"](machine, 15, lsb)
  return machine["inc-pc"](machine)
end
local function SUBN_VX_VY(machine, vx, vy)
  local x = machine["read-register"](machine, vx)
  local y = machine["read-register"](machine, vy)
  local sum = (y - x)
  local function _34_()
    if (sum < 0) then
      return (sum + 256)
    else
      return sum
    end
  end
  machine["write-register"](machine, vx, _34_())
  local function _35_()
    if (sum < 0) then
      return 0
    else
      return 1
    end
  end
  machine["write-register"](machine, 15, _35_())
  return machine["inc-pc"](machine)
end
local function SHL_VX_VY(machine, vx, vy)
  local val
  if machine.quirks.shifting then
    val = machine["read-register"](machine, vx)
  else
    val = machine["read-register"](machine, vy)
  end
  local msb = bit.rshift(val, 7)
  local shifted = bit.band(bit.lshift(val, 1), 255)
  machine["write-register"](machine, vx, shifted)
  machine["write-register"](machine, 15, msb)
  return machine["inc-pc"](machine)
end
local function SNE_VX_VY(machine, vx, vy)
  local x = machine["read-register"](machine, vx)
  local y = machine["read-register"](machine, vy)
  if (x == y) then
    return machine["inc-pc"](machine)
  else
    return machine["inc-pc"](machine, 2)
  end
end
local function LD_I_NNN(machine, n1, n2, n3)
  local addr = nibbles__3eword(0, n1, n2, n3)
  machine["write-index"](machine, addr)
  return machine["inc-pc"](machine)
end
local function JP_V0_NNN(machine, n1, n2, n3)
  local base_addr = nibbles__3eword(0, n1, n2, n3)
  local vx_or_v0
  if machine.quirks.jumping then
    vx_or_v0 = machine["read-register"](machine, n1)
  else
    vx_or_v0 = machine["read-register"](machine, 0)
  end
  local addr = (base_addr + vx_or_v0)
  machine.pc = bit.band(addr, 65535)
  return nil
end
local function RND_VX_NN(machine, vx, n1, n2)
  local mask = nibbles__3eword(0, 0, n1, n2)
  local num = bit.band(math.random(0, 255), mask)
  machine["write-register"](machine, vx, num)
  return machine["inc-pc"](machine)
end
local function DRW_VX_VY_N(machine, vx, vy, n)
  local function update_video_byte(mem_loc, new_byte)
    local _let_39_ = read_bytes(machine.video, mem_loc, 1)
    local old_byte = _let_39_[1]
    local xored = bit.bxor(old_byte, new_byte)
    local unset_3f = (old_byte ~= bit.band(old_byte, xored))
    if unset_3f then
      machine["write-register"](machine, 15, 1)
    else
    end
    return write_bytes(machine.video, mem_loc, {xored})
  end
  machine["write-register"](machine, 15, 0)
  local addr = machine["read-index"](machine)
  local x = (machine["read-register"](machine, vx) % 64)
  local y = (machine["read-register"](machine, vy) % 32)
  local stride = (64 / 8)
  local n0 = math.min(n, (32 - y))
  local bytes = machine["read-bytes"](machine, addr, n0)
  for i = 0, (#bytes - 1) do
    local byte = bytes[(i + 1)]
    local y_byte = (y + i)
    local bit_misalignment = (x % 8)
    local first_x_byte = math.floor((x / 8))
    local mem_loc_1 = (first_x_byte + (y_byte * stride))
    local mem_loc_2 = (mem_loc_1 + 1)
    local left_byte = bit.rshift(byte, bit_misalignment)
    local right_byte = bit.band(bit.lshift(byte, (8 - bit_misalignment)), 255)
    update_video_byte(mem_loc_1, left_byte)
    if (0 ~= (mem_loc_2 % 8)) then
      update_video_byte(mem_loc_2, right_byte)
    else
    end
  end
  return machine["inc-pc"](machine)
end
local function SKP_VX(machine, vx)
  local k = machine["read-register"](machine, vx)
  local want_mask = bit.lshift(1, k)
  local _let_42_ = machine["read-words"](machine, keyboard_mem_loc, 1)
  local have_mask = _let_42_[1]
  local test = bit.band(want_mask, have_mask)
  machine["inc-pc"](machine)
  if (0 < test) then
    return machine["inc-pc"](machine)
  else
    return nil
  end
end
local function SKNP_VX(machine, vx)
  local k = machine["read-register"](machine, vx)
  local dont_want_mask = bit.lshift(1, k)
  local _let_44_ = machine["read-words"](machine, keyboard_mem_loc, 1)
  local have_mask = _let_44_[1]
  local test = bit.band(dont_want_mask, have_mask)
  machine["inc-pc"](machine)
  if (0 == test) then
    return machine["inc-pc"](machine)
  else
    return nil
  end
end
local function LD_VX_DT(machine, vx)
  machine["write-register"](machine, vx, machine.delay.t)
  return machine["inc-pc"](machine)
end
local function LD_VX_K(machine, vx)
  local k = machine["read-register"](machine, vx)
  local want_mask = bit.lshift(1, k)
  local _let_46_ = machine["read-words"](machine, keyboard_mem_loc, 1)
  local have_mask = _let_46_[1]
  local test = bit.band(want_mask, have_mask)
  if (0 < test) then
    return machine["inc-pc"](machine)
  else
    return nil
  end
end
local function LD_DT_VX(machine, vx)
  local t = machine["read-register"](machine, vx)
  do end (machine.delay):set(t, machine["rtc-ms"])
  return machine["inc-pc"](machine)
end
local function LD_ST_VX(machine, vx)
  local t = machine["read-register"](machine, vx)
  do end (machine.sound):set(t, machine["rtc-ms"])
  return machine["inc-pc"](machine)
end
local function ADD_I_VX(machine, vx)
  local x = machine["read-register"](machine, vx)
  local i = machine["read-index"](machine)
  machine["write-index"](machine, (i + x))
  return machine["inc-pc"](machine)
end
local function LD_F_VX(machine, vx)
  local c = bit.band(15, machine["read-register"](machine, vx))
  local addr = (font_mem_loc + (c * 5))
  machine["write-index"](machine, addr)
  return machine["inc-pc"](machine)
end
local function LD_B_VX(machine, vx)
  local num = machine["read-register"](machine, vx)
  local hunds = math.floor((num / 100))
  local tens = math.floor(((num % 100) / 10))
  local ones = math.floor((num % 10))
  machine["write-bytes"](machine, machine["read-index"](machine), {hunds, tens, ones})
  return machine["inc-pc"](machine)
end
local function LD_I_VX(machine, vx)
  local addr = machine["read-index"](machine)
  local bytes
  do
    local tbl_17_auto = {}
    local i_18_auto = #tbl_17_auto
    for i = 0, vx do
      local val_19_auto = machine["read-register"](machine, i)
      if (nil ~= val_19_auto) then
        i_18_auto = (i_18_auto + 1)
        do end (tbl_17_auto)[i_18_auto] = val_19_auto
      else
      end
    end
    bytes = tbl_17_auto
  end
  machine["write-bytes"](machine, addr, bytes)
  if not machine.quirks.memory then
    machine["write-index"](machine, (addr + vx + 1))
  else
  end
  return machine["inc-pc"](machine)
end
local function LD_VX_I(machine, vx)
  local addr = machine["read-index"](machine)
  local bytes = machine["read-bytes"](machine, addr, (vx + 1))
  for i = 0, vx do
    machine["write-register"](machine, i, bytes[(i + 1)])
  end
  machine["write-index"](machine, (addr + vx + 1))
  return machine["inc-pc"](machine)
end
local last_ins = 0
local function fetch_decode_execute(machine)
  local _let_50_ = machine["read-words"](machine, machine.pc, 1)
  local instruction = _let_50_[1]
  local _
  last_ins = instruction
  _ = nil
  local _let_51_ = word__3enibbles(instruction)
  local n1 = _let_51_[1]
  local n2 = _let_51_[2]
  local n3 = _let_51_[3]
  local n4 = _let_51_[4]
  local _52_, _53_, _54_, _55_ = n1, n2, n3, n4
  if ((_52_ == 0) and (_53_ == 0) and (_54_ == 14) and (_55_ == 0)) then
    return CLS(machine)
  elseif ((_52_ == 0) and (_53_ == 0) and (_54_ == 14) and (_55_ == 14)) then
    return RET(machine)
  elseif ((_52_ == 0) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local n10 = _53_
    local n20 = _54_
    local n30 = _55_
    return JP_NNN(machine, n10, n20, n30)
  elseif ((_52_ == 1) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local n10 = _53_
    local n20 = _54_
    local n30 = _55_
    return JP_NNN(machine, n10, n20, n30)
  elseif ((_52_ == 2) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local n10 = _53_
    local n20 = _54_
    local n30 = _55_
    return CALL_NNN(machine, n10, n20, n30)
  elseif ((_52_ == 3) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local vx = _53_
    local n10 = _54_
    local n20 = _55_
    return SE_VX_NN(machine, vx, n10, n20)
  elseif ((_52_ == 4) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local vx = _53_
    local n10 = _54_
    local n20 = _55_
    return SNE_VX_NN(machine, vx, n10, n20)
  elseif ((_52_ == 5) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 0)) then
    local vx = _53_
    local vy = _54_
    return SE_VX_VY(machine, vx, vy)
  elseif ((_52_ == 6) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local vx = _53_
    local n10 = _54_
    local n20 = _55_
    return LD_VX_NN(machine, vx, n10, n20)
  elseif ((_52_ == 7) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local vx = _53_
    local n10 = _54_
    local n20 = _55_
    return ADD_VX_NN(machine, vx, n10, n20)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 0)) then
    local vx = _53_
    local vy = _54_
    return LD_VX_VY(machine, vx, vy)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 1)) then
    local vx = _53_
    local vy = _54_
    return OR_VX_VY(machine, vx, vy)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 2)) then
    local vx = _53_
    local vy = _54_
    return AND_VX_VY(machine, vx, vy)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 3)) then
    local vx = _53_
    local vy = _54_
    return XOR_VX_VY(machine, vx, vy)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 4)) then
    local vx = _53_
    local vy = _54_
    return ADD_VX_VY(machine, vx, vy)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 5)) then
    local vx = _53_
    local vy = _54_
    return SUB_VX_VY(machine, vx, vy)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 6)) then
    local vx = _53_
    local vy = _54_
    return SHR_VX_VY(machine, vx, vy)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 7)) then
    local vx = _53_
    local vy = _54_
    return SUBN_VX_VY(machine, vx, vy)
  elseif ((_52_ == 8) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 14)) then
    local vx = _53_
    local vy = _54_
    return SHL_VX_VY(machine, vx, vy)
  elseif ((_52_ == 9) and (nil ~= _53_) and (nil ~= _54_) and (_55_ == 0)) then
    local vx = _53_
    local vy = _54_
    return SNE_VX_VY(machine, vx, vy)
  elseif ((_52_ == 10) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local n10 = _53_
    local n20 = _54_
    local n30 = _55_
    return LD_I_NNN(machine, n10, n20, n30)
  elseif ((_52_ == 11) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local n10 = _53_
    local n20 = _54_
    local n30 = _55_
    return JP_V0_NNN(machine, n10, n20, n30)
  elseif ((_52_ == 12) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local vx = _53_
    local n10 = _54_
    local n20 = _55_
    return RND_VX_NN(machine, vx, n10, n20)
  elseif ((_52_ == 13) and (nil ~= _53_) and (nil ~= _54_) and (nil ~= _55_)) then
    local vx = _53_
    local vy = _54_
    local n10 = _55_
    return DRW_VX_VY_N(machine, vx, vy, n10)
  elseif ((_52_ == 14) and (nil ~= _53_) and (_54_ == 9) and (_55_ == 14)) then
    local vx = _53_
    return SKP_VX(machine, vx)
  elseif ((_52_ == 14) and (nil ~= _53_) and (_54_ == 10) and (_55_ == 1)) then
    local vx = _53_
    return SKNP_VX(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 0) and (_55_ == 7)) then
    local vx = _53_
    return LD_VX_DT(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 0) and (_55_ == 10)) then
    local vx = _53_
    return LD_VX_K(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 1) and (_55_ == 5)) then
    local vx = _53_
    return LD_DT_VX(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 1) and (_55_ == 8)) then
    local vx = _53_
    return LD_ST_VX(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 1) and (_55_ == 14)) then
    local vx = _53_
    return ADD_I_VX(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 2) and (_55_ == 9)) then
    local vx = _53_
    return LD_F_VX(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 3) and (_55_ == 3)) then
    local vx = _53_
    return LD_B_VX(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 5) and (_55_ == 5)) then
    local vx = _53_
    return LD_I_VX(machine, vx)
  elseif ((_52_ == 15) and (nil ~= _53_) and (_54_ == 6) and (_55_ == 5)) then
    local vx = _53_
    return LD_VX_I(machine, vx)
  elseif true then
    local _0 = _52_
    return error(fmt("unknown instruction 0x%s", bit.tohex(instruction, 4)))
  else
    return nil
  end
end
local function tick_timers(machine)
  do end (machine.delay):tick(machine["rtc-ms"])
  return (machine.sound):tick(machine["rtc-ms"])
end
local suspended_ms = 0
local function step(machine, _3fdelta_ms)
  local threshold_ms = math.floor((1000 / machine.hz))
  local _3fdelta_ms0 = math.floor((_3fdelta_ms or threshold_ms))
  suspended_ms = (suspended_ms + _3fdelta_ms0)
  if (threshold_ms <= suspended_ms) then
    machine["rtc-ms"] = (machine["rtc-ms"] + threshold_ms)
    tick_timers(machine)
    fetch_decode_execute(machine)
    suspended_ms = (suspended_ms - threshold_ms)
    return step(machine, 0)
  else
    return nil
  end
end
local function new_timer()
  local last_ms = 0
  local interval_ms = math.floor(((1 / 60) * 1000))
  local function _58_(timer, time, now_ms)
    last_ms = now_ms
    timer.t = bit.band(time, 255)
    return nil
  end
  local function _59_(timer, now_ms)
    if (0 < timer.t) then
      local delta = (now_ms - last_ms)
      if (interval_ms <= delta) then
        local steps = math.floor((delta / interval_ms))
        timer.t = math.max(0, (timer.t - steps))
        last_ms = (last_ms + (steps * interval_ms))
        return nil
      else
        return nil
      end
    else
      return nil
    end
  end
  return {t = 0, set = _58_, tick = _59_}
end
local function merge_table(t_into, t_from)
  local function unless_nil(x, y)
    if (nil == x) then
      return y
    else
      return x
    end
  end
  local t = t_into
  for k, v in pairs(t_from) do
    local _63_ = type(v)
    if (_63_ == "table") then
      assert(((nil == t_into[k]) or ("table" == type(t_into[k]))), string.format("%s must be table or nil", k))
      t[k] = merge_table(unless_nil(t_into[k], {}), v)
      t = t
    elseif true then
      local _ = _63_
      t[k] = unless_nil(t_into[k], v)
      t = t
    else
      t = nil
    end
  end
  return t
end
local function new(_3fopts)
  local _let_65_ = require("chip8.memory")
  local new_memory = _let_65_["new"]
  local read_bytes0 = _let_65_["read-bytes"]
  local write_bytes0 = _let_65_["write-bytes"]
  local read_words = _let_65_["read-words"]
  local write_words = _let_65_["write-words"]
  local defaults
  local function _66_()
    return nil
  end
  local function _67_()
    return nil
  end
  local function _68_()
    return nil
  end
  defaults = {devices = {keyboard = _66_, video = _67_, audio = _68_}, mhz = 0.5, compatibility = "CHIP-8"}
  local opts = merge_table((_3fopts or {}), defaults)
  local quirks
  do
    local _69_ = opts.compatibility
    if (_69_ == "CHIP-8") then
      quirks = {["vf-reset"] = true, jumping = false, memory = false, shifting = false}
    elseif (_69_ == "SUPER-CHIP-1.0") then
      quirks = {memory = true, shifting = true, jumping = true, ["vf-reset"] = false}
    elseif (_69_ == "XO-CHIP") then
      quirks = {memory = true, shifting = true, jumping = true, ["vf-reset"] = false}
    elseif true then
      local _ = _69_
      quirks = error(string.format("unknown compatibility %s", opts.compatibility))
    else
      quirks = nil
    end
  end
  local machine
  local function _71_(m, ...)
    return load_rom(m, ...)
  end
  local function _72_(m, _3fdistance)
    m.pc = (m.pc + (2 * (_3fdistance or 1)))
    return nil
  end
  local function stack_push(m, word)
    m.sp = (m.sp - 2)
    return m["write-words"](m, m.sp, {word})
  end
  local function _73_(m)
    local _let_74_ = m["read-words"](m, m.sp, 1)
    local word = _let_74_[1]
    m.sp = (m.sp + 2)
    return word
  end
  local function _75_(m)
    return (read_words(m.i, 0, 1))[1]
  end
  local function _76_(m, v)
    return write_words(m.i, 0, {v})
  end
  local function _77_(m, v)
    return m.v.block[v]
  end
  local function _78_(m, v, n)
    m.v.block[v] = n
    return nil
  end
  local function _79_(m, ...)
    return write_bytes0(m.memory, ...)
  end
  local function _80_(m, ...)
    return read_bytes0(m.memory, ...)
  end
  local function _81_(m, ...)
    return read_words(m.memory, ...)
  end
  local function _82_(m, ...)
    return write_words(m.memory, ...)
  end
  machine = {pc = 512, sp = 512, v = new_memory(15), i = new_memory(2), memory = new_memory(4096), video = new_memory(((64 / 8) * 32)), ["rtc-ms"] = 0, delay = new_timer(), sound = new_timer(), hz = (opts.mhz * 1000), step = step, quirks = quirks, ["load-rom"] = _71_, ["inc-pc"] = _72_, ["stack-push"] = stack_push, ["stack-pop"] = _73_, ["read-index"] = _75_, ["write-index"] = _76_, ["read-register"] = _77_, ["write-register"] = _78_, ["write-bytes"] = _79_, ["read-bytes"] = _80_, ["read-words"] = _81_, ["write-words"] = _82_}
  local function _83_(key_word)
    return machine["write-words"](machine, keyboard_mem_loc, {key_word})
  end
  opts.devices.keyboard(_83_)
  local function _84_()
    return read_bytes0(machine.video, 0, machine.video.size), {width = 64, height = 32}
  end
  opts.devices.video(_84_)
  machine["write-bytes"](machine, 0, bootloader)
  machine["write-bytes"](machine, font_mem_loc, font_bytes)
  return machine
end
return {new = new, step = step, ["dump-bytes"] = dump_bytes, ["dump-memory"] = dump_memory}