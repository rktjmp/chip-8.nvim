local _local_1_ = require("fennel")
local view = _local_1_["view"]
local ffi = require("ffi")
local function dump(memory, _3foffset, _3flen)
  local function _2_()
    local tbl_17_auto = {}
    local i_18_auto = #tbl_17_auto
    for i = (_3foffset or 0), (_3flen or (memory.size - 1)) do
      local val_19_auto = memory.block[i]
      if (nil ~= val_19_auto) then
        i_18_auto = (i_18_auto + 1)
        do end (tbl_17_auto)[i_18_auto] = val_19_auto
      else
      end
    end
    return tbl_17_auto
  end
  return print("block", view(_2_()))
end
local function new(size)
  local block = ffi.new("uint8_t[?]", size)
  for i = 0, (size - 1) do
    block[i] = 0
  end
  return {size = size, block = block}
end
local function read_bytes(memory, offset, count, _3fstride)
  assert(((offset + count) <= memory.size), "Error: read-bytes attempt to read past memory limit")
  local stride = (_3fstride or 1)
  local tbl_17_auto = {}
  local i_18_auto = #tbl_17_auto
  for i = 0, (count - 1) do
    local val_19_auto = memory.block[(offset + (i * stride))]
    if (nil ~= val_19_auto) then
      i_18_auto = (i_18_auto + 1)
      do end (tbl_17_auto)[i_18_auto] = val_19_auto
    else
    end
  end
  return tbl_17_auto
end
local function write_bytes(memory, offset, bytes, _3fstride)
  assert(((offset + #bytes) <= memory.size), "Error: write-bytes attempt to write past memory limit")
  local stride = (_3fstride or 1)
  for i = 0, (#bytes - 1) do
    memory.block[(offset + (i * stride))] = bytes[(i + 1)]
  end
  return nil
end
local function read_words(memory, offset, count)
  local tbl_17_auto = {}
  local i_18_auto = #tbl_17_auto
  for i = offset, ((offset + (count * 2)) - 1), 2 do
    local val_19_auto = bit.bor(bit.lshift(memory.block[i], 8), memory.block[(i + 1)])
    if (nil ~= val_19_auto) then
      i_18_auto = (i_18_auto + 1)
      do end (tbl_17_auto)[i_18_auto] = val_19_auto
    else
    end
  end
  return tbl_17_auto
end
local function write_words(memory, offset, words)
  for i, w in ipairs(words) do
    local index = (2 * (i - 1))
    do end (memory.block)[(offset + index)] = bit.band(bit.rshift(w, 8), 255)
    do end (memory.block)[(offset + index + 1)] = bit.band(w, 255)
  end
  return memory
end
local function nibbles__3eword(n1, n2, n3, n4)
  return bit.bor(bit.lshift(n1, 12), bit.lshift(n2, 8), bit.lshift(n3, 4), n4)
end
local function nibbles__3ebyte(n1, n2)
  return bit.bor(bit.lshift(n1, 4), n2)
end
local function word__3enibbles(word)
  return {bit.band(bit.rshift(word, 12), 15), bit.band(bit.rshift(word, 8), 15), bit.band(bit.rshift(word, 4), 15), bit.band(bit.rshift(word, 0), 15)}
end
local function byte__3enibbles(byte)
  return {bit.band(bit.rshift(byte, 4), 15), bit.band(bit.rshift(byte, 0), 15)}
end
return {new = new, ["read-bytes"] = read_bytes, ["write-bytes"] = write_bytes, ["read-words"] = read_words, ["write-words"] = write_words, ["nibbles->byte"] = nibbles__3ebyte, ["nibbles->word"] = nibbles__3eword, ["word->nibbles"] = word__3enibbles}