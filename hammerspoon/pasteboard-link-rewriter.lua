-- luacheck: globals hs
-- rewrite links in pasteboard
local M = {}

local alert = require 'hs.alert'

-- rewrite https://www.youtube.com/watch?v={video-hash-code} to https://youtu.be/{video-hash-code}
-- https://youtu.be/PHe0bXAIuk0?si=1YiGLQ9VI9O0A7nS
-- https://www.youtube.com/watch?v=PHe0bXAIuk0
local function youtube(url)
  if url == nil then
    return nil
  end
  -- luacheck: ignore
  for id, other in string.gmatch(url, 'https://www.youtube.com/watch%?v=([%w]+)&?(.*)') do
    local res = "https://youtu.be/" .. id .. other
    return res
  end
  return nil
end

local function tracker(url)
  if url == nil then
    return nil
  end
  -- function test_trakcer()
  --   local url = 'https://www.unknown.com/'
  --   for i, p in ipairs(parameters) do
  --     if i == 1 then
  --       url = url .. '?'
  --     else
  --       url = url .. '&valid=xxx&'
  --     end
  --     url = url .. p .. '=xxx'
  --   end
  --   return url
  -- end
  -- local url = test_trakcer()

  -- Use tracker parameters
  -- https://github.com/rknightuk/TrackerZapper
  local parameters ={"_bta_c",
            "_bta_tid",
            "_ga",
            "_hsenc",
            "_hsmi",
            "_ke",
            "_openstat",
            "cid",
            "dm_i",
            "ef_id",
            "epik",
            "fbclid",
            "gclid",
            "gclsrc",
            "gdffi",
            "gdfms",
            "gdftrk",
            "hsa_",
            "igshid",
            "matomo_",
            "mc_",
            "mkwid",
            "msclkid",
            "mtm_",
            "ns_",
            "oly_anon_id",
            "oly_enc_id",
            "otc",
            "pcrid",
            "piwik_",
            "pk_",
            "rb_clickid",
            "redirect_log_mongo_id",
            "redirect_mongo_id",
            "ref",
            "s_kwcid",
            "sb_referer_host",
            "scrolla",
            "soc_src",
            "soc_trk",
            "spm",
            "sr_",
            "srcid",
            "stm_",
            "trk_",
            "utm_",
            "vero_",
            -- bilibili
            "spm_id_from",
            "vd_source",
            -- youtube
            "si",
          }
  local base, query = string.match(url, "([^?]+)?(.*)")

  local function isTrackerParameter(param)
    for _, p in ipairs(parameters) do
      if string.match(param, p) then
        return true
      end
    end
    return false
  end

  if query then
    local parts = {}
    for param, value in string.gmatch(query, "([^&]+)=([^&]+)") do
      if not isTrackerParameter(param) then
        table.insert(parts, param .. "=" .. value)
      end
    end

    local newQuery = table.concat(parts, "&")

    if #newQuery > 0 then
      url = base .. "?" .. newQuery
    else
      url = base
    end
  end
  return url
end


local function get_link()
  -- luacheck: globals hs
  local pb = require('hs.pasteboard')
  local string = pb.readString()
  return string.match(string,'^https?://.*')
end

M.rewrite_youtube_link = function()
  local link = get_link()
  local url = youtube(link)
  -- Watch out here: auto-remove tracker parameters
  url = tracker(link)
  print(url)
  if url then
    local pb = require('hs.pasteboard')
    pb.setContents(url)
    alert.show(string.format("link: %s", url))
  end
end

M.remove_link_tracker = function()
  local link = get_link()
  local url = tracker(link)
  print(url)
  if url then
      local pb = require('hs.pasteboard')
      pb.setContents(url)
      alert.show(string.format("link: %s", url))
  end
end

return M
