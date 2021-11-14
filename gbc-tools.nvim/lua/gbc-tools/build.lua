local utils = require("gbc-tools.utils.utils")
local M = {}

function M.rom() 
    utils.command("build/rom", nil)
end

return M


