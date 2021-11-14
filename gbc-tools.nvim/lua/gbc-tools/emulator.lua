local utils = require("gbc-tools.utils.utils")
local M = {}

function M.start() 
    utils.command("emulator/start", nil)
end

function M.stop() 
    utils.command("emulator/stop", nil)
end

return M

