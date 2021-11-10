local utils = require("gbc-tools.utils.utils")
local popup = require("popup")

local M = {}

function M.toggle_breakpoint() 
    utils.command("debugger/toggle_breakpoint", nil)
end

function M.step() 
    utils.command("debugger/step", nil)
end

function M.step_over() 
    utils.command("debugger/step_over", nil)
end

function M.finish() 
    utils.command("debugger/finish", nil)
end

function M.continue() 
    utils.command("debugger/continue", nil)
end

function M.undo() 
    utils.command("debugger/undo", nil)
end

return M

