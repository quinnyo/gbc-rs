local utils = require("gbc-tools.utils.utils")

local M = {}

local latest_buf_id = nil

function M.execute_command(command, args, cwd)
  local full_command = utils.chain_commands({
    utils.make_command_from_args("cd", { cwd }),
    utils.make_command_from_args(command, args),
  })
  vim.api.nvim_command("FloatermNew " .. full_command)
end

return M

