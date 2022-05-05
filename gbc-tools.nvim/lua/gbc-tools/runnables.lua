local utils = require("gbc-tools.utils.utils")
local config = require("gbc-tools.config")

local M = {}

local function get_options(result)
  local option_strings = {}

  for _, runnable in ipairs(result) do
    local str = runnable.label
    print(str)
    table.insert(option_strings, str)
  end

  return option_strings
end

---comment
---@return string build command
---@return string|table args
---@return any cwd
local function getCommand(c, results)
  local ret = " "
  local args = results[c].args

  local dir = args.workspaceRoot

  ret = vim.list_extend({}, args.gbcArgs or {})
  --ret = vim.list_extend(ret, args.cargoExtraArgs or {})
  --table.insert(ret, "--")
  --ret = vim.list_extend(ret, args.executableArgs or {})

  return "gbc", ret, dir
end

function M.run_command(choice, result)
  -- do nothing if choice is too high or too low
  if choice == nil or choice < 1 or choice > #result then
    return
  end

  local opts = config.options.tools

  local command, args, cwd = getCommand(choice, result)

  opts.executor.execute_command(command, args, cwd)
end

function M.handler(_, result)
  -- get the choice from the user
  local options = get_options(result)
  if #options > 0 then
      vim.ui.select(
        options,
        { prompt = "Runnables", kind = "gbc-tools/runnables" },
        function(_, choice)
          M.run_command(choice, result)
        end
      )
  end
end

return M

