local config = require("gbc-tools.config")
local inlay = require("gbc-tools.inlay_hints")

local M = {}

function M.handler(_, result)
	if result.quiescent and config.options.tools.autoSetHints and not M.ran_once then
		inlay.set_inlay_hints()
		require("gbc-tools.inlay_hints").setup_autocmd()
		M.ran_once = true
	end
end

return M

