local nvim_lsp = require("lspconfig")
local config = require("gbc-tools.config")
local utils = require("gbc-tools.utils.utils")
local inlay = require("gbc-tools.inlay_hints")
local runnables = require("gbc-tools.runnables")

local M = {}

local function setupCommands()
	local lsp_opts = config.options.server

	lsp_opts.commands = vim.tbl_deep_extend("force", lsp_opts.commands or {}, {
		GBCSetInlayHints = {
			inlay.set_inlay_hints,
		},
		GBCDisableInlayHints = {
			inlay.disable_inlay_hints,
		},
		GBCToggleInlayHints = {
			inlay.toggle_inlay_hints,
		},
        GBCRunnables = {
            function()
                utils.command("project/runnables", runnables.handler)  
            end,
            '-nargs=0',
            description = '`:GBCRunnables` List runnables for the current workspace',
        },
	})
end

local function server_status(_, result)
	if result.quiescent and config.options.tools.autoSetHints and not M.ran_once then
		inlay.set_inlay_hints()
		inlay.setup_autocmd()
		M.ran_once = true
	end
end

local function setup_lsp()
    local configs = require("lspconfig.configs")
    configs['gbc_analyzer'] = {
      default_config = {
        cmd = { 'gbc-analyzer' },
        filetypes = { 'gbc' },
        root_dir = nvim_lsp.util.root_pattern('gbc.toml'),
        settings = {
          ['gbc-analyzer'] = {},
        },
      },
      docs = {
        description = [[
    gbc language server
        ]]
      },
    }
    nvim_lsp.gbc_analyzer.setup(config.options.server)
end

local function setup_dap()
    local dap = require("dap")
    dap.adapters.gbc = {
       type = 'executable',
       command = 'gbc',
       args = { 'run', '-p', 'gbd', '--', '--dap' },
    }
    dap.configurations.gbc = {
        {
            type = 'gbc',
            request = 'launch',
            name = "Launch ROM",
            program = "${file}",
        }
    }
end

local function setup_handlers()
	local lsp_opts = config.options.server
	local tool_opts = config.options.tools
	local custom_handlers = {}

	custom_handlers["experimental/serverStatus"] = utils.mk_handler(server_status)
	custom_handlers["experimental/inlayHints"] = utils.mk_handler(inlay.handler)

	lsp_opts.handlers = vim.tbl_deep_extend("force", custom_handlers, lsp_opts.handlers or {})
end

function M.setup(opts)
	config.setup(opts)
    setup_handlers()
	setupCommands()
    setup_lsp()
    if pcall(require, "dap") then
        setup_dap()
    end
end

return M

