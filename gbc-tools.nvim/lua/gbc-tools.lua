local nvim_lsp = require("lspconfig")
local config = require("gbc-tools.config")
local utils = require("gbc-tools.utils.utils")
local server_status = require("gbc-tools.server_status")
local inlay = require("gbc-tools.inlay_hints")
local outline = require("gbc-tools.outline")

local M = {}

local function setupCommands()
	local lsp_opts = config.options.server

	lsp_opts.commands = vim.tbl_deep_extend("force", lsp_opts.commands or {}, {
		GBCSetInlayHints = {
			require("gbc-tools.inlay_hints").set_inlay_hints,
		},
		GBCDisableInlayHints = {
			require("gbc-tools.inlay_hints").disable_inlay_hints,
		},
		GBCToggleInlayHints = {
			require("gbc-tools.inlay_hints").toggle_inlay_hints,
		},
        GBCDebuggerToggleBreakpoint = {
			require("gbc-tools.debugger").toggle_breakpoint,
        },
        GBCDebuggerStep = {
			require("gbc-tools.debugger").step,
        },
        GBCDebuggerNext = {
			require("gbc-tools.debugger").step_over,
        },
        GBCDebuggerFinish = {
			require("gbc-tools.debugger").finish,
        },
        GBCDebuggerContinue = {
			require("gbc-tools.debugger").continue,
        },
        GBCDebuggerUndo = {
			require("gbc-tools.debugger").undo,
        },
        GBCDebuggerOutlineToggle = {
			require("gbc-tools.outline").toggle,
        },
        GBCDebuggerOutlineOpen = {
			require("gbc-tools.outline").open,
        },
        GBCDebuggerOutlineClose = {
			require("gbc-tools.outline").close,
        },
        GBCEmulatorStart = {
			require("gbc-tools.emulator").start,
        },
        GBCEmulatorStop = {
			require("gbc-tools.emulator").stop,
        },
        GBCBuildRom = {
			require("gbc-tools.build").rom,
        },
	})
end

local function setup_lsp()
	nvim_lsp.gbc_analyzer.setup(config.options.server)
end

local function setup_handlers()
	local lsp_opts = config.options.server
	local tool_opts = config.options.tools
	local custom_handlers = {}

	custom_handlers["experimental/serverStatus"] = utils.mk_handler(server_status.handler)
	custom_handlers["experimental/inlayHints"] = utils.mk_handler(inlay.handler)
	custom_handlers["experimental/debuggerOutline"] = utils.mk_handler(outline.handler)

	lsp_opts.handlers = vim.tbl_deep_extend("force", custom_handlers, lsp_opts.handlers or {})
end

function M.setup(opts)
	config.setup(opts)
    outline.setup()
    setup_handlers()
	setupCommands()
    setup_lsp()
end

return M

