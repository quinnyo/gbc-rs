local nvim_lsp = require("lspconfig")
local config = require("gbc-tools.config")
local utils = require("gbc-tools.utils.utils")
local inlay = require("gbc-tools.inlay_hints")
local outline = require("gbc-tools.outline")
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
        GBCDebuggerToggleBreakpoint = {
            function()
                utils.command("debugger/toggle_breakpoint", nil)   
            end,
            '-nargs=0',
            description = '`:GBCDebuggerToggleBreakpoint` Add/remove a breakpoint at the current line',
        },
        GBCDebuggerStep = {
            function()
                utils.command("debugger/step", nil)   
            end,
            '-nargs=0',
            description = '`:GBCDebuggerStep` Run the next instruction, stepping into function calls',
        },
        GBCDebuggerNext = {
            function()
                utils.command("debugger/next", nil)   
            end,
            '-nargs=0',
            description = '`:GBCDebuggerNext` Run the next instruction, stepping over function calls',
        },
        GBCDebuggerFinish = {
            function()
                utils.command("debugger/finish", nil)   
            end,
            '-nargs=0',
            description = '`:GBCDebuggerFinish` Run until the current function returns',
        },
        GBCDebuggerContinue = {
            function()
                utils.command("debugger/continue", nil)   
            end,
            '-nargs=0',
            description = '`:GBCDebuggerContinue` Continue running until next stop',
        },
        GBCDebuggerUndo = {
            function()
                utils.command("debugger/undo", nil)   
            end,
            '-nargs=0',
            description = '`:GBCDebuggerUndo` Reverts the last debugger command',
        },
        GBCDebuggerOutlineToggle = {
			outline.toggle,
            '-nargs=0',
            description = '`:GBCDebuggerOutlineToggle` Toggles the debugger outline panel',
        },
        GBCDebuggerOutlineOpen = {
			outline.open,
            '-nargs=0',
            description = '`:GBCDebuggerOutlineOpen` Opens the debugger outline panel',
        },
        GBCDebuggerOutlineClose = {
			outline.close,
            '-nargs=0',
            description = '`:GBCDebuggerOutlineClose` Closes the debugger outline panel',
        },
        GBCEmulatorStart = {
            function(model)
                utils.command("emulator/start", nil, {
                    model = model
                })    
            end,
            '-nargs=?',
            description = '`:GBCEmulatorStart` Starts the emulator for the current workspace',
        },
        GBCEmulatorStop = {
            function()
               utils.command("emulator/stop", nil) 
            end,
            '-nargs=0',
            description = '`:GBCEmulatorStop` Stops the currently running emulator',
        },
        GBCBuildRom = {
            function()
                utils.command("build/rom", nil)  
            end,
            '-nargs=0',
            description = '`:GBCBuildRom` Builds the ROM for the current workspace',
        },
        GBCRunnables = {
            function()
                utils.command("project/runnables", runnables.handler)  
            end,
            '-nargs=0',
            description = '`:GBCRunnables` List runnables for the current workspace',
        },
        GBCWriteMemory = {
            function(...)
                local args = { ... }
                if vim.tbl_islist(args) and #args == 1 and type(args[1]) == "table" then
                    args = args[1]
                end
                -- TODO if only one arg is supplied write to whatever is under the cursor
                if #args == 2 then
                    utils.command("emulator/write_memory", nil, {
                        address = args[1],
                        value = args[2]
                    })
                end
            end,
            '-nargs=*',
            description = '`:GBCWriteMemory` Writes a value to the specified memory address or variable'
        }
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

local function setup_handlers()
	local lsp_opts = config.options.server
	local tool_opts = config.options.tools
	local custom_handlers = {}

	custom_handlers["experimental/serverStatus"] = utils.mk_handler(server_status)
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

