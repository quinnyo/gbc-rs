local config = require('gbc-tools.config')

local M = {}

M.state = {
    outline_items = {},
    outline_win = nil,
    outline_buf = nil,
    code_win = nil
}

M.View = {
    lines = {},
    parent = nil,
    winopts = {
        relativenumber = false,
        number = false,
        list = false,
        winfixwidth = true,
        winfixheight = true,
        foldenable = false,
        spell = false,
        signcolumn = 'yes',
        foldmethod = 'manual',
        foldcolumn = '0',
        cursorcolumn = false,
        colorcolumn = '0',
        wrap = false
    }
}

local function wipe_state()
    M.state = {outline_items = {}, flattened_outline_items = {}}
end

local function wipe_rogue_buffer()
    for _, bn in ipairs(vim.api.nvim_list_bufs()) do
        if vim.fn.bufname(bn) == "GAMEBOY" then
            return pcall(vim.api.nvim_buf_delete, bn, { force = true })
        end
    end
end

---creates the status window and sets it up
---@return string bufnr
---@return string bufnr
local function setup_view()
    -- remove any left over buffers
    wipe_rogue_buffer()

    -- create a scratch unlisted buffer
    local bufnr = vim.api.nvim_create_buf(false, true)

    -- delete buffer when window is closed / buffer is hidden
    vim.api.nvim_buf_set_option(bufnr, "bufhidden", "delete")

    local current_win = vim.api.nvim_get_current_win()
    --local current_win_width = vim.api.nvim_win_get_width(current_win)

    -- create a split
    vim.cmd(config.get_split_command())

    -- resize to a % of the current window size
    vim.cmd("vertical resize " .. math.ceil(config.options.width))

    -- get current (status) window and attach our buffer to it
    local winnr = vim.api.nvim_get_current_win()
    vim.api.nvim_win_set_buf(winnr, bufnr)

    -- window stuff
    vim.api.nvim_win_set_option(winnr, "number", false)
    vim.api.nvim_win_set_option(winnr, "relativenumber", false)
    vim.api.nvim_win_set_option(winnr, "winfixwidth", true)
    vim.api.nvim_win_set_option(winnr, "colorcolumn", '0')
    vim.api.nvim_win_set_option(winnr, "cursorcolumn", false)
    vim.api.nvim_win_set_option(winnr, "signcolumn", 'no')
    vim.api.nvim_win_set_option(winnr, "spell", false)
    vim.api.nvim_win_set_option(winnr, "foldcolumn", '0')
    vim.api.nvim_win_set_option(winnr, "list", false)
    vim.api.nvim_win_set_option(winnr, "number", false)
    vim.api.nvim_win_set_option(winnr, "relativenumber", false)

    -- buffer stuff
    vim.api.nvim_buf_set_name(bufnr, "GAMEBOY")
    vim.api.nvim_buf_set_option(bufnr, "filetype", "GameBoy")
    vim.api.nvim_buf_set_option(bufnr, "modifiable", false)
    vim.api.nvim_buf_set_option(bufnr, "swapfile", false)
    vim.api.nvim_buf_set_option(bufnr, "buftype", "nofile")

    vim.api.nvim_exec(
        [[
          augroup TroubleHighlights
            autocmd! * <buffer>
            autocmd BufEnter <buffer> lua require("gbc-tools.outline").on_enter()
          augroup END
        ]],
        false
    )
    vim.api.nvim_buf_set_keymap(bufnr, "n", "<cr>", [[<cmd>lua require("gbc-tools.outline").jump_to()<cr>]], {
        silent = true,
        noremap = true,
        nowait = true,
    })
    M.on_enter()
    return bufnr, winnr
end

local function open_view()
    M.state.code_win = vim.api.nvim_get_current_win()
    M.state.outline_buf, M.state.outline_win = setup_view()
    vim.api.nvim_buf_attach(M.state.outline_buf, false,
                            {on_detach = function(_, _) wipe_state() end})
end

local function refresh_view()
    if M.state.outline_buf then
        if M.View.lines == nil or #M.View.lines == 0 then
            M.View.lines = { "Status: No emulator connected." }
        end
        vim.api.nvim_buf_set_option(M.state.outline_buf, "modifiable", true)
        vim.api.nvim_buf_set_lines(M.state.outline_buf, 0, -1, false, M.View.lines)
        vim.api.nvim_buf_set_option(M.state.outline_buf, "modifiable", false)
    end
end

function M.jump_to()
    local locations = M.View.locations or {}
    if M.View.parent then
        local row, col = unpack(vim.api.nvim_win_get_cursor(M.state.outline_win))
        local loc = locations[""..(row - 1)]
        if loc then 
            vim.api.nvim_set_current_win(M.View.parent)
            vim.cmd("edit " .. loc.filename)
            vim.api.nvim_win_set_cursor(M.View.parent, { loc.line + 1, loc.character })
        end
    end
end

function M.on_enter()
    M.View.parent = M.View.parent or vim.fn.win_getid(vim.fn.winnr("#"))
end

function M.handler(_, result)
    M.View.lines = result.lines
    M.View.locations = result.locations or {}
    refresh_view()
end

function M.toggle()
    if M.state.outline_buf == nil then
        M.open()
        refresh_view()
    else
        M.close()
    end
end

function M.open()
    if M.state.outline_buf == nil then
        open_view()
    end
end

function M.close()
    if M.state.outline_buf then
        vim.api.nvim_win_close(M.state.outline_win, true)
    end
end

function M.setup()
    wipe_rogue_buffer()
end

return M

