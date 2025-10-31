vim.g.cornelis_use_global_binary = true
vim.g.cornelis_suppress_prompts = false

vim.api.nvim_create_autocmd({"BufRead", "BufNewFile"}, {
  pattern = {"*.lagda.md"},
  callback = function()
    vim.bo.filetype = "agda"
  end
})

local map = vim.keymap.set
local opts = { buffer = true, silent = true }

local function run(cmd)
      return function()
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Esc>", true, false, true), "nx", false)
        vim.cmd(cmd)
        vim.api.nvim_feedkeys("a", "nx", false)
      end
    end

vim.api.nvim_create_autocmd("FileType", {
  pattern = "agda",
  callback = function()
    -- Load
    map("i", "<C-c><C-l>", run("CornelisLoad"), opts)
    map("i", "<C-c><C-x C-c>", run("CornelisQuit"), opts)

    -- Type / goal
    map("i", "<C-c><C-,>", run("CornelisTypeContext"), opts)
    map("i", "<C-c><C-.>", run("CornelisTypeContextInfer"), opts)
    map("i", "<C-c><C-d>", run("CornelisInfer"), opts)
    map("i", "<C-c><C-t>", run("CornelisTypeContextInfer"), opts)
    map("i", "<C-c><C-n>", run("CornelisNormalize"), opts)

    -- Case split / refine / give
    map("i", "<C-c><C-c>", run("CornelisMakeCase"), opts)
    map("i", "<C-c><C-r>", run("CornelisRefine"), opts)
    map("i", "<C-c><C-space>", run("CornelisGive"), opts)
    map("i", "<C-c><C-RET>", run("CornelisGive"), opts)

    -- Goal navigation
    map("i", "<C-c><C-f>", run("CornelisNextGoal"), opts)
    map("i", "<C-c><C-b>", run("CornelisPrevGoal"), opts)

    -- Seirch / auto
    map("i", "<C-c><C-a>", run("CornelisAuto"), opts)

    -- Context
    map("i", "<C-c><C-h>", run("CornelisHelperFunction"), opts)

    -- Comment / un-comment
    map("i", "<C-c><C-;>", run("CornelisComment"), opts)
  end,
})

vim.g.cornelis_use_floating_goal = true

-- vim.api.nvim_create_autocmd("BufWritePost", {
--   pattern = "*.agda",
--   command = "CornelisLoad"
-- })

vim.g.cornelis_rewrite_mode = "replace"
vim.g.cornelis_split_location = "bottom"
