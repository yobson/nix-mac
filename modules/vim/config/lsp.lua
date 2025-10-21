vim.lsp.config("hls", {
  filetypes = { 'haskell', 'lhaskell', 'cabal' },
})

vim.lsp.config("qmlls", {
  filetypes = { 'qml', 'qmljs' },
  single_file_support = true,
})

vim.lsp.enable({"hls", "qmlls"})

vim.api.nvim_create_autocmd("CursorHold", {
  callback = function()
    vim.diagnostic.open_float(nil, { focus = false })
  end
})

vim.opt.updatetime = 300
