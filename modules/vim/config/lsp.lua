vim.lsp.config("hls", {
  filetypes = { 'haskell', 'lhaskell', 'cabal' },
})

vim.lsp.config("qmlls", {
  filetypes = { 'qml', 'qmljs' },
  single_file_support = true,
})

vim.lsp.enable({"hls", "qmlls"})
