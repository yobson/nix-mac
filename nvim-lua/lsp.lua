require('lspconfig')['hls'].setup{
  filetypes = { 'haskell', 'lhaskell', 'cabal' },
}

require('lspconfig')['qmlls'].setup{
  filetypes = { 'qml', 'qmljs' },
  single_file_support = true,
}
