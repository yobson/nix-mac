vim.api.nvim_create_autocmd('FileType', {
  pattern = { 'haskell', 'nix' },
  callback = function() vim.treesitter.start() end,
})
