{ config, pkgs, ... }:

{
  programs.vim = {
    enable = true;
    enableSensible = true;
    plugins = [
      { names = [
          "ale"
          "base16-vim"
          "direnv-vim"
          "editorconfig-vim"
          "fzfWrapper"
          "lightline-vim"
          "vim-gitbranch"
          "vim-multiple-cursors"
          "vim-signify"
        ];
      }
      { names = [ "vim-nix" "vim-addon-nix" ]; filename_regex = "^.nix\$"; }
    ];

    vimConfig = ''
      set hidden

      set nobackup
      set nowritebackup

      scriptencoding utf-8
      set cmdheight=2
      set updatetime=300
      set shortmess+=c
      set signcolumn=yes
      set encoding=utf-8
      set backspace=2
      set tabstop=2
      set softtabstop=0
      set shiftwidth=2
      set expandtab
      set nofoldenable

      set number
      set showcmd
      set cursorline
      set showmatch
      set colorcolumn=80
      colorscheme base16-solarized-dark
      syntax enable

      set laststatus=2
      let g:lightline = {
        \ 'colorscheme': 'solarized',
        \ 'active': {
        \   'left': [ [ 'mode', 'paste' ],
        \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
        \ },
        \ 'component_function': {
        \   'gitbranch': 'gitbranch#name'
        \ },
      \ }
    '';
  };
}
