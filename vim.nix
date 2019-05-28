{ config, pkgs, ... }:

{
  programs.vim = {
    enable = true;
    enableSensible = true;
    plugins = [
      { names = [
          "base16-vim"
          "direnv-vim"
          "editorconfig-vim"
          "fugitive"
          "fugitive-gitlab-vim"
          "fzfWrapper"
          "vim-gutentags"
          "hlint-refactor-vim"
          "LanguageClient-neovim"
          "lightline-vim"
          "vim-gitbranch"
          "vim-multiple-cursors"
          "vim-signify"
          "youcompleteme"
        ];
      }
      { names = [ "vim-nix" "vim-addon-nix" ]; filename_regex = "^.nix\$"; }
      { name = "python-mode"; filename_regex = "^.py\$"; }
    ];

    vimConfig = ''
      scriptencoding utf-8
      set encoding=utf-8
      set backspace=2
      set tabstop=2
      set softtabstop=0
      set shiftwidth=2
      set expandtab

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

      set statusline+=%{gutentags#statusline()}
      let g:LanguageClient_serverCommands = {
        \ 'haskell': ['hie-wrapper'],
        \ 'python': ['pyls'],
        \ }
    '';
  };
}
