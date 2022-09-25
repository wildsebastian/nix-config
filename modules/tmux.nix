{ config, pkgs, ... }:

{
  programs.tmux.enable = true;

  programs.tmux.extraConfig = ''
    set -g prefix C-a
    unbind C-b
    set -g default-terminal "screen-256color"

    set -s escape-time 0
    set-option -g history-limit 10000

    bind | split-window -h
    bind - split-window -v

    bind h select-pane -L
    bind j select-pane -D
    bind k select-pane -U
    bind l select-pane -R

    bind -r C-h select-window -t :-
    bind -r C-l select-window -t :+

    bind H resize-pane -L 5
    bind J resize-pane -D 5
    bind K resize-pane -U 5
    bind L resize-pane -R 5

    run-shell "~/.tmux/themes/nord-tmux/nord.tmux"
  '';
}
