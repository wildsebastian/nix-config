{ config, pkgs, ... }:

{
  programs.tmux.enable = true;
  programs.tmux.enableSensible = false;
  programs.tmux.enableMouse = true;
  programs.tmux.enableFzf = true;
  programs.tmux.enableVim = true;

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

    # set-option -g allow-rename off
    # set-option -g status on
    # set-option -g status-interval 300
    # set-option -g status-justify centre
    # set-option -g status-keys vi
    # set-option -g status-left "#[fg=white,bg=default] #(tmux_uptime.sh)#[fg=white,bg=default]"
    # set-option -g status-left-length 60
    # set-option -g status-left-style default
    # set-option -g status-position bottom
    # set-option -g status-right "#[fg=white,bg=default]%a %d.%m.%Y %H:%M"
    # set-option -g status-right-length 40
    # set-option -g status-right-style default
    # set-option -g status-style fg=colour136,bg=colour235

    # default window title colors
    # set-window-option -g window-status-style fg=colour244  # base0
    # set-window-option -g window-status-style bg=default

    # active window title colors
    # set-window-option -g window-status-current-style fg=colour166  # orange
    # set-window-option -g window-status-current-style bg=default
    run-shell "~/.tmux/themes/nord-tmux/nord.tmux"
    '';
}
