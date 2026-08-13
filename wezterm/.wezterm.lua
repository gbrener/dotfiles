local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.color_scheme = 'DoomOne'
--config.color_scheme = 'Tomorrow (dark) (terminal.sexy)'
--config.color_scheme = 'Trim Yer Beard (terminal.sexy)'
config.font_size = 11.0
config.window_background_opacity = 0.92
config.macos_window_background_blur = 42
config.hide_tab_bar_if_only_one_tab = true
--config.window_decorations = "TITLE | RESIZE"
config.window_decorations = "RESIZE"
config.enable_scroll_bar = false

return config
