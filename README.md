# Theme Switch

一个用于 Emacs 的主题切换工具，提供以下功能：

1. 维护喜欢的主题列表
2. 随机切换主题，并可添加到收藏夹
3. 基于时间的护眼模式自动切换（日间/夜间模式）及一键切换
4. 使用 consult-theme 进行主题预览

## 安装

### 手动安装

1. 下载 `theme-switch.el` 文件
2. 将其放置在 Emacs 的 load-path 中
3. 在你的 Emacs 配置文件中添加：

```elisp
(require 'theme-switch)
```

### 最小化配置示例

如果你只想快速启动并尝试基本功能，可以使用以下最小化配置：

```elisp
;; 加载 theme-switch
(require 'theme-switch)

;; 启用主题切换模式（使用默认设置）
(theme-switch-mode 1)

;; 设置一个快捷键来显示主题菜单
(global-set-key (kbd "C-c t") 'theme-switch-menu)
```

这个最小化配置将使用所有可用的主题（除了 'user'），并提供一个快捷键 `C-c t` 来访问主题菜单，从而可以使用所有功能。

详细的配置示例请参考 `example/examples.el` 文件

你可以直接调用这些函数，或者参考它们的实现来配置你自己的设置。

## 主题切换逻辑

theme-switch 提供了多种主题切换方式，每种方式的行为略有不同：

### 主题来源

- **收藏主题列表**：通过 `theme-switch-favorite-themes` 变量设置，如果为空，则使用所有可用主题减去排除列表
- **排除主题列表**：通过 `theme-switch-excluded-themes` 变量设置，默认包含 'user' 主题
- **日间主题列表**：通过 `theme-switch-day-themes` 变量设置，如果为空，则使用收藏主题列表
- **夜间主题列表**：通过 `theme-switch-night-themes` 变量设置，如果为空，则使用收藏主题列表
- **所有可用主题**：系统中安装的所有主题，减去排除列表

### 切换命令行为

- **随机切换（收藏） (`theme-switch-random-favorites`)**：从收藏主题列表中随机选择一个主题。这个操作不会改变当前的护眼模式状态。
- **随机切换（所有） (`theme-switch-random-available`)**：从所有可用主题中随机选择一个主题，忽略排除列表。这个操作不会改变当前的护眼模式状态。
- **日间模式 (`theme-switch-day-mode`)**：从日间主题列表中随机选择一个主题，并将护眼模式设置为"日间"。
- **夜间模式 (`theme-switch-night-mode`)**：从夜间主题列表中随机选择一个主题，并将护眼模式设置为"夜间"。
- **切换护眼模式 (`theme-switch-toggle-eye-care`)**：在日间、夜间和关闭模式之间循环切换。
- **自动切换 (`theme-switch-auto`)**：根据当前时间自动选择日间或夜间模式。
- **切换到上一个主题 (`theme-switch-previous`)**：切换到历史记录中的上一个使用的主题。

### 护眼模式状态

护眼模式有三种状态：
- **日间模式**：使用日间主题列表中的主题
- **夜间模式**：使用夜间主题列表中的主题
- **关闭**：不使用特定的主题列表，随机切换使用收藏主题列表

使用 `theme-switch-toggle-eye-care` 可以在这三种状态之间循环切换。

### 配置变量保存

所有配置变量（如 `theme-switch-favorite-themes`、`theme-switch-excluded-themes` 等）都通过 Emacs 的 customize 系统保存。当你使用 `theme-switch-add-to-favorites` 等函数时，相关变量会自动保存到你的 custom 文件中（通常是 `custom.el` 或者你在 `custom-file` 变量中设置的文件）。

如果你想手动设置这些变量，建议使用 customize 系统或者在配置文件中使用 `customize-set-variable` 函数，例如：

```elisp
(customize-set-variable 'theme-switch-favorite-themes '(zenburn solarized-dark))
```

## 使用方法

### 交互命令

- `M-x theme-switch-load-theme` - 从喜欢的主题列表中选择一个主题
- `M-x theme-switch-random` - 从收藏主题列表中随机选择一个主题
- `M-x theme-switch-random-available` - 从所有可用主题中随机选择一个主题
- `M-x theme-switch-day-mode` - 切换到日间模式
- `M-x theme-switch-night-mode` - 切换到夜间模式
- `M-x theme-switch-toggle-eye-care` - 在日间、夜间和关闭模式之间切换
- `M-x theme-switch-auto` - 根据当前时间自动选择模式
- `M-x theme-switch-previous` - 切换到上一个使用的主题
- `M-x theme-switch-toggle-auto-switch` - 切换是否启用自动主题切换
- `M-x theme-switch-preview` - 使用 consult-theme 预览主题
- `M-x theme-switch-menu` - 显示所有主题切换命令的菜单
- `M-x theme-switch-add-to-favorites` - 将当前主题添加到收藏夹
- `M-x theme-switch-remove-from-favorites` - 从收藏夹中删除主题
- `M-x theme-switch-add-to-excluded` - 将主题添加到排除列表
- `M-x theme-switch-remove-from-excluded` - 从排除列表中删除主题
- `M-x theme-switch-list-favorites` - 列出所有收藏的主题
- `M-x theme-switch-list-excluded` - 列出所有排除的主题

### 推荐的键绑定

建议的键绑定示例（详见 `example/examples.el`）：

```elisp
(global-set-key (kbd "C-c t r") 'theme-switch-random)      ;; 从收藏中随机切换主题
(global-set-key (kbd "C-c t R") 'theme-switch-random-available) ;; 从所有主题中随机切换
(global-set-key (kbd "C-c t e") 'theme-switch-toggle-eye-care) ;; 切换护眼模式
(global-set-key (kbd "C-c t m") 'theme-switch-menu)        ;; 主题菜单
(global-set-key (kbd "C-c t +") 'theme-switch-add-to-favorites) ;; 添加到收藏夹
(global-set-key (kbd "C-c t -") 'theme-switch-remove-from-favorites) ;; 从收藏夹删除
```

## 自定义钩子

theme-switch 提供了两个钩子，可以在主题加载前后执行自定义操作：

- `theme-switch-before-load-theme-hook` - 主题加载前执行
- `theme-switch-after-load-theme-hook` - 主题加载后执行

钩子函数接收一个参数：主题名称（符号）。使用示例请参考 `example/examples.el` 中的 `theme-switch-example-hooks` 函数。

## 系统要求

- Emacs 25.1 或更高版本
- 主题预览功能需要安装 consult 包

## 许可证

本项目采用 GPL-3.0 许可证 - 详见 LICENSE 文件
