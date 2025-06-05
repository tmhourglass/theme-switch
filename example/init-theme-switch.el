;;; init-theme-switch.el --- Theme Switch 示例配置

;;; Commentary:
;; 这个文件包含了 theme-switch.el 的全部配置。
;; 你可以复制这些配置到你的 Emacs 配置文件中，并根据需要进行修改。

;;; Code:

(require 'theme-switch)

;;; 主题设置
;; 设置喜欢的主题列表
(setq theme-switch-favorite-themes '(zenburn solarized-dark solarized-light leuven modus-vivendi modus-operandi))

;; 设置不想使用的主题
(setq theme-switch-excluded-themes '(user adwaita))

;;; 护眼模式设置
;; 设置日间模式主题列表（亮色主题）
(setq theme-switch-day-themes '(leuven solarized-light modus-operandi))

;; 设置夜间模式主题列表（暗色主题）
(setq theme-switch-night-themes '(zenburn solarized-dark modus-vivendi))

;; 设置日间/夜间模式的时间
(setq theme-switch-day-start "06:30")
(setq theme-switch-night-start "18:30")

;; 启用自动切换
(setq theme-switch-auto-switch-enabled t)
(setq theme-switch-auto-switch-interval 1800) ;; 30分钟检查一次

;;; 钩子设置
;; 主题加载前的钩子
(add-hook 'theme-switch-before-load-theme-hook
        (lambda (theme)
            (message "即将加载主题: %s" theme)))

;; 主题加载后的钩子（针对特定主题进行自定义调整）
(add-hook 'theme-switch-after-load-theme-hook
        (lambda (theme)
            (cond
            ((eq theme 'zenburn)
            (set-face-attribute 'region nil :background "gray30"))
            ((eq theme 'solarized-dark)
            (set-face-attribute 'mode-line nil :background "dark slate blue"))
            ((eq theme 'solarized-light)
            (set-face-attribute 'mode-line nil :background "light sky blue")))))

;;; 键绑定设置
;; 基本主题切换
(global-set-key (kbd "C-c t r") 'theme-switch-random)     ;; 随机切换主题
(global-set-key (kbd "C-c t p") 'theme-switch-previous)   ;; 切换到上一个主题
(global-set-key (kbd "C-c t l") 'theme-switch-load-theme) ;; 从列表中选择主题

;; 护眼模式
(global-set-key (kbd "C-c t e") 'theme-switch-toggle-eye-care) ;; 切换护眼模式
(global-set-key (kbd "C-c t d") 'theme-switch-day-mode)        ;; 切换到日间模式
(global-set-key (kbd "C-c t n") 'theme-switch-night-mode)      ;; 切换到夜间模式
(global-set-key (kbd "C-c t a") 'theme-switch-toggle-auto-switch) ;; 切换自动模式

;; 主题收藏管理
(global-set-key (kbd "C-c t +") 'theme-switch-add-to-favorites)     ;; 添加到收藏夹
(global-set-key (kbd "C-c t -") 'theme-switch-remove-from-favorites) ;; 从收藏夹删除
(global-set-key (kbd "C-c t f") 'theme-switch-list-favorites)        ;; 列出收藏主题

;; 排除列表管理
(global-set-key (kbd "C-c t x +") 'theme-switch-add-to-excluded)     ;; 添加到排除列表
(global-set-key (kbd "C-c t x -") 'theme-switch-remove-from-excluded) ;; 从排除列表删除
(global-set-key (kbd "C-c t x") 'theme-switch-list-excluded)          ;; 列出排除主题

;; 其他功能
(global-set-key (kbd "C-c t v") 'theme-switch-preview)   ;; 预览主题
(global-set-key (kbd "C-c t m") 'theme-switch-menu)      ;; 显示主题菜单

;;; 启用主题切换模式
(theme-switch-mode 1)

;;; 启动时根据当前时间自动选择模式（可选）
(theme-switch-auto)

(provide 'init-theme-switch)

;;; init-theme-switch.el ends here
