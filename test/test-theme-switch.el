;;; test-theme-switch.el --- Theme Switch 测试文件 -*- lexical-binding: t -*-

;;; Commentary:
;; 这个文件包含了 theme-switch.el 的测试用例。
;; 使用 ERT (Emacs Regression Testing) 框架进行测试。

;;; Code:

(require 'ert)
(require 'theme-switch)

;;; 测试辅助函数

(defun test-theme-switch--setup ()
  "测试前的设置。"
  ;; 保存当前状态
  (setq test-theme-switch--original-themes custom-enabled-themes)
  (setq test-theme-switch--original-favorite-themes theme-switch-favorite-themes)
  (setq test-theme-switch--original-excluded-themes theme-switch-excluded-themes)
  (setq test-theme-switch--original-day-themes theme-switch-day-themes)
  (setq test-theme-switch--original-night-themes theme-switch-night-themes)
  (setq test-theme-switch--original-day-start theme-switch-day-start)
  (setq test-theme-switch--original-night-start theme-switch-night-start)
  (setq test-theme-switch--original-auto-switch-enabled theme-switch-auto-switch-enabled)
  (setq test-theme-switch--original-auto-switch-interval theme-switch-auto-switch-interval)

  ;; 设置测试环境
  (theme-switch--disable-all-themes)
  (setq theme-switch-favorite-themes '(tango tsdh-light tsdh-dark))
  (setq theme-switch-excluded-themes '(user))
  (setq theme-switch-day-themes '(tsdh-light))
  (setq theme-switch-night-themes '(tsdh-dark))
  (setq theme-switch-day-start "06:00")
  (setq theme-switch-night-start "18:00")
  (setq theme-switch-auto-switch-enabled nil)
  (setq theme-switch-auto-switch-interval 60)
  (setq theme-switch--history nil)

  ;; 清除钩子
  (setq theme-switch-before-load-theme-hook nil)
  (setq theme-switch-after-load-theme-hook nil))

(defun test-theme-switch--teardown ()
  "测试后的清理。"
  ;; 恢复原始状态
  (theme-switch--disable-all-themes)
  (mapc #'load-theme test-theme-switch--original-themes)
  (setq theme-switch-favorite-themes test-theme-switch--original-favorite-themes)
  (setq theme-switch-excluded-themes test-theme-switch--original-excluded-themes)
  (setq theme-switch-day-themes test-theme-switch--original-day-themes)
  (setq theme-switch-night-themes test-theme-switch--original-night-themes)
  (setq theme-switch-day-start test-theme-switch--original-day-start)
  (setq theme-switch-night-start test-theme-switch--original-night-start)
  (setq theme-switch-auto-switch-enabled test-theme-switch--original-auto-switch-enabled)
  (setq theme-switch-auto-switch-interval test-theme-switch--original-auto-switch-interval)
  (theme-switch--setup-timer)

  ;; 清除钩子
  (setq theme-switch-before-load-theme-hook nil)
  (setq theme-switch-after-load-theme-hook nil))

;;; 基本功能测试

(ert-deftest test-theme-switch-load-theme ()
  "测试 `theme-switch-load-theme` 函数。"
  (test-theme-switch--setup)
  (unwind-protect
      (progn
        ;; 测试加载主题
        (theme-switch-load-theme 'tango)
        (should (eq theme-switch--current-theme 'tango))
        (should (equal custom-enabled-themes '(tango)))
        (should (equal theme-switch--history '(tango)))

        ;; 测试加载另一个主题
        (theme-switch-load-theme 'tsdh-light)
        (should (eq theme-switch--current-theme 'tsdh-light))
        (should (equal custom-enabled-themes '(tsdh-light)))
        (should (equal theme-switch--history '(tsdh-light tango))))
    (test-theme-switch--teardown)))

(ert-deftest test-theme-switch-random ()
  "测试 `theme-switch-random` 函数。"
  (test-theme-switch--setup)
  (unwind-protect
      (progn
        ;; 测试随机选择主题
        (let ((random-theme nil))
          ;; 使用 mock 函数替换 random
          (cl-letf (((symbol-function 'random)
                     (lambda (n) 0))) ;; 始终返回第一个主题
            (theme-switch-random)
            (setq random-theme theme-switch--current-theme))

          ;; 验证结果
          (should (memq random-theme theme-switch-favorite-themes))
          (should (eq random-theme 'tango)) ;; 由于 mock 了 random 函数，应该总是选择第一个主题
          (should (equal custom-enabled-themes (list random-theme)))))
    (test-theme-switch--teardown)))

(ert-deftest test-theme-switch-previous ()
  "测试 `theme-switch-previous` 函数。"
  (test-theme-switch--setup)
  (unwind-protect
      (progn
        ;; 手动设置历史记录，避免依赖 theme-switch-load-theme
        (setq theme-switch--history '(tsdh-dark tsdh-light tango))
        (setq theme-switch--current-theme 'tsdh-dark)

        ;; 测试切换到上一个主题
        (theme-switch-previous)
        (should (eq theme-switch--current-theme 'tsdh-light))

        ;; 再次切换到上一个主题
        (theme-switch-previous)
        (should (eq theme-switch--current-theme 'tango)))
    (test-theme-switch--teardown)))

;;; 护眼模式测试

(ert-deftest test-theme-switch--is-day-time-p ()
  "测试 `theme-switch--is-day-time-p` 函数。"
  (test-theme-switch--setup)
  (unwind-protect
      (progn
        ;; 测试简单情况：日间在夜间之前
        (setq theme-switch-day-start "06:00")
        (setq theme-switch-night-start "18:00")

        ;; 模拟不同时间
        (cl-letf (((symbol-function 'theme-switch--current-time-in-minutes)
                   (lambda () (+ (* 5 60) 30)))) ;; 05:30
          (should-not (theme-switch--is-day-time-p)))

        (cl-letf (((symbol-function 'theme-switch--current-time-in-minutes)
                   (lambda () (+ (* 12 60) 0)))) ;; 12:00
          (should (theme-switch--is-day-time-p)))

        (cl-letf (((symbol-function 'theme-switch--current-time-in-minutes)
                   (lambda () (+ (* 20 60) 30)))) ;; 20:30
          (should-not (theme-switch--is-day-time-p)))

        ;; 测试复杂情况：日间跨越午夜
        (setq theme-switch-day-start "22:00")
        (setq theme-switch-night-start "06:00")

        (cl-letf (((symbol-function 'theme-switch--current-time-in-minutes)
                   (lambda () (+ (* 23 60) 30)))) ;; 23:30
          (should (theme-switch--is-day-time-p)))

        (cl-letf (((symbol-function 'theme-switch--current-time-in-minutes)
                   (lambda () (+ (* 3 60) 0)))) ;; 03:00
          (should (theme-switch--is-day-time-p)))

        (cl-letf (((symbol-function 'theme-switch--current-time-in-minutes)
                   (lambda () (+ (* 12 60) 0)))) ;; 12:00
          (should-not (theme-switch--is-day-time-p))))
    (test-theme-switch--teardown)))

(ert-deftest test-theme-switch-day-night-mode ()
  "测试 `theme-switch-day-mode` 和 `theme-switch-night-mode` 函数。"
  (test-theme-switch--setup)
  (unwind-protect
      (progn
        ;; 测试日间模式
        (theme-switch-day-mode)
        (should (eq theme-switch--current-theme 'tsdh-light)) ;; 应该使用日间模式主题

        ;; 测试夜间模式
        (theme-switch-night-mode)
        (should (eq theme-switch--current-theme 'tsdh-dark))) ;; 应该使用夜间模式主题
    (test-theme-switch--teardown)))

(ert-deftest test-theme-switch-auto ()
  "测试 `theme-switch-auto` 函数。"
  (test-theme-switch--setup)
  (unwind-protect
      (progn
        ;; 测试自动模式（日间）
        (cl-letf (((symbol-function 'theme-switch--is-day-time-p)
                   (lambda () t))) ;; 模拟日间时间
          (theme-switch-auto)
          (should (eq theme-switch--current-theme 'tsdh-light))) ;; 应该使用日间模式主题

        ;; 测试自动模式（夜间）
        (cl-letf (((symbol-function 'theme-switch--is-day-time-p)
                   (lambda () nil))) ;; 模拟夜间时间
          (theme-switch-auto)
          (should (eq theme-switch--current-theme 'tsdh-dark)))) ;; 应该使用夜间模式主题
    (test-theme-switch--teardown)))

;;; 自动切换测试

(ert-deftest test-theme-switch-toggle-auto-switch ()
  "测试 `theme-switch-toggle-auto-switch` 函数。"
  (test-theme-switch--setup)
  (unwind-protect
      (progn
        ;; 初始状态应该是禁用的
        (should-not theme-switch-auto-switch-enabled)
        (should-not theme-switch--timer)

        ;; 测试启用自动切换
        (theme-switch-toggle-auto-switch)
        (should theme-switch-auto-switch-enabled)
        (should theme-switch--timer)

        ;; 测试禁用自动切换
        (theme-switch-toggle-auto-switch)
        (should-not theme-switch-auto-switch-enabled)
        (should-not theme-switch--timer))
    (test-theme-switch--teardown)))

(ert-deftest test-theme-switch-mode ()
  "测试 `theme-switch-mode` 函数。"
  (test-theme-switch--setup)
  (unwind-protect
      (progn
        ;; 初始状态应该是禁用的
        (theme-switch-mode -1)
        (should-not theme-switch-mode)
        (should-not theme-switch--timer)

        ;; 测试启用主题切换模式
        (theme-switch-mode 1)
        (should theme-switch-mode)

        ;; 启用自动切换
        (setq theme-switch-auto-switch-enabled t)
        (theme-switch--setup-timer)
        (should theme-switch--timer)

        ;; 测试禁用主题切换模式
        (theme-switch-mode -1)
        (should-not theme-switch-mode)
        (should-not theme-switch--timer))
    (test-theme-switch--teardown)))

;;; 钩子测试

(ert-deftest test-theme-switch-hooks ()
  "测试 `theme-switch-before-load-theme-hook` 和 `theme-switch-after-load-theme-hook` 钩子。"
  (test-theme-switch--setup)
  (unwind-protect
      (let ((before-hook-called nil)
            (after-hook-called nil)
            (hook-theme nil))
        ;; 添加测试钩子
        (add-hook 'theme-switch-before-load-theme-hook
                  (lambda (theme)
                    (setq before-hook-called t)
                    (setq hook-theme theme)))

        (add-hook 'theme-switch-after-load-theme-hook
                  (lambda (theme)
                    (setq after-hook-called t)
                    (should (eq theme hook-theme))))

        ;; 加载主题
        (theme-switch-load-theme 'tango)

        ;; 验证钩子是否被调用
        (should before-hook-called)
        (should after-hook-called)
        (should (eq hook-theme 'tango)))
    (test-theme-switch--teardown)))

;;; 运行所有测试

(defun test-theme-switch-run-all-tests ()
  "运行所有 Theme Switch 测试。"
  (interactive)
  (ert-run-tests-interactively "^test-theme-switch-"))

(provide 'test-theme-switch)

;;; test-theme-switch.el ends here
