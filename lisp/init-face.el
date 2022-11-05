;;;  -*- lexical-binding: t -*-
(require 'init-base)

(progn ; `display'
  (add-hook 'after-init-hook #'column-number-mode)

  (straight-use-package 'doom-themes)
  (straight-use-package 'doom-modeline)
  
  (load-theme 'doom-dracula :no-confirm)
  (with-eval-after-load 'doom-themes
    (setq doom-themes-enable-bold t
	      doom-themes-enable-italic t))
  
  (add-hook 'after-init-hook #'doom-modeline-mode)
  (with-eval-after-load 'doom-modeline-mode
    (setq doom-modeline-major-mode-icon t))
)

(progn ; `font'
  (setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.

  ;; config font
  (defvar face:font-size 20)

  (defvar face:default-font
    (cond (base:win-p "CaskaydiaCove NF Mono") ;; install from scoop
          (base:linux-p "CaskaydiaCove Nerd Font Mono")))

  (defvar face:emoji-font
    (cond (base:win-p "Segoe UI Emoji")
          (base:linux-p "Noto Color Emoji")))

  (defvar face:fontset-font "HarmonyOS Sans SC")

  (set-face-attribute 'default
		              nil
		              :font (font-spec :family face:default-font
				                       :size face:font-size))
  (set-fontset-font t
		            'unicode
		            (font-spec :family face:emoji-font
			                   :size face:font-size))

  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :family face:fontset-font
                :size face:font-size)))
  )

(provide 'init-face)
;;; init-face.el ends here
