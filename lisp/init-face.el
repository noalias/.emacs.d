;;;  -*- lexical-binding: t -*-
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-dracula :no-confirm)
  (setq doom-themes-enable-bold t
	    doom-themes-enable-italic t))

(use-package doom-modeline
  :straight t
  :hook after-init-hook
  :config
  (setq doom-modeline-major-mode-icon t))

(use-package emacs
  :init
  (use-package init-base)
  ;; config font
  (defvar face:font-size 20)

  (defvar face:default-font
    (cond (base:win-p "CaskaydiaCove NF Mono") ;; install from scoop
          (base:linux-p "CaskaydiaCove Nerd Font Mono")))

  (defvar face:emoji-font
    (cond (base:win-p "Segoe UI Emoji")
          (base:linux-p "Noto Color Emoji")))

  (defvar face:fontset-font "HarmonyOS Sans SC")
  :hook (after-init-hook . column-number-mode)
  :config
  (progn ; `font'
    (setq inhibit-compacting-font-caches t)  ; Don’t compact font caches during GC.
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
                  :size face:font-size)))))

(provide 'init-face)
;;; init-face.el ends here
