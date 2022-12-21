;;; -*- lexical-binding: t -*-
(use-package emacs
  :demand t
  :init
  (use-package all-the-icons
    :straight t)
  (use-package no-littering
    :straight t)

  (defconst base:win-p (eq system-type 'windows-nt))
  (defconst base:linux-p (eq system-type 'gnu/linux))
  (defconst base:display-graphic-p (and (display-graphic-p)
                                        (featurep 'all-the-icons)))
  :bind
  (("s-m" . consult-bookmark)
   ("s-g" . magit)
   ("s-c" . org-capture)
   ("s-a" . org-agenda)
   ("s-v" . straight-visit-package)
   ("s-q" . save-buffers-kill-emacs)
   ("s-k" . kill-current-buffer)
   ("M-c" . edit:keys))
  :config
  (progn ; `custom-file'
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file)))
  
  (setq server-auth-dir (expand-file-name "server" no-littering-var-directory))
  
  (when base:win-p
    (setq w32-pass-lwindow-to-system nil) 
    (setq w32-lwindow-modifier 'super)
    (w32-register-hot-key [s-]))
  
  (progn ; `Encoding'
    ;; (set-language-environment               "UTF-8")     ;; System default coding
    ;; (prefer-coding-system                   'utf-8)      ;; prefer
    ;; (set-buffer-file-coding-system          'utf-8-unix) ;;
    ;; (set-charset-priority                   'unicode)    ;;
    ;; (set-clipboard-coding-system            'utf-8-unix) ;; clipboard
    ;; (set-default-coding-systems             'utf-8)      ;; buffer/file: 打开文件时的默认编码
    ;; (set-file-name-coding-system            'utf-8-unix) ;; unix/linux/macos
    ;; (set-keyboard-coding-system             'utf-8-unix) ;; keyboard
    ;; (set-next-selection-coding-system       'utf-8-unix) ;; selection
    ;; (set-selection-coding-system            'utf-8)      ;; selection
    ;; (set-terminal-coding-system             'utf-8-unix) ;; terminal
    ;; (setq coding-system-for-read            'utf-8)      ;;
    ;; (setq default-buffer-file-coding-system 'utf-8)      ;;
    ;; (setq locale-coding-system              'utf-8)      ;; local
    ;; UTF-8 as the default coding system
    (when (fboundp 'set-charset-priority)
      (set-charset-priority 'unicode))
    (prefer-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8)
    (setq system-time-locale "C")
    (unless base:win-p
      (set-language-environment 'utf-8)
      (set-file-name-coding-system 'utf-8)
      (set-selection-coding-system 'utf-8)))

  (progn ; `unset-keys'
    (global-unset-key (kbd "C-x C-o"))
    (global-unset-key (kbd "C-x f"))
    (global-unset-key (kbd "C-x C-d"))
    (global-unset-key (kbd "C-x C-k"))
    (global-unset-key (kbd "C-h C-f"))
    (global-unset-key (kbd "C-x C-b"))
    (global-unset-key (kbd "C-x C-p"))
    (global-unset-key (kbd "C-h C-a")))
  
  (progn ; `misc'
    (fset 'yes-or-no-p 'y-or-n-p)
    (setq visible-bell t
          ring-bell-function 'ignore)))

(provide 'init-base)
;;; init-base.el ends here
