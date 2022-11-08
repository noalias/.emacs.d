;;; -*- lexical-binding: t -*-
(progn ; `package'
  (require 'cl-lib)
  (require 'cl-generic)
  (require 'rx)
  (setq reb-re-syntax 'rx)
  
  (progn ; `straight'
    ;; config `straight'
    (setq straight-check-for-modifications '(check-on-save)
                                        ;straight-vc-git-default-protocol 'ssh
          straight-vc-git-default-clone-depth 1
          straight-base-dir (or (getenv "XDG_DATA_HOME")
                                "~/.local/share/")
          )

    ;; load `straight'
    (defvar bootstrap-version)
    (let* ((bootstrap-dir
	        (expand-file-name "straight/repos" straight-base-dir))
           (bootstrap-file
	        (expand-file-name "straight.el/bootstrap.el" bootstrap-dir))
           (bootstrap-version 6)
           )
      (unless (file-exists-p bootstrap-file)
        (or (file-exists-p bootstrap-dir)
	        (mkdir bootstrap-dir t))
        (let ((default-directory bootstrap-dir))
          (process-lines "git"
		                 "clone"
		                 "git@github.com:radian-software/straight.el.git"
		                 "--depth"
		                 "1"))
        )
      (load bootstrap-file nil 'nomessage))
    )
  
  (with-eval-after-load 'straight
    (straight-use-package 'dash)
    (require 'dash)
    (straight-use-package 'all-the-icons)
    (straight-use-package 'consult)
    (require 'consult)
    (straight-use-package 'transient)
    (straight-use-package 'no-littering)
    (require 'no-littering)
    )
  )

(progn ; `var'
  (defconst base:win-p (eq system-type 'windows-nt))
  (defconst base:linux-p (eq system-type 'gnu/linux))
  )

(progn ; `function'
  )

(progn ; `edit'
  (add-hook 'after-init-hook #'electric-pair-mode)
  (add-hook 'after-init-hook #'show-paren-mode)
  (setq show-paren-sytle 'parentthesis)
  
  (setq-default fill-column 80
                tab-width 4
                indent-tabs-mode nil)
  )

(progn ; `Encoding'
  ;; UTF-8 as the default coding system
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))

  (prefer-coding-system 'utf-8-auto)
  (when base:win-p
    (set-language-environment 'chinese-gbk)
    (set-file-name-coding-system 'gbk)
    ;; (set-selection-coding-system 'gb2312)
    )
  )

(progn ; `lisp-config'
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  (setq server-auth-dir (expand-file-name "server" no-littering-var-directory))
  (progn ; `custom-file'
    (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
    (when (file-exists-p custom-file)
      (load custom-file)))
  )

(progn ; `misc'
  (fset 'yes-or-no-p 'y-or-n-p)
  
  (setq visible-bell t
        ring-bell-function 'ignore
        )
  )

(provide 'init-base)
;;; init-base.el ends here
