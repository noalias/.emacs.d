;;; early-init.el --- earliest birds               -*- lexical-binding: t -*-
(setq load-prefer-newer t)
(setq byte-compile-warnings nil)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
	(expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; packages config
(setq package-enable-at-startup nil
      package-archives
      (list (cons "gnu" "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
            (cons "melpa" "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
            (cons "nognu" "http://mirrors.tuna.tsinghua.edu.cn/elpa/nognu/")
            (cons "org" "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))


;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

(progn ; `straight'
    ;; config `straight'
    (setq straight-check-for-modifications '(check-on-save)
          straight-vc-git-default-clone-depth 1
          straight-base-dir (or (getenv "XDG_DATA_HOME")
                                "~/.local/share/")
          ;; 在 `use-package' 使用 `straight' 安装包
          ;; straight-use-package-by-default t
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
  (straight-use-package 'use-package)
  ;; 禁止自动添加 `-hook'
  (setq use-package-hook-name-suffix nil)
  (require 'use-package)
  )

;;; early-init.el ends here
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
