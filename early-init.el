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

;;; early-init.el ends here
;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; End:
;;; early-init.el ends here
