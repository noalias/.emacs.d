;;; -*- lexical-binding: t -*-
(use-package text-mode
  :init
  (setq-default major-mode 'text-mode))

(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode))

(use-package yaml-mode
  :straight t
  :bind ("C-m" . newline-and-indent))

(use-package org
  :init
  (defvar org:cloud-file)
  (defvar org:template-file)
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :config
  (require 'org-tempo)
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  (let ((default-directory org-directory))
    (setq org:cloud-file (expand-file-name "Cloud/日常.org"))
    (setq org:template-file (expand-file-name "Templates/报销.org")))
  (setq org-refile-targets `((nil :level . 1)
                             (,org:cloud-file :level . 1)
                             ("emacs.org" :level . 1)
                             ("work.org" :level . 1)
                             ("toys.org" :level . 1)
                             ("projects.org" :level . 1))
        ;; `entry' 放置在子节点首位
        org-reverse-note-order t))

(provide 'init-text)
;;; init-text.el ends here
