;;; -*- lexical-binding: t -*-
(progn ;    `text-mode'
  (setq-default major-mode 'text-mode)
  ;; `markdown-mode'
  (straight-use-package 'markdown-mode)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  ;; `yaml-mode'
  (straight-use-package 'yaml-mode)
  (with-eval-after-load 'yaml-mode
    (define-key yaml-mode-map (kbd "C-m") #'newline-and-indent))
  )

(with-eval-after-load 'org
  (require 'org-tempo)
  
  (defvar org:cloud-file)
  (defvar org:template-file)

  (global-set-key (kbd "C-c c") #'org-capture)
  (global-set-key (kbd "C-c a") #'org-agenda)
  
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)
  ;; refile 设置
  (let ((default-directory org-directory))
    (setq org:cloud-file (expand-file-name "Cloud/日常.org"))
    (setq org:template-file (expand-file-name "Templates/报销.org")))
    
  (setq org-refile-targets `((nil :level . 1)
                             (,org:cloud-file :level . 1)
                             ("emacs.org" :level . 1)
                             ("work.org" :level . 1)
                             ("toys.org" :level . 1)
                             ("projects.org" :level . 1))
        org-reverse-note-order t ;; `entry' 放置在子节点首位
        )
  ;; agenda 设置
  (setq org-agenda-files (list org-directory
                               org:cloud-file))
  ;; capture 设置
  (setq org-capture-templates `(("t" "For everyday's tasks"
                                 entry
                                 (file "Ideas.org")
                                 "* TODO %? :everyday:\nSCHEDULED: %^{计划时间}T DEADLINE: %^{截止时间}T"
                                 :prepend t
                                 :refile-targets ((,org:cloud-file :level . 1)) ; 注意，这里不能添加 "'"
                                 :empty-lines-after 1
                                 :kill-buffer t)
                                ("e" "For emacs configs"
                                 entry
                                 (file "emacs.org")
                                 "* TODO %? :emacs:\nSCHEDULED: %^{计划时间}T"
                                 :prepend t
                                 :refile-targets (("emacs.org" :level . 1))
                                 :empty-lines-after 1
                                 :kill-buffer t)
                                ("w" "For works")
                                ("wm" "出差报销"
                                 entry
                                 (file "work.org")
                                 (file ,org:template-file)
                                 :prepend t
                                 :empty-lines-after 1
                                 :kill-buffer t)
                                ("wo" "其它事项"
                                 entry
                                 (file "work.org")
                                 (file ,org:template-file)
                                 :prepend t
                                 :empty-lines-after 1
                                 :kill-buffer t)
                                ("y" "一些玩具的使用汇总"
                                 entry
                                 (file "toys.org")
                                 "* TODO %?"
                                 :prepend t
                                 :empty-lines-after 1
                                 :kill-buffer t)
                                ("p" "For projects"
                                 entry
                                 (file "Ideas.org")
                                 ""
                                 :prepend t
                                 :refile-targets (("project.org" :level . 1))
                                 :empty-lines-after 1
                                 :kill-buffer t)
                                )
        )
  )
(add-hook 'org-mode-hook #'valign-mode)

(provide 'init-text)
;;; init-text.el ends here
