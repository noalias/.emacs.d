;;;  -*- lexical-binding: t -*-
(use-package magit
  :straight t
  :defer t
  :config
  (autoload 'magit-add-section-hook "magit-section")
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package project
  :bind
  (:map global:commands-map
        ("p SPC" . project-dired)
        ("p b" . consult-project-buffer)
        ("p k" . project-kill-buffers)
        ("p D" . project-find-dir)
        ("p p" . project-switch-project)
        ("p o" . project-find-file)
        ("p ." . project-shell-command)
        ("p ," . project-async-shell-command)
        ("p g" . magit-project-status)))
  
(provide 'init-project)
