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

(use-package emacs
  :bind ("M-c p" . project:keys)
  :config
  (transient-define-prefix project:keys ()
    "Project 操作"
    ["Project Commands"
     ("SPC" "Dired for project" project-dired)
     ("b" "Switch to buffer in project" consult-project-buffer)
     ("k" "Kill buffers in project" project-kill-buffers)
     ("D" "Dired for project directorys" project-find-dir)
     ("p" "Switch project" project-switch-project)
     ("o" "Open file in project" project-find-file)
     ("." "Run shell command in project" project-shell-command)
     ("," "Run shell command in project async" project-async-shell-command)
     ("g" "Magit" magit-project-status)
     ]))
  
(provide 'init-project)
