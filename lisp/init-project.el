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
  
(provide 'init-project)
