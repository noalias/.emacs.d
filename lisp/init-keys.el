;;; -*- lexical-binding: t -*-
(require 'init-base)
(require 'init-file)
(require 'init-project)
(require 'init-window)
(require 'init-help)
(require 'init-buffer)

(global-set-key (kbd "C-x B") #'ibuffer)
(global-set-key (kbd "M-SPC") #'keys:action)
(global-set-key (kbd "M-c") #'edit:keys)

(global-unset-key (kbd "C-x C-o"))
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x C-d"))
(global-unset-key (kbd "C-x C-k"))
(global-unset-key (kbd "C-h C-f"))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key (kbd "C-x C-p"))
(global-unset-key (kbd "C-h C-a"))

(when base:win-p
  (setq w32-pass-lwindow-to-system nil) 
  (setq w32-lwindow-modifier 'super)
  (w32-register-hot-key [s-]))

(global-set-key (kbd "s-m") #'consult-bookmark)
(global-set-key (kbd "s-g") #'magit)
(global-set-key (kbd "s-c") #'org-capture)
(global-set-key (kbd "s-a") #'org-agenda)
(global-set-key (kbd "s-v") #'straight-visit-package)
(global-set-key (kbd "s-q") #'save-buffers-kill-emacs)
(global-set-key (kbd "s-k") #'kill-current-buffer)

(provide 'init-keys)
;;; init-keys.el ends here
