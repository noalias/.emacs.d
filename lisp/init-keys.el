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

(transient-define-prefix keys:action ()
  "常用命令的导航键"
  ["Quick"
   :class transient-row
   ("SPC" "Bookmark" consult-bookmark)
   ("v" "Visit package" straight-visit-package)
   ("K" "Save buffers and kill emacs" save-buffers-kill-emacs)
   ("c" "Capture ideas" org-capture)
   ("a" "Agenda" org-agenda)
   ("g" "Magit" magit)
   ]
  ["Dispatch"
   :class transient-row
   ("p" "Projects" project:keys)
   ("h" "Help" help:keys)
   ("b" "Buffer" buffer:keys)
   ("f" "File" file:keys)
   ("w" "Window" window:keys)
   ])

(provide 'init-keys)
;;; init-keys.el ends here
