;;; -*- lexical-binding: t -*-
(require 'init-base)
(straight-use-package 'dashboard)
(straight-use-package 'page-break-lines)

(dashboard-setup-startup-hook)
(with-eval-after-load 'dashboard
  (page-break-lines-mode)
  (setq dashboard-banner-logo-title "Welcome to Emacs!"
        dashboard-startup-banner 'logo
        dashboard-center-content t)
  (setq dashboard-items '((bookmarks . 7)
                          (recents . 5)))
  (setq dashboard-set-heading-icons t
        dashboard-set-files-icons t)
  (dashboard-modify-heading-icons '((bookmarks . "book")
                                    (recents . "file-text")))
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)

  (define-keymap
    :keymap dashboard-mode-map
    "k" nil
    "j" nil
    "m" nil
    "r" nil
    "n" #'dashboard-next-line
    "p" #'dashboard-previous-line
    "o" #'dashboard-return
    "M" #'dashboard-jump-to-bookmarks
    "R" #'dashboard-jump-to-recents
    )
  )

(provide 'init-dashboard)
;;; init-dashboard.el ends here
