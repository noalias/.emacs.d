;;;  -*- lexical-binding: t -*-
(use-package prog-mode
  :defer
  :config
  (add-hook 'prog-mode-hook #'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'eldoc-mode)
  
  (defun prog:indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  
  (use-package elisp-mode
    :defer
    :config
    (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
    (add-hook 'emacs-lisp-mode-hook #'reveal-mode)
    (add-hook 'lisp-interaction-mode-hook #'prog:indent-spaces-mode))

  (use-package rust-mode
    :defer
    :straight t
    :config
    (add-hook 'rust-mode-hook #'prog:indent-spaces-mode)
    (setq rust-format-on-save t))

  ;; (use-package lua-mode
  ;;   :vc
  ;;   :commands (lua-mode lua-mode-hook)
  ;;   :mode "\\.lua\\'"
  ;;   :interpreter "lua"
  ;;   :config
  ;;   (add-hook 'lua-mode-hook #'prog:indent-spaces-mode))
  )

(use-package eglot
  :hook (rust-mode-hook . eglot-ensure))

(provide 'init-prog)
