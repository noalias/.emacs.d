;;;  -*- lexical-binding: t -*-
(progn ;    `prog-mode'
  ;; lsp-bridge-mode t
  ;; yas-minor-mode t
  
  (add-hook 'prog-mode-hook #'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'eldoc-mode)

  (defun prog:indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  
  (progn ; `lisp-mode'
    (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
    (add-hook 'emacs-lisp-mode-hook #'reveal-mode)
    (add-hook 'lisp-interaction-mode-hook #'prog:indent-spaces-mode)
    )

  (progn ; `rust-mode'
    (straight-use-package 'rust-mode)
    (add-hook 'rust-mode-hook #'prog:indent-spaces-mode)
    (setq rust-format-on-save t)
    )

  (progn ; `lua-mode'
    (straight-use-package 'lua-mode)
    (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
    (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
    (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
    )

  (progn ; `elvish-mode'
    (straight-use-package 'elvish-mode)
    (setq elvish-keywords
          '("pragma" "tmp" "set" "var" "fn" "elif" "if" "else" "try" "except" "finally" "use" "return" "while" "for" "break" "continue" "del" "and" "or" "fail" "multi-error"))
    )
  )

(progn ; `lsp'
  (progn ; `eglot'
    (add-hook 'rust-mode-hook #'eglot-ensure)
    )
  )

(progn ; `completion'
  (straight-use-package '(corfu
                          :files (:defaults "extensions/*.el")))
  ;; face
  (straight-use-package '(kind-all-the-icons
                          :host github
                          :repo "Hirozy/kind-all-the-icons"))
  (add-hook 'after-init-hook #'global-corfu-mode)
  (add-hook 'after-init-hook #'corfu-history-mode)
  (with-eval-after-load 'corfu
    (setq corfu-cycle t
          corfu-auto t
          corfu-separator ?\s
          corfu-quit-at-boundary nil   ;; Never quit at completion boundary
          corfu-quit-no-match t        ;; Never quit, even if there is no match
          corfu-preview-current nil    ;; Disable current candidate preview
          corfu-min-width 80
          corfu-max-width 100
          corfu-auto-delay 0.2
          corfu-auto-prefix 2
          corfu-preselect-first nil    ;; Disable candidate preselection
          corfu-on-exact-match nil     ;; Configure handling of exact matches
          corfu-scroll-margin 0        ;; Use scroll margin
          )
    (require 'kind-all-the-icons)
    (add-to-list 'corfu-margin-formatters 
                 #'kind-all-the-icons-margin-formatter)
    )
  )

(provide 'init-prog)
