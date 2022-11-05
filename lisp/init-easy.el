;;;  -*- lexical-binding: t -*-
(require 'init-base)

(progn ;    `isearch'
  (setq isearch-allow-scroll t))

(add-hook 'after-init-hook #'savehist-mode)

(progn ; `vertico'
  (straight-use-package '(vertico
                          :files (:defaults "extensions/*.el")))
  (add-hook 'after-init-hook #'vertico-mode)
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)
  (with-eval-after-load 'vertico
    (require 'vertico-directory)
    (define-keymap
      :keymap vertico-map
      "?" #'minibuffer-completion-help
      "M-RET" #'minibuffer-force-complete-and-exit
      "M-TAB" #'minibuffer-complete
      "RET" #'vertico-directory-enter
      "DEL" #'vertico-directory-delete-char
      "M-DEL" #'vertico-directory-delete-word
      )
    
    (setq vertico-scroll-margin 0
	      vertico-resize t
	      vertico-cycle t)
    )
  )


(progn ; `orderless'
  (straight-use-package 'orderless)
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))
        )
  )

(progn ; `consult'
  (global-set-key [remap switch-to-buffer]  #'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  (global-set-key [remap bookmark-jump] #'consult-bookmark)
  (global-set-key [remap project-switch-to-buffer] #'consult-project-buffer)
  (global-set-key [remap yank-pop] #'consult-yank-pop)
  (global-set-key [remap apropos-command] #'consult-apropos)
  (global-set-key [remap goto-line] #'consult-goto-line)
  (global-set-key [remap imenu] #'consult-imenu)
  (global-set-key [remap isearch-forward] #'consult-line)
  (with-eval-after-load 'consult
    (setq consult-async-min-input 2
          consult-async-refresh-delay 0.15
          consult-async-input-debounce 0.1
          consult-async-input-throttle 0.2
          consult-narrow-key "<"
          consult-line-number-widwn t))
  )

(progn ; `embark'
  (straight-use-package 'embark)
  (global-set-key (kbd "C-;") #'embark-act)
  (global-set-key (kbd "M-.") #'embark-dwim)
  (global-set-key (kbd "C-h B") #'embark-bindings)
  (setq prefix-help-command #'embark-refix-help-command)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
  (with-eval-after-load 'embark
    (straight-use-package 'embark-consult)
    (require 'embark-consult)
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))
    )
  )

(provide 'init-easy)
;;; init-easy.el ends here
