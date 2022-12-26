;;;  -*- lexical-binding: t -*-
(use-package prog-mode
  :defer t
  :config
  (add-hook 'prog-mode-hook #'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'eldoc-mode)
  
  (defun prog:indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  
  (use-package elisp-mode
    :defer t
    :config
    (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
    (add-hook 'emacs-lisp-mode-hook #'reveal-mode)
    (add-hook 'lisp-interaction-mode-hook #'prog:indent-spaces-mode))

  (use-package rust-mode
    :defer t
    :straight t
    :config
    (add-hook 'rust-mode-hook #'prog:indent-spaces-mode)
    (setq rust-format-on-save t))

  (use-package lua-mode
    :straight t
    :commands (lua-mode lua-mode-hook)
    :mode "\\.lua\\'"
    :interpreter "lua"
    :config
    (add-hook 'lua-mode-hook #'prog:indent-spaces-mode))
  )

(use-package eglot
  :hook (rust-mode-hook . eglot-ensure))

(use-package company
  :straight t
  :init
  (use-package init-base)
  :hook
  (after-init-hook . global-company-mode)
  :bind
  (("M-/" . company-complete)
   :map company-active-map
   ([remap company-show-doc-buffer] . company-box-doc-manually))
  :config
  (setq company-global-modes '(not message-mode
                                   help-mode
                                   eshell-mode
                                   shell-mode))
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 2
        company-icon-margin 3
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-backends '((company-capf :with company-yasnippet)
                           (company-keywords company-files)))
  
  (use-package orderless
    :no-require t
    :config
    (setq orderless-component-separator "[ &]")
    (defun just-one-face (fn &rest args)
      (let ((orgerless-match-faces [completions-common-part]))
        (apply fn args)))
    (advice-add 'company-capf--candidates :around #'just-one-face))
  
  (use-package company-box
    :straight t
    :hook company-mode-hook
    :config
    (setq company-box-enable-icon t
          company-box-backends-colors nil
          company-box-show-single-candidate t
          company-box-doc-delay 0.5)

    (defun prog:company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'prog:company-box-icons--elisp)

    (defun prog:company-box--display (string on-update)
      "Display the completions."
      (company-box--render-buffer string on-update)
      (let ((frame (company-box--get-frame))
            (border-color (face-foreground 'font-lock-comment-face nil t)))
        (unless frame
          (setq frame (company-box--make-frame))
          (company-box--set-frame frame))
        (company-box--compute-frame-position frame)
        (company-box--move-selection t)
        (company-box--update-frame-position frame)
        (unless (frame-visible-p frame)
          (make-frame-visible frame))
        (company-box--update-scrollbar frame t)
        (set-face-background 'internal-border border-color frame)
        (when (facep 'child-frame-border)
          (set-face-background 'child-frame-border border-color frame)))
      (with-current-buffer (company-box--get-buffer)
        (company-box--maybe-move-number (or company-box--last-start 1))))
    (advice-add #'company-box--display :override #'prog:company-box--display)

    (when base:display-graphic-p
      (setq company-box-icons-all-the-icons
            `((Unknown       . ,(all-the-icons-material "find_in_page" :height 1.0 :v-adjust -0.2))
              (Text          . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
              (Method        . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Function      . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Constructor   . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
              (Field         . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
              (Variable      . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
              (Class         . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Interface     . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Module        . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Property      . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
              (Unit          . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
              (Value         . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
              (Enum          . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Keyword       . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
              (Snippet       . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
              (Color         . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
              (File          . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
              (Reference     . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
              (Folder        . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
              (EnumMember    . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
              (Constant      . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
              (Struct        . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
              (Event         . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
              (Operator      . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
              (TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
              (Template      . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
            company-box-icons-alist 'company-box-icons-all-the-icons))
    )
  )

(provide 'init-prog)
