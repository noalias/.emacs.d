;;;  -*- lexical-binding: t -*-
(use-package emacs
  :init (use-package init-base)
  :hook ((after-init-hook . savehist-mode)
         (minibuffer-setup-hook . cursor-intangible-mode))
  :config
  (setq isearch-allow-scroll t)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*.el"))
  :hook after-init-hook
  :bind
  (:map vertico-map
	("?" . minibuffer-completion-help)
	("M-RET" . minibuffer-force-complete-and-exit)
	("M-TAB" . minibuffer-complete)
	("RET" . vertico-directory-enter)
	("DEL" . vertico-directory-delete-char)
	("M-DEL" . vertico-directory-delete-word))
  :config
  (use-package vertico-directory)
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy)
  (setq vertico-scroll-margin 0
	    vertico-resize t
	    vertico-cycle t))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :straight t
  :demand t
  :bind
  (([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap yank-pop] . consult-yank-pop)
   ([remap apropos-command] . consult-apropos)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap isearch-forward] . consult-line))
  :config
  (setq consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-debounce 0.1
        consult-async-input-throttle 0.2
        consult-narrow-key "<"
        consult-line-number-widwn t)
  
  (defvar consult--fd-command nil)
  (if base:win-p
      (modify-coding-system-alist 'process "fd" '(utf-8 . gbk)))
  
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))
  
  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial)))))

(use-package embark
  :straight t
  :bind
  (("C-;" . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-refix-help-command)
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)
  (use-package embark-consult
    :straight t
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))))

(provide 'init-easy)
;;; init-easy.el ends here
