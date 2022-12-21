;;;  -*- lexical-binding: t -*-
(defvar window:boring-buffers-and-modes
  '("\\*Messages\\*"
    "Output\\*$" "\\*Pp Eval Output\\*$"
    "\\*Compile-Log\\*"
    "\\*Completions\\*"
    "\\*Warnings\\*"
    "\\*Flymake diagnostics.*\\*"
    "\\*Async Shell Command\\*"
    "\\*Apropos\\*"
    "\\*Backtrace\\*"
    "\\*prodigy\\*"
    "\\*Calendar\\*"
    "\\*Embark Actions\\*"
    "\\*Finder\\*"
    "\\*Kill Ring\\*"
    "\\*Embark Export:.*\\*"
    bookmark-bmenu-mode
    lsp-bridge-ref-mode
    comint-mode
    compilation-mode
    help-mode helpful-mode
    tabulated-list-mode
    Buffer-menu-mode
    occur-mode
    gnus-article-mode devdocs-mode
    grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
    ivy-occur-mode ivy-occur-grep-mode
    process-menu-mode list-environment-mode cargo-process-mode
    youdao-dictionary-mode osx-dictionary-mode fanyi-mode

    "^\\*eshell.*\\*.*$" eshell-mode
    "^\\*shell.*\\*.*$"  shell-mode
    "^\\*terminal.*\\*.*$" term-mode
    "^\\*vterm.*\\*.*$"  vterm-mode

    "\\*DAP Templates\\*$" dap-server-log-mode
    "\\*ELP Profiling Restuls\\*" profiler-report-mode
    "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
    "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
    "\\*[Wo]*Man.*\\*$"
    "\\*ert\\*$" overseer-buffer-mode
    "\\*gud-debug\\*$"
    "\\*lsp-help\\*$" "\\*lsp session\\*$"
    "\\*quickrun\\*$"
    "\\*tldr\\*$"
    "\\*vc-.*\\*$"
    "^\\*elfeed-entry\\*$"
    "^\\*macro expansion\\**"

    "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
    "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
    "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
    "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
    "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
    rustic-cargo-outdated-mode rustic-cargo-test-moed))

(use-package emacs
  :init
  (use-package init-base)
  :demand t
  :bind
  ("s-w" . window:keys)
  :after (winner)
  :config
  (transient-define-prefix window:keys ()
    "Window 操作"
    ["Window Commands"
     ["Select"
      ("SPC" "Switch" other-window :transient t)
      ("p" "Select the up window" windmove-up)
      ("n" "Select the below window" windmove-down)
      ("f" "Select the right window" windmove-right)
      ("b" "Select the left window" windmove-left)
      ]
     ["Actions"
      ("o" "Delete other windows" delete-other-windows)
      ("q" "Delete window" delete-window)
      ("," "Undo changes in window configuration" winner-undo :transient t)
      ("." "Restore a recent window configuration" winner-redo :transient t)
      ("z" "Restore latest popper" popper-toggle-latest)
      ]
     ["Resize"
      ("h" "←" shrink-window-horizontally :transient t)
      ("j" "↓" enlarge-window :transient t)
      ("k" "↑" shrink-window :transient t)
      ("l" "→" enlarge-window-horizontally :transient t)
      ("=" "balance" balance-windows)
      ]
     ["Split"
      ("d" "Split window right" split-window-right)
      ("s" "Split window below" split-window-below)
      ]
     ]))

(use-package winner
  :straight t
  :hook after-init-hook
  :commands (winner-undo winner-redo)
  :config
  (setq winner-boring-buffers '("*Completions*"
                                  "*Compile-Log*"
                                  "*inferior-lisp*"
                                  "*Fuzzy Completions*"
                                  "*Apropos*"
                                  "*Help*"
                                  "*cvs*"
                                  "*Buffer List*"
                                  "*Ibuffer*"
                                  "*esh command on file*")))

(use-package popper
  :straight t
  :hook after-init-hook
  :bind
  (("C-<tab>" . popper-cycle)
   ("C-M-<tab>" . popper-toggle-type))
  :config
  (defun popper:fit-window-height (win)
        "Determine the height of popup window WIN by fitting it to the buffer's content."
        (fit-window-to-buffer
         win
         (floor (frame-height) 3)
         (floor (frame-height) 3)))

  (defun popper:close-window (&rest _)
        "Close popper window via `C-g'."
        ;; `C-g' can deactivate region
        (when (and (called-interactively-p 'interactive)
                   (not (region-active-p))
                   popper-open-popup-alist)
          (let ((window (caar popper-open-popup-alist)))
            (when (window-live-p window)
              (delete-window window)))))
  
  (setq popper-window-height #'popper:fit-window-height)
  (advice-add #'keyboard-quit :before #'popper:close-window)
  (setq popper-echo-dispatch-actions t)
  (popper-echo-mode 1)
  (when base:display-graphic-p
    (setq popper-mode-line
          '(:eval (format " %s "
                          (all-the-icons-octicon
                           "pin"
                           :height 0.9
                           :v-adjust 0.0
                           :face 'mode-line-emphasis)))))
  (setq popper-reference-buffers window:boring-buffers-and-modes))

(provide 'init-window)
;;; init-window.el ends here
