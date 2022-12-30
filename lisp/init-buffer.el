;;; -*- lexical-binding: t -*-
(use-package emacs
  :demand
  :bind
  (:map global:commands-map
        ("b o" . consult-buffer-other-window)
        ("b i" . ibuffer)
        ("b s" . save-buffer)
        ("b k" . kill-current-buffer)
        ("b p" . switch-to-prev-buffer)
        ("b n" . switch-to-next-buffer))
  :init
  (defvar buffer:skip-regexp
    (rx bos
        ?*
        (or "Messages"
            "Output"
            "Compile-Log"
            "Completions"
            "Warnings"
            "Flymake diagnostics"
            "Async Shell Command"
            "Async-native-compile-log"
            "Native-compile-Log"
            "Apropos"
            "Backtrace"
            "prodigy"
            "help"
            "Calendar"
            "lsp-bridge"
            "Embark Actions"
            "Finder"
            "Kill Ring"
            "Embark Export:"
            "eshell"
            "epc con"
            "shell"
            "terminal"
            "vterm"
            "quickrun"
            "elfeed-entry"
            "macro expansion"
            "Agenda Commands"
            "Org Select"
            "Capture"
            "CAPTURE-"
            "prolog"
            "rustfmt"
            "Disabled Command"
            "straight-byte-compilation"
            "straight-process"
            )
        (* anything)
        ?*
        eos))
  :config
  (setq switch-to-prev-buffer-skip-regexp buffer:skip-regexp)
    
  (use-package consult
    :defer t
    :config
    (add-to-list 'consult-buffer-filter buffer:skip-regexp))
  )

(provide 'init-buffer)
;;; init-buffer.el ends here
