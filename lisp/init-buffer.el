;;; -*- lexical-binding: t -*-
(use-package emacs
  :demand t
  :bind ("s-b" . buffer:keys)
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
  (transient-define-prefix buffer:keys ()
    "Buffer 操作"
    ["Buffer Commands"
     ("SPC" "Switch to buffer" consult-buffer)
     ("o" "Switch to other window" consult-buffer-other-window)
     ("i" "Ibuffer" ibuffer)
     ("s" "Save current buffer" save-buffer)
     ("k" "Kill current buffer" kill-current-buffer)
     ("p" "Switch to prefix buffer" switch-to-prev-buffer)
     ("n" "Switch to next buffer" switch-to-next-buffer)
     ])
  (use-package consult
    :defer t
    :config
    (add-to-list 'consult-buffer-filter buffer:skip-regexp))
  )

(provide 'init-buffer)
;;; init-buffer.el ends here
