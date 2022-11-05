;;; -*- lexical-binding: t -*-
(setq switch-to-prev-buffer-skip-regexp (rx bos
                                            ?*
                                            (or "Messages"
                                                "Output"
                                                "Compile-Log"
                                                "Completions"
                                                "Warnings"
                                                "Flymake diagnostics"
                                                "Async Shell Command"
                                                "Async-native-compile-log"
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
                                                "rustfmt")
                                            (* anything)
                                            ?*
                                            eos)
      )

(transient-define-prefix buffer:keys ()
  "Buffer 操作"
  ["Buffer Commands"
   ("SPC" "Switch to buffer" consult-buffer)
   ("o" "Switch to other window" consult-buffer-other-window)
   ("s" "Save current buffer" save-buffer)
   ("S" "Save all buffers" (lambda ()
                             (interactive)
                             (let ((buffer-save-without-query t))
                               (call-interactively #'save-some-buffers))))
   ("k" "Kill current buffer" kill-current-buffer)
   ("p" "Switch to prefix buffer" switch-to-prev-buffer)
   ("n" "Switch to next buffer" switch-to-next-buffer)
   ])

(provide 'init-buffer)
;;; init-buffer.el ends here
