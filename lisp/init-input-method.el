;;; -*- lexical-binding: t -*-
(straight-use-package 'rime)

(progn ; `input-method'
  (setq default-input-method "rime")
  (with-eval-after-load 'rime
    (define-key rime-active-mode-map (kbd "TAB") #'rime-inline-ascii)
    (setq rime-disable-predicates '(rime-predicate-prog-in-code-p
                                    meow-normal-mode-p
                                    meow-motion-mode-p
                                    meow-keypad-mode-p
                                    meow-beacon-mode-p
				                    )
          rime-inline-predicates '(rime-predicate-space-after-cc-p
                                   rime-predicate-after-ascii-char-p
                                   )
          )
    
    
    (setq rime-inline-ascii-trigger 'shift-r)
    (setq rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g" "C-v" "M-v" "," "." "S-<return>")
          rime-inline-ascii-holder ?a
          rime-cursor "|"
          rime-show-candidate 'minibuffer
          rime-title "rime")
    
    (setq rime-user-data-dir "~/.config/rime/emacs")
    )
  )

(provide 'init-input-method)
;;; init-input-method.el ends here
