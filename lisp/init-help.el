;;; -*- lexical-binding: t -*-
(use-package helpful
  :straight t
  :init
  (use-package init-base)
  (use-package init-easy)
  :bind
  ("M-c h" . help:keys)
  :config
  (use-package apropos
    :defer t
    :config
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))
  (transient-define-prefix help:keys ()
    "Helps 操作"
    ["Help commands"
     ("SPC" "Help for point" helpful-at-point)
     ("f" "Help for functions" helpful-callable)
     ("x" "Help for command" helpful-command)
     ("k" "Help for key" helpful-key)
     ("p" "Help for packages" describe-package)
     ("v" "Help for values" helpful-variable)
     ("o" "Help for symbol" helpful-symbol)
     ("i" "Help for info" info-other-window)
     ("s" "Help for shortdoc" shortdoc)
     ("a" "Help for aprops" consult-apropos)
     ]))

(provide 'init-help)
;;; init-help.el ends here
