;;; -*- lexical-binding: t -*-
(use-package emacs
  :init
  (setq auto-save-silent t   ; quietly save
        make-backup-files nil
        auto-save-default nil
        create-lockfiles nil
        auto-save-delete-trailing-whitespace t
        delete-auto-save-files t)
  :hook (after-init-hook . recentf-mode)
  :bind
  (:map global:commands-map
        ("f SPC" . find-file)
        ("f d" . consult-dir)
        ("f f" . consult-fd)
        ("f e" . consult-file-externally)
        ("f g" . consult-ripgrep)
        ("f r" . consult-recent-file))
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude (rx bos ?.
                                    (or (seq "do" (or ?c ?t) (? ?x))
                                        "ppt")
                                    eos)))

(provide 'init-file)
;;; init-file.el ends here
