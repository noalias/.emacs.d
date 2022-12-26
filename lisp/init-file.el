;;; -*- lexical-binding: t -*-
(use-package emacs
  :init
  (use-package init-base)
  (setq auto-save-silent t)   ; quietly save
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil)
  (setq auto-save-delete-trailing-whitespace t)
  (setq delete-auto-save-files t)
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
                                    eos))
  (use-package consult-dir
    :straight t
    :bind
    ;(:map vertico-map
    ;      ("C-M-d" . consult-dir)
    ;      ("C-M-j" . consult-dir-jump-file))
    :config
    (defun file:consult-dir--work-dirs ()
      "Return a list of work dirs."
      (let ((target-dir "d:/work/"))
        (cons target-dir
              (cl-remove-if
               (lambda (file)
                 (or (not (file-directory-p file))
                     (string-prefix-p "." (file-name-base file))))
               (directory-files target-dir :full)))))
    (defvar file:consult-dir--work-source
      `(:name "Work dirs"
        :narrow   ?w
        :category file
        :face     consult-file
        :history  file-name-history
        :enabled  ,(lambda () (file-exists-p "d:/work/"))
        :items    ,#'file:consult-dir--work-dirs)
      "Fasd directory source for `consult-dir'.")
    (add-to-list 'consult-dir-sources 'file:consult-dir--work-source t)

    (defun file:consult-dir--study-dirs ()
      "Return a list of work dirs."
      (let ((target-dir "d:/study/"))
        (cons target-dir
              (cl-remove-if
               (lambda (file)
                 (or (not (file-directory-p file))
                     (string-prefix-p "." (file-name-base file))))
               (directory-files target-dir :full)))))
    (defvar file:consult-dir--study-source
      `(:name "Study dirs"
        :narrow   ?s
        :category file
        :face     consult-file
        :history  file-name-history
        :enabled  ,(lambda () (file-exists-p "d:/study/"))
        :items    ,#'file:consult-dir--study-dirs)
      "Fasd directory source for `consult-dir'.")
    (add-to-list 'consult-dir-sources 'file:consult-dir--study-source t)))

(provide 'init-file)
;;; init-file.el ends here
