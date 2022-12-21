;;; -*- lexical-binding: t -*-
(use-package emacs
  :init
  (use-package init-base)
  (use-package init-easy)
  (setq auto-save-silent t)   ; quietly save
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq create-lockfiles nil)
  (setq auto-save-delete-trailing-whitespace t)
  (setq delete-auto-save-files t)
  :hook (after-init-hook . recentf-mode)
  :bind ("M-c f" . file:keys)
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
    (:map vertico-map
          ("C-M-d" . consult-dir)
          ("C-M-j" . consult-dir-jump-file))
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
    (add-to-list 'consult-dir-sources 'file:consult-dir--study-source t))
  
  (transient-define-prefix file:keys ()
    "File 操作"
    ["Files Commands"
     ("SPC" "Find file" find-file)
     ("d" "Find dir" consult-dir)
     ("f" "Locate the files" consult-fd)
     ("e" "Open file with external command" consult-file-externally)
     ("g" "Find file with rg" consult-ripgrep)
     ("r" "Find recent file" consult-recent-file)
     ]))

(provide 'init-file)
;;; init-file.el ends here
