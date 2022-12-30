;;;  -*- lexical-binding: t -*-
(use-package emacs
  :config
  (setq delete-by-moving-to-trash t)       ; Deleting files go to OS's trash folder
  (when base:win-p
    ;; 是否使用外部 ls.exe
    (if (setq ls-lisp-use-insert-directory-program nil)
        (modify-coding-system-alist 'process
                                    "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . gbk-dos))
      (setq ls-lisp-format-time-list '("👌%m-%d %H:%M" "👎%m-%d %H:%M")))))

(use-package dired
  :bind
  (:map dired-mode-map
   ;; 折叠子目录
   ("TAB" . dired-hide-subdir)
   ("C-k" . dired-kill-subdir)
   ("M-p" . dired-prev-subdir)
   ("M-n" . dired-next-subdir)
   ;; `f' 进入目录或文件
   ;; `b' 返回上级目录
   ("b" . dired-up-directory)
   ("e" . dired:find-file-externally))
  :config
  (setq dired-listing-switches "-algGh --time-style=iso"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-kill-when-opening-new-dired-buffer t
        )
  (setq dired-guess-shell-alist-user
        `(("elvish tar.elv")
          ("\\.elv\\'" "elvish")
          ("\\.pdf\\'" "mupdf &")
          (,(rx bos ?.
                (or (seq "do" (or ?c ?t) (? ?x))
                    "ppt")
                eos)
           "wps &")))
      
  (defun dired:find-file-externally (&optional arg)
    "Open marked or current file in operating system's default application."
    (interactive "P")
    (dired-map-over-marks
     (consult-file-externally (dired-get-file-for-visit))
     arg))

  (defun dired:mupdf-merge-files (&optional arg)
    "使用 mupdf tool 将文件合并为一个 pdf 文件"
    (interactive "P")
    (unless (executable-find "mutool")
      (user-error "Can not find *mutool*"))
    (let ((files (dired-get-marked-files :no-dir)))
      (if (length< files 2)
          (user-error "Less files to merge"))
      (message "%s"
       (concat
        (shell-quote-argument "mutool merge -o output.pdf ")
        (combine-and-quote-strings
         (mapcar
          (lambda (s) (encode-coding-string s 'gbk))
          files))))))
  
  (use-package dired-aux
    :config
    (add-to-list 'dired-compress-file-suffixes
               (list
                (rx ?. (or "zip" "7z") eos)
                ""
                "7z x -aoa -o%o %i"))))

(use-package dired-x
  :hook (dired-mode-hook . dired-omit-mode)
  :config
  (setq dired-omit-files
        (rx bos (or (seq "desktop.ini")
                    (seq ?~ (? ?$) (* (or alnum (category chinese-two-byte))) (? ".tmp"))
                    eos))))

(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode))

;; (use-package all-the-icons-dired
;;   :straight t
;;   :if base:display-graphic-p
;;   :hook dired-mode-hook)

(provide 'init-dired)
;;; init-dired.el ends here
