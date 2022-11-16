;;;  -*- lexical-binding: t -*-
(require 'init-base)

(setq delete-by-moving-to-trash t)       ; Deleting files go to OS's trash folder

(when base:win-p
  ;; 是否使用外部 ls.exe
  (if (setq ls-lisp-use-insert-directory-program nil)
      (modify-coding-system-alist 'process
                                  "[cC][mM][dD][pP][rR][oO][xX][yY]" '(gbk-dos . gbk-dos))
    (setq ls-lisp-format-time-list '("👌%m-%d %H:%M" "👎%m-%d %H:%M"))
    )
  )

(progn ;`hooks'
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (when (display-graphic-p)
    (straight-use-package 'all-the-icons-dired)
    (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))
  )

(straight-use-package 'diredfl)
(with-eval-after-load 'dired
  (diredfl-global-mode)

  (progn ; `functions'
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
      (let ((files (dired-get-marked-files 'no-dir arg
                                          (lambda (file)
                                            (string-match-p
                                             (rx bos (or "jpg" "pdf" "png") eos)
                                             (file-name-extension file))))))
        (if (length< files 2)
            (user-error "Less files to merge"))
        (process-lines "mutool"
                       "merge"
                       (string-join files " ")
                       "-o"
                       "output.pdf"))
      )
    )

  (setq dired-listing-switches "-algGh --time-style=iso"
        dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-kill-when-opening-new-dired-buffer t
        )

  (progn ;`keybindings'
    (define-keymap
      :prefix 'dired-mode-regexp-map
      "u" #'dired-upcase
      "l" #'dired-downcase
      "d" #'dired-flag-files-regexp
      "g" #'dired-mark-files-containing-regexp
      "m" #'dired-mark-files-regexp
      "r" #'dired-do-rename-regexp
      "C" #'dired-do-copy-regexp
      "S" #'dired-do-symlink-regexp
      "Y" #'dired-do-relsymlink-regexp
      "&" #'dired-flag-garbage-files
      )

    (define-keymap
      :keymap dired-mode-map
      ;; 折叠子目录
      "TAB" #'dired-hide-subdir
      "C-k" #'dired-kill-subdir
      "M-p" #'dired-prev-subdir
      "M-n" #'dired-next-subdir
      ;; `f' 进入目录或文件
      ;; `b' 返回上级目录
      "b" #'dired-up-directory
      "e" #'dired:find-file-externally
      ;; 将 `%' prefix 转化为 `.'
      "." #'dired-mode-regexp-map
      )
    )
  )

(with-eval-after-load 'dired-x
  ;; 忽略部分文件
  (setq dired-omit-files
        (rx bos (or (seq "desktop.ini")
                    (seq "~$" (* (or alnum (category chinese-two-byte))))
                    eos))
        )
  )

(with-eval-after-load 'dired-aux
  ;; 使用 `X' 或 `!' 运行默认程序
  (setq dired-guess-shell-alist-user
        `((,(rx bos
                (or (seq (+ num) "报销")
                    "STORE")
                eos)
           "elvish tar.elv")
          ("\\.elv\\'" "elvish")
          ("\\.pdf\\'" "mupdf &")
          (,(rx bos ?.
                (or (seq "do" (or ?c ?t) (? ?x))
                    "ppt")
                eos)
           "wps &")))

  (add-to-list 'dired-compress-file-suffixes
               (list
                (rx ?. (or "zip" "7z") eos)
                ""
                "7z x -aoa -o%o %i")))

(provide 'init-dired)
;;; init-dired.el ends here
