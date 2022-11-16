;;; -*- lexical-binding: t -*-
(require 'init-base)

(straight-use-package '(auto-save
                        :host github
                        :repo "manateelazycat/auto-save"))
(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)
(setq delete-auto-save-files t)

(add-hook 'after-init-hook #'recentf-mode)
(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude (rx bos ?.
                                    (or (seq "do" (or ?c ?t) (? ?x))
                                        "ppt")
                                    eos))
  )

(straight-use-package 'consult-dir)
(autoload 'consult-dir-dired "consult-dir")

(when base:win-p
  ;; 使用 `everything' 搜索文件
  ;; (modify-coding-system-alist 'process "es" '(gbk . gbk))
  ;; (setq consult-locate-args (encode-coding-string "es.exe -i -p -r" 'gbk))
  ;; 使用 `fd' 搜索文件
  (modify-coding-system-alist 'process "fd" '(utf-8 . gbk))
  (setq consult-locate-args (encode-coding-string "fd -H -a --color=never" 'gbk))
  )

(transient-define-prefix file:keys ()
  "File 操作"
  ["Files Commands"
   ("SPC" "Find file" find-file)
   ("s" "Set bookmark" bookmark-set)
   ("d" "Dired current directory" consult-dir-dired)
   ("j" "Jump to file in current diectory" consult-dir-jump-file)
   ("b" "Open bookmark files" consult-bookmark)
   ("f" "Locate the files" consult-locate)
   ("v" "View the dirs" consult-dir)
   ("e" "Open file with external command" consult-file-externally)
   ("g" "Find file with rg" consult-ripgrep)
   ("r" "Find recent file" consult-recent-file)
   ])

(provide 'init-file)
;;; init-file.el ends here
