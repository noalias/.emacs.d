;;; -*- lexical-binding: t -*-
(require 'init-base)
(require 'thingatpt)

;; 设置当前行高亮
(add-hook 'after-init-hook #'global-hl-line-mode)
;; 设置光标形状
(setq-default cursor-type 'bar)

;;; edit functions
(defface fwar34-hi-orange
  '((((min-colors 88) (background dark))
     (:background "orange1" :foreground "black"))
    (((background dark)) (:background "orange" :foreground "black"))
    (((min-colors 88)) (:background "orange1"))
    (t (:background "orange")))
  "Default face for hi-lock mode.")

(defvar edit:overlay nil)

(defun edit:overlay--set (beg end)
  (if edit:overlay
      (setq edit:overlay (move-overlay edit:overlay beg end))
    (setq edit:overlay (make-overlay beg end)))
  (overlay-put edit:overlay 'face 'fwar34-hi-orange))

(defun edit:overlay--update-from-region ()
  (when (use-region-p)
    (edit:overlay--set (region-beginning)
                       (region-end))))

(defun edit:overlay--get-sides ()
  (when (overlayp edit:overlay)
    (when-let ((beg (overlay-start edit:overlay))
               (end (overlay-end edit:overlay)))
      (cons beg end))))

(defun edit:overlay--cancel ()
  (when (overlayp edit:overlay)
    (delete-overlay edit:overlay)
    (setq edit:overlay nil)))

(defun edit:thing--get (thing)
  (when-let* ((pt (bounds-of-thing-at-point thing))
              (ol (edit:overlay--set (car pt) (cdr pt))))
    (goto-char (car pt))))

;; get thing
(defun edit:word ()
  (interactive)
  (edit:thing--get 'word))

(defun edit:symbol ()
  (interactive)
  (edit:thing--get 'symbol))

(defun edit:sexp ()
  (interactive)
  (edit:thing--get 'sexp))

(defun edit:list ()
  (interactive)
  (edit:thing--get 'list))

(defun edit:defun ()
  (interactive)
  (edit:thing--get 'defun))

(defun edit:line ()
  (interactive)
  (edit:thing--get 'line))

(defun edit:buffer ()
  (interactive)
  (edit:thing--get 'buffer))

;; move
(defun edit:go-to-other-side ()
  (interactive)
  (pcase (edit:overlay--get-sides)
    (`(,beg . ,end)
     (pcase (point)
       ((pred (eq beg)) (goto-char end))
       ((pred (eq end)) (goto-char beg))))))

;; action
(defun edit:copy ()
  (interactive)
  (pcase (edit:overlay--get-sides)
    (`(,beg . ,end)
     (kill-ring-save beg end)))
  (edit:overlay--cancel))

(defun edit:cut ()
  (interactive)
  (pcase (edit:overlay--get-sides)
    (`(,beg . ,end)
     (kill-region beg end)))
  (edit:overlay--cancel))

(defun edit:insert ()
  (interactive)
  (pcase (edit:overlay--get-sides)
    (`(,beg . _)
     (unless (eq (point beg))
       (goto-char beg))))
  (edit:overlay--cancel))

(defun edit:append ()
  (interactive)
  (pcase (edit:overlay--get-sides)
    (`(_ . ,end)
     (unless (eq pt end)
       (goto-char end))))
  (edit:overlay--cancel))

(defun edit:insert-pair (&optional arg)
  (interactive "P")
  (pcase (edit:overlay--get-sides)
    (`(,beg . ,end)
     (goto-char beg)
     (push-mark end nil t)
     ;; `insert-pair' 可以根据前一输入按键，插入匹配的括号
     (insert-pair arg)
     (setq deactivate-mark t)))
  (edit:overlay--cancel))

(defun edit:cancel ()
  (interactive)
  (edit:overlay--cancel))

;;; keys...........
(transient-define-prefix edit:keys ()
  "Self edit mode"
  ["Select"
   :class transient-row
   ("SPC" "Active region" set-mark-command)
   ("w" "Word" edit:word :transient t)
   ("s" "Symbol" edit:symbol :transient t)
   ("e" "Sexp" edit:sexp :transient t)
   ("o" "List" edit:list :transient t)
   ("f" "Defun" edit:defun :transient t)
   ("l" "Line" edit:line :transient t)
   ("b" "Buffer" edit:buffer :transient t)
   ]
  ["Move"
   :class transient-row
   ("g" "Go to line" consult-goto-line)
   ("/" "Go to other side" edit:go-to-other-side :transient t)
   ]
  ["Action"
   :class transient-row
   ("q" "Cancel" edit:cancel)
   ("y" "Copy" edit:copy)
   ("c" "Change and copy" edit:cut)
   ("i" "Insert" edit:insert)
   ("a" "Append" edit:append)
   ("(" "Insert (" edit:insert-pair)
   ("{" "Insert {" edit:insert-pair)
   ("[" "Insert [" edit:insert-pair)
   ("<" "Insert <" edit:insert-pair)
   ("\"" "Insert \"" edit:insert-pair)
   ("\'" "Insert \'" edit:insert-pair)
   ]
  )

(provide 'init-edit)
;;; init-edit.el ends here
