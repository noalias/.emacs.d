;;; -*- lexical-binding: t -*-
(require 'init-base)
(require 'thingatpt)

(progn ; `other-config'
  ;; 设置当前行高亮
  (add-hook 'after-init-hook #'global-hl-line-mode)
  ;; 设置光标形状
  (setq-default cursor-type 'bar)
  (add-hook 'after-init-hook #'electric-pair-mode)
  (add-hook 'after-init-hook #'show-paren-mode)
  (setq show-paren-sytle 'parentthesis)
  (setq-default fill-column 80
                tab-width 4
                indent-tabs-mode nil)
  )

;;; Simple edit mode
(defvar-local edit:overlay nil)

(defface edit:overlay-face
  '((t (:background "salmon" :foreground "black")))
  "Thing Overlay face.")

(defun edit:overlay--set (beg end)
  (if (overlayp edit:overlay)
      (setq edit:overlay (move-overlay edit:overlay beg end))
    (setq edit:overlay (make-overlay beg end)))
  (overlay-put edit:overlay 'face 'edit:overlay-face))

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

;; point
(defvar edit:point--stack)

(defun edit:point--push (pt)
  (push edit:point--stack pt))

(defun edit:point--pop ()
  (pop edti:point--stack))

;; things
;; The `thingatpt' contain:
;; `Lines' `Strings' `Sexps' `Symbols' `Lists' `Defuns' `Number'
;; `Filenames' `Faces' `URIs' `email' `UUID'
;; `Buffer' `Region'
(defun edit:thing--get (thing)
  (if-let ((bounds (bounds-of-thing-at-point thing)))
      (edit:overlay--set (car bounds) (cdr bounds))
    (user-error "No %s here" thing)))

; get thing
(defun edit:word ()
  (interactive)
  (edit:thing--get 'word))

(defun edit:symbol ()
  (interactive)
  (edit:thing--get 'symbol))

(defun edit:string ()
  (interactive)
  (edit:thing--get 'string))

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
       (_ (goto-char beg))))))

;; pair
(defun edit:insert-pair (&optional arg)
  (interactive "P")
  (save-excursion
    (pcase (edit:overlay--get-sides)
    (`(,beg . ,end)
     (goto-char beg)
     (push-mark end nil t)
     ;; `insert-pair' 可以根据前一输入按键，插入匹配的括号
     (insert-pair arg)
     (when (use-region-p)
       (edit:overlay--set (1- (region-beginning))
                          (1+ (region-end))))
     (setq deactivate-mark t)))))

(defun edit:delete-pair ()
  (interactive)
  (save-excursion
    (pcase (edit:overlay--get-sides)
      (`(,beg . ,end)
       (when (member (list (get-byte beg) (get-byte (1- end)))
                     insert-pair-alist)
         (goto-char end)
         (delete-char -1)
         (goto-char beg)
         (delete-char 1))))))

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
    (`(,beg . ,_)
     (unless (eq (point) beg)
       (goto-char beg))))
  (edit:overlay--cancel))

(defun edit:append ()
  (interactive)
  (pcase (edit:overlay--get-sides)
    (`(,_ . ,end)
     (unless (eq (point) end)
       (goto-char end))))
  (edit:overlay--cancel))

(defun edit:comment (arg)
  (interactive "*P")
  (save-excursion
    (pcase (edit:overlay--get-sides)
      (`(,beg . ,end)
       (goto-char beg)
       (push-mark end nil t)
       (comment-dwim arg)
       (setq deactivate-mark t))))
  (edit:overlay--cancel))

(defun edit:intent ()
  (interactive)
  (save-excursion
    (pcase (edit:overlay--get-sides)
      (`(,beg . ,end)
       (goto-char beg)
       (push-mark end nil t)
       (when (use-region-p)
         (indent-region beg end))
       (setq deactivate-mark t))))
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
   ("r" "String" edit:string :transient t)
   ("o" "List" edit:list :transient t)
   ("f" "Defun" edit:defun :transient t)
   ("l" "Line" edit:line :transient t)
   ("b" "Buffer" edit:buffer :transient t)
   ]
  ["Move"
   :class transient-row
   ("g" "Go to line" consult-goto-line)
   ("/" "Go to other side" edit:go-to-other-side :transient t)
   ("e" "Expand thing" (lambda () (interactive) (message "Todo!")))
   ("z" "Go back to prefix thing" (lambda () (interactive) (message "Todo!")))
   ]
  ["Pair"
   :class transient-row
   ("(" "Insert (" edit:insert-pair :transient t)
   ("{" "Insert {" edit:insert-pair :transient t)
   ("[" "Insert [" edit:insert-pair :transient t)
   ("<" "Insert <" edit:insert-pair :transient t)
   ("\"" "Insert \"" edit:insert-pair :transient t)
   ("\'" "Insert \'" edit:insert-pair :transient t)
   ("u" "Delete pair" edit:delete-pair :transient t)
   ]
  ["Action"
   :class transient-row
   ("q" "Cancel" edit:cancel)
   ("y" "Copy" edit:copy)
   ("c" "Change and copy" edit:cut)
   ("i" "Insert" edit:insert)
   ("a" "Append" edit:append)
   (";" "Comment" edit:comment)
   ("TAB" "Intent" edit:intent)
   ]
  )

(provide 'init-edit)
;;; init-edit.el ends here
