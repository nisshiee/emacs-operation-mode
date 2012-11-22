;;; -*-Emacs-Lisp-*-
;;; operation-mode.el - Major mode for server operations


;; -------------------- faces --------------------
(make-face 'operation-mode-copied-face)
(set-face-background 'operation-mode-copied-face "firebrick4")
(make-face 'operation-mode-log-face)
(set-face-foreground 'operation-mode-log-face "green3")


;; -------------------- key-map --------------------
(defvar operation-mode-map nil)
(if (not operation-mode-map)
    (progn
      (setq operation-mode-map (make-keymap))
      (define-key operation-mode-map (kbd "M-c") 'operation-mode-copy-cmd)
      (define-key operation-mode-map (kbd "M-v") 'operation-mode-copy-cmd)
      (define-key operation-mode-map (kbd "M-l") 'operation-mode-insert-log)))


;; -------------------- syntax table --------------------
(defvar operation-mode-syntax-table
  (copy-syntax-table shell-mode-syntax-table))

(modify-syntax-entry ?- "w" operation-mode-syntax-table)
(modify-syntax-entry ?# "<" operation-mode-syntax-table)
(modify-syntax-entry ?\n ">" operation-mode-syntax-table)


;; -------------------- font lock --------------------
(defvar operation-mode-keyword-regexp
  (regexp-opt '("cp"
                "rm"
                "ls"
                "ssh"
                "sudo"
                "su"
                "tcsh"
                "scp"
                "touch"
                "date"
                "rm"
                "exit"
                "diff"
                "setenv"
                "export"
                "set"
                "for"
                "foreach"
                "end"
                "do"
                "done"
                "if"
                "then"
                "fi"
                "tail"
                "grep"
                "zgrep"
                "zcat"
                "more"
                "zmore"
                "less"
                "cat"
                "ps"
                "sh"
                "kill"
                "perl"
                "java"
                "vi"
                "mount")
              'words))
(font-lock-add-keywords 'operation-mode
                        `(
                          (,operation-mode-keyword-regexp . 0)))
(setq font-lock-syntax-table operation-mode-syntax-table)

;; -------------------- mode main --------------------
(defun operation-mode ()
  "major mode for server operations"
  (interactive)
  (kill-all-local-variables)

  (setq major-mode                      'operation-mode
        mode-name                       "Operation"
        comment-start                   "#"
        comment-end                     "")
  (use-local-map operation-mode-map)
  (set-syntax-table operation-mode-syntax-table)
  (font-lock-fontify-buffer))


;; -------------------- interactive functions --------------------
(defun operation-mode-copy-cmd ()
  "copy operation command"
  (interactive)
  (let ((beg (point-at-bol ())) (end (point-at-eol ())))
    (kill-ring-save beg end)
    (overlay-put
     (make-overlay beg end (current-buffer) t nil)
     'face 'operation-mode-copied-face))
  (next-line))

(defun operation-mode-insert-log ()
  "insert space for opeartion log"
  (interactive)
  (let ((beg (point)))
    (insert "--log--\n\n--log--\n")
    (overlay-put
     (make-overlay beg (- (point) 1) (current-buffer) t nil)
     'face 'operation-mode-log-face)))


;; -------------------- provides --------------------
(provide 'operation-mode)
