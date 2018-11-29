;;; hdl-mode.el --- Support for writing HDL files in emacs.


(setq hdl-mode-keywords
      '("CHIP" "IN" "OUT" "PARTS"))

(setq hdl-mode-functions
      '())

(setq hdl-mode-constants
      '("false" "true"))

;; Create word-bound regexps out of the string keywords
(setq hdl-mode-keywords-regexp (regexp-opt hdl-mode-keywords 'words))
(setq hdl-mode-functions-regexp (regexp-opt hdl-mode-functions 'words))
(setq hdl-mode-constants-regexp (regexp-opt hdl-mode-constants 'words))

(setq hdl-mode-font-lock-keywords
  `(
    (,hdl-mode-keywords-regexp . font-lock-keyword-face)
    (,hdl-mode-functions-regexp . font-lock-function-name-face)
    (,hdl-mode-constants-regexp . font-lock-constant-face)
))

;; command to comment/uncomment text
(defun hdl-mode-syntax-table-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "/*") (comment-end "*/"))
    (comment-dwim arg)))

;; syntax table
(defvar hdl-mode-syntax-table nil "Syntax table for `hdl-mode'.")
(setq hdl-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        ;; // style comments
        (modify-syntax-entry ?\/ ". 12b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        ;; /* */ style comments
        (modify-syntax-entry ?\/ ". 14" syn-table)
        (modify-syntax-entry ?* ". 23" syn-table)
        syn-table))

;;;###autoload
(define-derived-mode hdl-mode fundamental-mode "hdl-mode"
  "A major mode for writing internal analytics report
specifications"
  :syntax-table hdl-mode-syntax-table
  (setq font-lock-defaults '((hdl-mode-font-lock-keywords)))
  ;; modify the keymap
  (define-key hdl-mode-map [remap comment-dwim] 'hdl-mode-syntax-table-comment-dwim))

(provide 'hdl-mode)

;;; hdl-mode.el ends here


;;;###autoload
(define-derived-mode hdl-mode fundamental-mode "hdl-mode"
  "A major mode for writing internal analytics report
specifications"
  :syntax-table hdl-mode-syntax-table
  (setq font-lock-defaults '((hdl-mode-font-lock-keywords)))
  ;; modify the keymap
  (define-key hdl-mode-map [remap comment-dwim] 'hdl-mode-syntax-table-comment-dwim))

(provide 'hdl-mode)

;;; hdl-mode.el ends here
