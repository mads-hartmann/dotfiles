;;; report-spec-mode.el --- Support for writing issuu internal anyltics report specs

;; To enable this package, use the following snippet.
;; (use-package report-spec-mode
;;   ;; My own small package for report specifications for one of our
;;   ;; internal analytics systems at issuu
;;   :ensure nil
;;   :load-path "my-pkgs/"
;;   :defer)

(setq report-spec-mode-keywords
      '("where" "by" "monthly" "weekly" "daily" "over" "days"))

(setq report-spec-mode-functions
      '("metric"))

;; Create word-bound regexps out of the string keywords
(setq report-spec-mode-keywords-regexp (regexp-opt report-spec-mode-keywords 'words))
(setq report-spec-mode-functions-regexp (regexp-opt report-spec-mode-functions 'words))

(setq report-spec-mode-font-lock-keywords
  `(
    (,report-spec-mode-keywords-regexp . font-lock-keyword-face)
    (,report-spec-mode-functions-regexp . font-lock-function-name-face)
))

;; command to comment/uncomment text
(defun report-spec-mode-syntax-table-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "#") (comment-end ""))
    (comment-dwim arg)))

;; syntax table
(defvar report-spec-mode-syntax-table nil "Syntax table for `report-spec-mode'.")
(setq report-spec-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?# "< b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        syn-table))

;;;###autoload
(define-derived-mode report-spec-mode fundamental-mode "report-spec-mode"
  "A major mode for writing internal analytics report
specifications"
  :syntax-table report-spec-mode-syntax-table
  (setq font-lock-defaults '((report-spec-mode-font-lock-keywords)))
  ;; modify the keymap
  (define-key report-spec-mode-map [remap comment-dwim] 'report-spec-mode-syntax-table-comment-dwim))

(provide 'report-spec-mode)

;;; report-spec-mode.el ends here
