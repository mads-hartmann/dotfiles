;;; log-mode.el --- Major mode for browsing log file

;; Author: Mads Hartmann Jensen <mads379@gmail.com>
;; Keywords: Log browsing


;;; Commentary:

;; A very simple mode that just provide a nice wrapper for some custom
;; key-bindings and a set of configurations that are nice when reading
;; .log files.
;;
;; (load-file "~/.emacs.d/log-mode.el")
;; (require 'log-mode)
;; (add-to-list 'auto-mode-alist '("\\.log$" . log-mode))

;;; Code:

(defvar log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'beginning-of-buffer)
    (define-key map (kbd "G") 'end-of-buffer)
    (define-key map (kbd "j") 'scroll-up-line)
    (define-key map (kbd "k") 'scroll-down-line)
    (define-key map (kbd "f") 'scroll-up-command)
    map)
  "Keymap for `log-mode'.")

(defvar log-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    ;; We add `p' so that M-c on 'hello' leads to 'Hello' rather than 'hello'.
    (modify-syntax-entry ?' "w p" st)
    st)
  "Syntax table used while in `log-mode'.")

;;;###autoload
(define-derived-mode log-mode nil "Log-mode"
  "A major mode for browsing log files."
  (setq-local font-lock-defaults nil)
  (read-only-mode)          ; You usually don't want to edit the logs.
  (setq auto-revert-interval 1) ; Being 5 seconds behind is annoying (5s is the default)
  (auto-revert-tail-mode)       ; tail -f like behavior
  (highlight-lines-matching-regexp "ERROR" 'hi-red-b)) ; Highlight errors.

(provide 'log-mode)
;;; log-mode.el ends here
