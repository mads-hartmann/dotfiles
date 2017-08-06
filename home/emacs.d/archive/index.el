;; TODO notes taking from when I was working on this
;; (I now use projectile instead.)
;;
;; - It would be nice to make it into a proper elpa plugin with it's
;;   own minor-mode and key-bindings that are only relevent when the
;;   mode is enabled.
;; - If the current file isn't in the tags-table, and a the file is
;;   inside a project, and a TAGS file exist, then ask to focus that
;;   project instead.
;; - List buffers for files in the project
;; - Put more meta-data in the .index file
;; - Stuff that we want to ignore but isn't in the .gitignore file
;; - ctags indexing definitions, i.e. if I want to avoid indexing imports.
;; - Create a proper project for it, with readme etc. detailing how to
;;   put stuff in your ctags file and everything. A really helpfull
;;   one.
;;

;;
;; I find (c/e)tags to be really useful when navigating projects of
;; almost any size. This file contains a few functions that improve
;; the way I prefer use TAGS.
;;
;;   * Jump to any definition in the project (using ido)
;;   * Jump to any definition in the currently opened buffer (using ido)
;;   * Open any file in the current project (using ido)
;;   * Complete symbol at point (code-completion) (using ido)
;;
;; Most of the above listed functions are supported by etags out of
;; the box, but the functions below uses some conventions that makes
;; using tags so much more enjoyable (IMO).
;;
;;   * A project is defined by having a .gitignore file
;;   * When indexing your project it will skip any folders that are
;;     excluded in your .gitignore file.
;;   * The entire index is re-built whenever you save a file. This
;;     might not be optimal if you work on large projects but
;;     from my experience it's very convenient.
;;
;; This configuration works well for me.
;;
;; /Mads Hartmann
;;

(require 'etags)

(defconst ctags-path "/usr/local/Cellar/ctags/5.8/bin/ctags")
(defconst project-root-file-indicator ".index")

(defconst no-project-root-err-msg
  (concat "Couldn't find a "
          project-root-file-indicator
          " file"))

(defun find-project-root ()
  (interactive)
  (let ((dir (upward-find-file project-root-file-indicator)))
    (if dir
        dir
      nil)))

(defun index-process-file-cmd-str (dir ignore)
  "Shell command used to index a single file"
  (format "cd %s && %s -a -e -R %s . TAGS 2>/dev/null"
          dir
          ctags-path
          ignore))

(defun create-index-cmd-str (dir ignore)
  "Shell command used to generate the TAGS file"
  (format "cd %s && %s -e -R %s . TAGS 2>/dev/null"
          dir
          ctags-path
          ignore))

(defun forget-current-tags-table ()
  "Forget everything we know about the current tags-table."
  (if (get-buffer "TAGS")
      (kill-buffer "TAGS"))
  (tags-reset-tags-tables)
  (setq tags-completion-table nil))

(defun focus-project-containing-file (&optional path)
  "Resets the currently active tags-table and visits the TAGS
   file closest (upwards) to 'path'. If no value for 'path' is
   supplied it will start the search at the directory containing
   the currently opened file."
  (interactive)
  (let ((dir (find-project-root)))
    (if dir
        (progn
          (forget-current-tags-table)
          (visit-tags-table dir))
      (message no-project-root-err-msg))))

(defun index-current-file ()
  (interactive)
  (let ((dir (find-project-root)))
    (if dir
        (let* ((ignored (read-lines (concat dir "/.gitignore")))
               (ignored-args-str (mapconcat (lambda (i) (concat "--exclude=" i)) ignored " "))
               (index-cmd (index-process-file-cmd-str dir ignored-args-str)))
          (forget-current-tags-table)
          (shell-command index-cmd)
          (focus-project-containing-file dir)
          (message "Done indexing file"))
      (message no-project-root-err-msg))))

(defun index-current-project ()
  "Creates a TAGS file for the project that contains the
   currently opened file.

   It will clear the current tags-table and load the newly
   generated TAGS file."
  (interactive)
  (let ((dir (find-project-root)))
    (if dir
        (let* ((ignored (read-lines (concat dir "/.gitignore")))
               (ignored-args-str (mapconcat (lambda (i) (concat "--exclude=" i)) ignored " "))
               (index-cmd (create-index-cmd-str dir ignored-args-str)))
          (forget-current-tags-table)
          (shell-command index-cmd)
          (focus-project-containing-file dir)
          (message "Done indexing project"))
      (message no-project-root-err-msg))))

(defun files-in-tags-table ()
  (save-excursion
    (visit-tags-table-buffer)
    (mapcar (lambda (x) (expand-file-name (prin1-to-string x t))) (tags-table-files))))

;;
;; Hooks
;;

(defun index-project-if-applicable ()
  (if (find-project-root)
      (index-current-project)))

(defun index-file-if-applicable ()
  (if (find-project-root)
      (index-current-file)))

;; (add-hook 'after-save-hook 'index-file-if-applicable)
;; (remove-hook 'after-save-hook 'index-file-if-applicable)
(add-hook 'after-save-hook 'index-project-if-applicable)


;;
;; Navigation
;;

(defun ido-find-file-in-tag-files ()
  "Find a file listed in the current tag file. From Stuart
   Halloway's 'What You Can Learn From ido.el' screencast."
  (interactive)
  (let ((file-names (files-in-tags-table)))
    (find-file
      (ido-completing-read "File: " file-names nil t))))

(defun ido-find-tag ()
  "Jump to any tag in the project using ido."
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (find-tag (ido-completing-read "Tag in project: " tag-names))))

(defun ido-find-tag-in-file ()
  "Jump to any tag in the currently active file."
  (interactive)
  (let ((full-file-path (buffer-file-name))
        (file-name (buffer-name)))
    (if (member full-file-path (files-in-tags-table))
        (let* ((symbols-hash (symbol-in-file-completion-list file-name))
              (symbol-names (keys symbols-hash)))
          (if symbol-names
              (let ((selected (ido-completing-read "Tag in file: " symbol-names nil t)))
                (goto-char (+ 1 (string-to-number (gethash selected symbols-hash)))))
            (message "No symbols in current file, sorry.")))
      (message "File '%s' is not part of the index. Use M-x focus-project-containing-file." file-name))))

(defun symbol-in-file-completion-list (file-name)
  "Generates a hash-map mapping available symbols in the
   currently active file based on the associated tags-table
   to the offset of those definitions."
  (save-excursion
    (let* ((enable-recursive-minibuffers t)
           (symbol-names (make-hash-table :test 'equal)))
      (visit-tags-table-buffer)
      (goto-char (point-min))
      (let* ((beginning (search-forward file-name nil t))
             (end (re-search-forward "" nil t)))
        (if beginning
            (progn
              (goto-char beginning)
              (while (and (re-search-forward "\\(.*\\).+,\\(.+\\)" nil t)
                          (<= (point) end))
                (let ((symbol-name (buffer-substring (match-beginning 1) (match-end 1)))
                      (symbol-char-offset (buffer-substring (match-beginning 2) (match-end 2))))
                  (puthash symbol-name symbol-char-offset symbol-names)))
              symbol-names)
          symbol-names)))))

(defun ido-complete-symbol-at-point ()
  "Complete symbol at point based on entries in the tags table."
  (interactive)
  (tags-completion-table)
  (let* ((word (thing-at-point 'symbol t))
         (boundaries (bounds-of-thing-at-point 'symbol))
         (start (car boundaries) )
         (end (cdr boundaries) )
         (tag-names))
    (mapatoms (lambda (x)
                (if (string/starts-with (prin1-to-string x t) word)
                    (push (prin1-to-string x t) tag-names)))
              tags-completion-table)
    (let ((selection (ido-completing-read "Completion: " tag-names)))
      (if selection
          (progn
            (delete-region start end)
            (insert selection))))))

(defun keys (hashtable)
  "Return all keys in hashtable."
  (let (allkeys)
    (maphash (lambda (kk vv) (setq allkeys (cons kk allkeys))) hashtable)
    allkeys))
