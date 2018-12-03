;;; project.el -- Configuration of Emacs to weave/tangle my dotfiles
;;;
;;; Commentary:
;;; This really isn't so interesting. It confgiures Emacs to run
;;; nicely inside of Docker for weaving and tangling my dotfiles
;;; project into something thats useful.
;;; Code:

(require 'org)
(require 'ox-publish)

;; A few Emacs specific variables.
(setq
 ;; Don't try to save buffers of make backup files. You're running
 ;; inside of Docker without a TTY attached - no-one cares.
 make-backup-files nil
 auto-save-default nil
 org-html-link-org-files-as-html t
)

(defvar publishing-directory (concat default-directory "/.website")
  "The absolute path to where the weaved contents should be published to.")

(defvar base-directory (concat default-directory "/literate/org")
  "The absolute path to where the org-files are stored.")

(defun format-folder-name (name)
  "Capitalize and remove hyphen from NAME."
  (capitalize (replace-regexp-in-string "-" " " name)))

(defun format-file-name (entry project)
  "Find the title of ENTRY in PROJECT.
Due to some funky caching issues I've decided to ignore the TITLE property."
  (let ((file (org-publish--expand-file-name entry project)))
    (file-name-nondirectory (file-name-sans-extension file))))

;; Slightly modified version of `org-publish-sitemap-default-entry`
(defun custom-sitemap-entry (entry style project)
  "Default format for site map ENTRY, as a string.
ENTRY is a file name.  STYLE is the style of the sitemap.
PROJECT is the current project."
  (cond ((not (directory-name-p entry))
   (format "[[file:%s][%s]]"
     entry
     (format-file-name entry project)))
  ((eq style 'tree)
   ;; Return only last subdir.
   (format-folder-name (file-name-nondirectory (directory-file-name entry))))
  (t entry)))

(setq org-publish-project-alist
      `(
        ("weave"
         :base-directory ,base-directory
         :base-extension "org"
         :publishing-directory ,publishing-directory
         :publishing-function org-html-publish-to-html
         :html-extension "html"
         :recursive t
         :headline-levels 3
         :body-only nil
         :html-preamble nil
         :html-postamble nil
         :auto-sitemap nil                      ; I maintain it manually to have full control
         :html-head-include-default-style nil   ; Disable the default css style
         :html-head-include-scripts nil         ; Disable the default javascript snippet
         )
        ("tangle"
         :base-directory ,base-directory
         :base-extension "org"
         :recursive t
         :publishing-directory ""       ; Empty as we're not actually publishing, we're tangling
         :publishing-function org-babel-tangle-publish)))

;;; project.el ends here
