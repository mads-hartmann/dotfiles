(require 'org)
(require 'ox-publish)

(setq dotfiles-dir "/home/babel/dotfiles")
(setq website-output (concat dotfiles-dir "/literate/output/website"))
(setq tagle-output (concat dotfiles-dir "/literate/output/tagles"))

; I had an idea of doing the TOC for the given page here.
; But I don't think you have access to the file that being processed.
(defun dotfiles-postmble ()
  "")

; I had an idea of doing the full sitemap here
; That's still doable. Another time maybe.
(defun dotfiles-preamble ()
  "")

(defun format-folder-name (name)
  "Capitalize and remove hyphen"
  (capitalize (replace-regexp-in-string "-" " " name)))

;; Due to some funky caching issues I've decided to ignore the
;; TITLE property.
(defun format-file-name (entry project)
  "Find the title of FILE in PROJECT."
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
  (t entry)
))

(setq org-publish-project-alist
      `(
        ("everything" :components ("org" "static"))
        ("org"
         :base-directory ,(concat dotfiles-dir "/literate/org")
         :base-extension "org"
         :publishing-directory ,website-output
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 3
         :html-extension "html"
         :body-only nil
         :auto-preamble nil
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         :sitemap-title ""
         :sitemap-format-entry custom-sitemap-entry
         :html-postamble ,(dotfiles-postmble)
         :html-preamble ,(dotfiles-preamble)
         :html-head-include-default-style nil   ; Disable the default css style
         :html-head-include-scripts nil         ; Disable the default javascript snippet
         :html-link-home "index.html"           ; Just the default for this project.
         :html-link-up "../index.html"          ; Just the default for this project.
         )
        ("static"
         :base-directory ,(concat dotfiles-dir "/literate/static")
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,website-output
         :publishing-function org-publish-attachment
         :recursive t)
        ("tangles"
         :base-directory ,(concat dotfiles-dir "/literate/org")
         :base-extension "org"
         :recursive t
         :publishing-directory ,tagle-output
         :publishing-function org-babel-tangle-publish)
         ))
