(require 'org)

(setq dotfiles-dir "~/dev/personal/dotfiles")
(setq website-output (concat dotfiles-dir "/literate/output/website"))

(setq org-publish-project-alist
      `(
        ("everything" :components ("org-notes" "org-static"))
        ("org"
         :base-directory ,(concat dotfiles-dir "/literate/org")
         :base-extension "org"
         :publishing-directory ,website-output
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 4
         :html-extension "html"
         :body-only nil
         :auto-preamble nil
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "My Dotfiles"
         )
        ("static"
         :base-directory ,(concat dotfiles-dir "/literate/static")
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory ,website-output
         :publishing-function org-publish-attachment
         :recursive t)))
