;; See this commit 
;; https://github.com/mads-hartmann/mads-hartmann.github.com/commit/cd02588db1480542e3a5a01b5896905d9ad14a31

(require 'org)

(setq org-publish-project-alist
      '(
        ("everything" :components ("org-notes" "org-static"))
        ("org"
         :base-directory "~/dev/dotfiles/literate/org"
         :base-extension "org"
         :publishing-directory "~/dev/dotfiles/literate/output/website"
         :publishing-function org-html-publish-to-html
         :recursive t
         :headline-levels 4
         :html-extension "html"
         :body-only nil
         :auto-preamble nil
         :auto-sitemap t
         :sitemap-filename "index.html"
         )
        ("static"
         :base-directory "~/dev/dotfiles/literate/static"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "~/dev/dotfiles/literate/output/website"
         :publishing-function org-publish-attachment
         :recursive t)))

