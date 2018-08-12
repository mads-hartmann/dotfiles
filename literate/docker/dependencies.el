;; Configure the package manager.
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-refresh-contents)
(package-initialize)

;; Install extra org-mode things (TODO: Do I need these)
;; TODO: Specify the EXACT version fo these packages?
(package-install 'org-plus-contrib)
(package-install 'htmlize) ;; Used for syntax highlighting.
