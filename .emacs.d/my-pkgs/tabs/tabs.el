;;; tabs.el --- Display tabs using the header line

;; Author: Mads Hartmann <mads379@gmail.com>
;; Maintainer: Mads Hartmann <mads379@gmail.com>
;; Created: 10 July 2016
;; Keywords: user-interface

;;; Commentary:
;;
;; This is a tabs manager that window specific.
;;
;; Similar packages
;; --------------
;;
;; elscreen.el
;;     This package (as the name suggests) adds the concept of tabbar
;;     and screens.  A tab thus stores the whole frame configuration.
;;     This is not what I needed as.
;;
;; tabbar.el
;;     With a good amount of configuration it might actually be
;;     possible to get this package to do the same.
;;
;; Implementation
;; --------------
;;
;; Conceptually each window has a potentially empty tab-list.  The
;; header-line is used to display the tabs.  The header-line-format is
;; a buffer-local variable.  There isn't a concept of a window-local
;; variable but it is emulated using indirect buffer.
;;
;; Window-parameters are used keep track of the state of each
;; window.  The following parameters are stored for each window:
;;
;; `tabs-initialized'
;; `tabs-display'
;; `tabs-current-index'
;; `tabs-list'
;;
;; To change the look you should change the
;;
;; `header-line'
;;     As the header-line is used to display the tabs this is the face
;;     you should change in order to set the background color of the
;;     tab bar.
;; `tabs-active-tab'
;;     The face for the tab that is active.
;; `tabs-inactive-tab'
;;     The face for the tabs that are inactive.
;;
;; Discarded ideas
;; ---------------
;;
;; - window-local header-line-format: The header-line-format is a
;;   buffer-local variable, meaning that if I show a buffer in another
;;   window or frame it will also display the tab bar there which is
;;   unfortunate as I'd prefer the tab bar to only be associated with
;;   a given window (and not the buffer).  However, Emacs doens't have
;;   support for window-local variables.  They can be emulated by
;;   using indirect buffers, but then I would need to have two
;;   copies of every buffer in the tab bar -- the base buffer and the
;;   indirect buffer.
;;
;;; Code:
;;
;; - TODO: Hook into the changing of buffer -- it should change the name of the tab
;; - TODO: Hook into creation of buffer.  It should always set the header-mode-line
;; - TODO: Change the tab face if it has git-changes, isn't saved etc.?
;; - TODO: Use a namespace?

;;; To use it

;; (use-package tabs
;;   ;; My own small package for report specifications for one of our
;;   ;; internal analytics systems at issuu
;;   :ensure nil
;;   :load-path "tabs/"
;;   :commands tabs-mode
;;   :diminish tabs-mode
;;   :bind
;;   (("s-T" . tabs-toggle-display)
;;    ("s-t" . tabs-new-tab)
;;    :map tabs-keymap
;;    ("s-}" . tabs-next-tab)
;;    ("s-{" . tabs-previous-tab)
;;    ("s-w" . tabs-close-current-tab))
;;   :config (tabs-mode))


;;; User Customizable Variables:

(defconst tabs-version "2016-07-12")

(defgroup tabs nil
  "WindowTabs -- Emacs window-specific tabs"
  :tag "WindowTabs"
  :group 'environment)

(defface tabs-active-tab
  '((t (:background "black"
        :foreground "Gray50"
        :height 110
        :slant italic
        :box (:line-width 4 :color "black" :style nil))))
  "Face for current tab."
  :group 'tabs)

(defface tabs-inactive-tab
  '((t (:foreground "black"
        :background "grey75"
        :height 110
        :box (:line-width 4 :color "grey75" :style nil))))
  "Face to fontify background of tab line."
  :group 'tabs)

;;; Key bindings:

(defvar tabs-keymap (make-sparse-keymap)
  "Keymap for tabs.")

;;; View:

(defun header-line-format-for-buffer (index buffer)
  "Get the header-line for the the INDEX BUFFER."
  (let ((tab (cond
             ((eq index (tabs-get-current-index)) (header-line-format-active-tab (buffer-name buffer)))
             (t (header-line-format-inactive-tab (buffer-name buffer)))))
        (spacing " "))
    (list tab spacing)))

(defun header-line-format-left-fringe ()
  "Header-line format for the fringe.

The header-line extends over the fringe.  This function creates
the header-line format that can be used to add left-padding in
order to not have tabs in the fringe."
  (concat
   (make-string (car (window-fringes)) 32)
   "  "))

(defun header-line-format-inactive-tab (name)
  "Header-line format for the active tab using NAME."
  (list (propertize name 'face 'tabs-inactive-tab)))

(defun header-line-format-active-tab (name)
  "Header-line format for the inactive tab using NAME."
  (list (propertize name 'face 'tabs-active-tab)))

(defun header-line-format-tabs ()
  "Header-line format for the tabs."
  (cl-mapcar 'header-line-format-for-buffer
          (number-sequence 0 (- (length (tabs-get-tabs)) 1))
          (tabs-get-tabs)))

(defun tabs-render ()
  "Render the UI."
  (setq header-line-format
        (list
         (header-line-format-left-fringe)
         '(:eval (header-line-format-tabs)))))

;;; Convenience

(defun wt/remove-at (index xs)
  "Remove an element at INDEX based from the list XS."
  (if (eq index 0)
      (cdr xs)
    (concatenate 'list (subseq xs 0 index) (subseq xs (+ 1 index)))))

;;; Model:

(defun tabs-initialize-window ()
  "Initialize the state of the tabbar for the current window."
  (if (not (tabs-initializedp))
      (progn
        (set-window-parameter nil :tabs-initialized t)
        (set-window-parameter nil :tabs-display t)
        (set-window-parameter nil :tabs-current-index 0)
        (set-window-parameter nil :tabs-list (list (current-buffer))))))

(defun tabs-initializedp ()
  "True if the state has been initialized for the current window."
  (window-parameter nil :tabs-initialized))

(defun tabs-displayp ()
  "True if the tab bar is displayed."
  (window-parameter nil :tabs-display))

(defun tabs-set-display (bool)
  "Set the winodws tab display property to BOOL."
  (set-window-parameter nil :tabs-display bool))

(defun tabs-set-current-index (index)
  "Set the currently selected tab to be INDEX."
  (if (or (< index (- (length (tabs-get-tabs)) 1))
          (> index 0))
      (progn
        (set-window-parameter nil :tabs-current-index index)
        (window--display-buffer
         (nth index (tabs-get-tabs))
         (get-buffer-window (current-buffer))
         'window))))

(defun tabs-get-current-index ()
  "Return the index of the currently selected tab."
  (window-parameter nil :tabs-current-index))

(defun tabs-get-tabs ()
  "Get the tabs of the current window."
  (window-parameter nil :tabs-list))

(defun tabs-new-tab-with-buffer (buffer)
  "Create a new tab with BUFFER in the current window."
  (set-window-parameter nil :tabs-list (cons buffer (tabs-get-tabs))))

(defun tabs-close-tab (index)
  "Close the tab at the given INDEX."
  (let* ((current-list (tabs-get-tabs))
         (current-index (tabs-get-current-index))
         (new-list (wt/remove-at current-index current-list))
         (new-index (- current-index 1)))
    (progn
      (set-window-parameter nil :tabs-list new-list)
      (set-window-parameter nil :tabs-current-index new-index))))

;;; Interactive functions:
;;  Functions that I expect a bufer to call.

(defun tabs-new-tab ()
  "Create a new tab using the default buffer."
  (interactive)
  (tabs-new-tab-with-buffer (get-buffer-create "*scratch*"))
  (tabs-render))

(defun tabs-close-current-tab ()
  "Close the current tab."
  (interactive)
  (tabs-close-tab (tabs-get-current-index))
  (tabs-render))

(defun tabs-close-tab-for-buffer (&optional buffer)
  "Close the tab associated with the buffer."
  (interactive)
  ;; TODO: Finish this. The buffer being killed is the
  ;; current bufsfer
  (message "not implemented yet")
  (tabs-render))

(defun tabs-next-tab ()
  "Select the next (right) tab in the window."
  (interactive)
  (tabs-set-current-index (+ 1 (tabs-get-current-index)))
  (tabs-render))

(defun tabs-previous-tab ()
  "Select the previous (left) tab in the window."
  (interactive)
  (tabs-set-current-index (- (tabs-get-current-index) 1))
  (tabs-render))

(defun tabs-select-tab-index (index)
  "Select the tab with the specific INDEX.

The index starts at 0"
  (interactive)
  (message "not implemented"))

(defun tabs-toggle-display ()
  "Display tabs bar in current window."
  (interactive)
  (tabs-set-display (not (tabs-displayp)))
  (tabs-render))

(defun tabs-enable ()
  "Show tabs for the current window."
  (interactive)

  (add-hook 'kill-buffer-hook 'tabs-close-tab-for-buffer)

  (tabs-initialize-window)
  (tabs-render))

(defun tabs-disable ()
  "Don't show tabs for the current window."
  (interactive)

  (remove-hook 'kill-buffer-hook 'tabs-close-tab-for-buffer)

  ;; Clean-up. reset all windows?
  (setq header-line-format nil))

;;;###autoload
(define-minor-mode tabs-mode
  "Minor mode for showing tabs." nil " tabs" tabs-keymap
  (if tabs-mode (tabs-enable) (tabs-disable)))

(provide 'tabs)

;;; tabs.el ends here
