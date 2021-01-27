;;; startup.el --- A startup screen

;;; Commentary:

;;; A customizable startup screen.  Highly inspired in dashboard.el
;;; and Doom Emacs dashboard.

(require 'all-the-icons)
(require 'projectile)

;;; Code:

(defgroup startup nil
  "Extensible startup screen."
  :group 'applications)

(defface startup-item
  '((t (:inherit font-lock-keyword-face)))
  "Face used for startup items.")

(defface startup-dim
  '((t (:inherit font-lock-doc-face)))
  "Face used for dim text in startup screen.")

(defvar startup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'widget-forward)
    (define-key map (kbd "<backtab>") #'widget-backward)
    (define-key map (kbd "R") #'startup-reload)

    (define-key map (kbd "j") #'widget-forward)
    (define-key map (kbd "k") #'widget-backward)
    map)
  "Keymap for startup mode.")

(defvar startup-buffer-last-width nil
  "Previous width of startup buffer.")

(define-derived-mode startup-mode special-mode "Startup"
  "Startup major mode for the startup screen.
\\<startup-mode-map>
"
  (run-hooks 'startup-mode-hook)
  (if (and (featurep 'evil)
           (evil-mode))
      (evil-make-intercept-map startup-mode-map 'normal))
  (startup-insert-items))

(defcustom startup-buffer-name "*startup*"
  "Startup buffer name."
  :type 'string)

(defcustom startup-banner-path
  (expand-file-name "banner.png" (file-name-directory (locate-library "startup")))
  "Startup banner path."
  :type 'string)

(defcustom startup-projectile-path nil
  "Projectile path to reload projects from."
  :type 'string)

(defcustom startup-list
  '((:name "Recents" :text "Recently opened files" :icon "insert_drive_file" :shortcut "r"
           :action (lambda (&rest _) (interactive) (counsel-recentf)))
    (:name "Projects" :text "Switch project" :icon "developer_board" :shortcut "p"
           :action (lambda (&rest _) (interactive) (counsel-projectile-switch-project)))
    (:name "Bookmarks" :text "Jump to bookmark" :icon "bookmark" :shortcut "b"
           :action (lambda (&rest _) (interactive) (counsel-bookmark)))
    (:name "Agenda" :text "Open org-agenda" :icon "view_agenda" :shortcut "a"
           :action (lambda (&rest _) (interactive) (org-agenda))))
  "Items that will be displayed in startup.

Currently, only material icons are supported."
  :type 'list)

(defun startup-align (beg end)
  "Align region defined by BEG and END by brackets."
  (align-regexp beg end "\\(\\s-*\\)(" nil 12))

(defun startup-center (beg end)
  "Center region defined by BEG and END."
  (let ((fill-column (window-width))
        (buffer-read-only nil))
    (center-region beg end)))

(defun startup-insert-banner ()
  "Insert centered banner image."
  (let* ((image (create-image startup-banner-path))
         (width (car (image-size image)))
         (left-margin (floor (- (/ (window-width) 2) (/ width 2)))))
    (insert (make-string left-margin ?\ ))
    (insert-image image)))

(defun startup-insert-item (item)
  "Insert ITEM.

With its icon as prefix and shortcut as suffix enclosed in brackets.
Only material icons are supported currently."
  (destructuring-bind (&key name text icon shortcut action) item
    (insert (all-the-icons-material icon :face 'startup-item :height 1))
    (insert "  ")
    (widget-create 'link
                   :tag (propertize text 'face 'startup-item)
                   :mouse-face 'highlight
                   :format "%[%t%]"
                   :help-echo nil
                   :action action
                   :button-prefix ""
                   :button-suffix "")
    (startup-insert-shortcut shortcut)
    (startup-define-shortcut shortcut action)))

(defun startup-insert-shortcut (shortcut)
  "Insert SHORTCUT enclosed in brackets."
  (insert (propertize (concat "(" shortcut ")") 'face 'startup-dim)))

(defun startup-define-shortcut (shortcut action)
  "Bind SHORTCUT to ACTION in startup-mode-map."
  (define-key startup-mode-map (kbd shortcut) action))

(defun startup-insert-separator ()
  "Insert separator."
  (insert "\n\n"))

(defun startup-reload (&rest _)
  "Reload startup screen.

Also, reload projectile projects if startup-projectile-path is set."
  (interactive)
  (message "Reloading...")
  (if (not (null startup-projectile-path))
      (projectile-discover-projects-in-directory startup-projectile-path))
  (startup-insert-items)
  (message "done"))

(defun startup-insert-navigator ()
  "Insert navigator.

Consists of a github link to my repo and a refresh button."
  (widget-create 'url-link
                 :tag (string-join
                       `(,(all-the-icons-faicon "github" :v-adjust 0.03)
                         "Github") " ")
                 :button-face 'startup-dim
                 :mouse-face 'highlight
                 :help-echo "Go to github repository"
                 :format "%[%t%]"
                 "https://github.com/rstcruzo/dot-emacs")
  (insert " ")
  (widget-create 'link
                 :tag (string-join
                       `(,(all-the-icons-faicon "refresh" :v-adjust 0.03)
                         "Refresh") " ")
                 :button-face 'startup-dim
                 :mouse-face 'highlight
                 :format "%[%t%]"
                 :help-echo "Refresh startup page"
                 :button-prefix "["
                 :button-suffix "]"
                 :action #'startup-reload))

(defun startup-insert-items ()
  "Insert the list of items into the buffer."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer startup-buffer-name))))
    (when (or (not (eq startup-buffer-last-width (window-width)))
              (not buffer-exists))
      (setq startup-buffer-last-width (window-width))
      (save-excursion
        (let ((buffer-read-only nil))
          (erase-buffer)
          (startup-insert-separator)
          (startup-insert-banner)
          (let ((beg (point)))
            (startup-insert-separator)
            (startup-insert-navigator)
            (startup-insert-separator)
            (startup-insert-separator)

            (startup-center beg (point)))

          (let ((beg (point)))
            (mapc (lambda (el)
                    (startup-insert-item el)
                    (startup-insert-separator))
                  startup-list)

            (startup-align beg (point))
            (startup-center beg (point)))))
      ;; jump to first item - 2 navigator widgets plus 1
      (widget-forward 3))))

(defun startup-resize-on-hook (&rest _)
  "Reinsert items when necessary."
  (let ((startup-window (get-buffer-window startup-buffer-name))
        (selected-window (frame-selected-window)))
    (when (and startup-window
               (not (window-minibuffer-p selected-window)))
      (with-selected-window startup-window
        (startup-insert-items)))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-size-change-functions 'startup-resize-on-hook)
            (startup-resize-on-hook)))

(defun startup-setup-hook ()
  "Setup Startup to appear on Emacs startup."
  (add-hook 'after-init-hook (lambda ()
                               (switch-to-buffer startup-buffer-name)
                               (startup-mode)
                               (startup-insert-items)
                               (redisplay))))

(provide 'startup)
;;; startup.el ends here

