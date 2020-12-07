(require 'all-the-icons)

(defgroup startup nil
  "Extensible startup screen.")

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
    map)
  "Keymap for startup mode.")

(defvar startup-buffer-last-width nil
  "Previous width of startup buffer")

(define-derived-mode startup-mode fundamental-mode "Startup"
  "Startup major mode for startup screen.
\\<startup-mode-map>
"
  (run-hooks 'startup-mode-hook)
  (startup-insert-items))

(defcustom startup-buffer-name "*startup*"
  "Startup buffer name.")

(defcustom startup-list
  '((:name "Recents" :text "Recently opened files" :icon "insert_drive_file" :shortcut "r"
           :action counsel-recentf)
    (:name "Projects" :text "Switch project" :icon "developer_board" :shortcut "p"
           :action counsel-projectile-switch-project)
    (:name "New Workspace" :text "Open new workspace"
           :icon "work" :shortcut "w"
           :action eyebrowse-create-named-window-config)
    (:name "Bookmarks" :text "Jump to bookmark" :icon "bookmark" :shortcut "b"
           :action counsel-bookmark)
    (:name "Agenda" :text "Open org-agenda" :icon "view_agenda" :shortcut "a"
           :action org-agenda)
    (:name "Org" :text "Open org files" :icon "note" :shortcut "o"
           :action (lambda () (interactive) (counsel-find-file "~/Dropbox/org")))
    (:name "Config" :text "Open emacs configuration" :icon "settings" :shortcut "e"
           :action (lambda () (interactive) (find-file "~/.emacs.d/config.org")))
    (:name "Config Workspace" :text "Open configuration workspace"
           :icon "settings_applications" :shortcut "E"
           :action rst/eyebrowse-create-config-window-config))
  "Items that will be displayed in startup.")

(defun startup-align (beg end)
  (align-regexp beg end "\\(\\s-*\\)(" nil 12))

(defun startup-center (beg end)
  (let ((fill-column (window-width))
        (buffer-read-only nil))
    (center-region beg end)))

(defun startup-insert-banner ()
  (let* ((image (create-image "~/.emacs.d/banner.png"))
         (width (car (image-size image)))
         (left-margin (floor (- (/ (window-width) 2) (/ width 2)))))
    (insert (make-string left-margin ?\ ))
    (insert-image image)))

(defun startup-insert-item (item)
  (destructuring-bind (&key name text icon shortcut action) item
    (insert (all-the-icons-material icon :face 'startup-item :height 1))
    (insert (propertize (concat "  " text) 'face 'startup-item))
    (startup-insert-shortcut shortcut)

    (startup-define-shortcut shortcut action)))

(defun startup-insert-shortcut (shortcut)
  (insert (propertize (concat "(" shortcut ")") 'face 'startup-dim)))

(defun startup-define-shortcut (shortcut action)
  (define-key startup-mode-map (kbd shortcut) action))

(defun startup-insert-separator ()
  (insert "\n\n"))

(defun startup-insert-welcome-text ()
  (insert (propertize "Welcome rstcruzo!" 'face 'startup-dim)))

(defun startup-insert-navigator ()
  (widget-create 'url-link
                 :tag (string-join
                       `(,(all-the-icons-faicon "github" :v-adjust 0.03)
                         "Github") " ")
                 :mouse-face 'highlight
                 :format "%[%t%]"
                 "https://github.com/rstcruzo/dot-emacs"))

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
            (startup-insert-welcome-text)
            (startup-insert-separator)

            (startup-center beg (point)))

          (let ((beg (point)))
            (mapc (lambda (el)
                    (startup-insert-item el)
                    (startup-insert-separator))
                  startup-list)

            (startup-align beg (point))
            (startup-center beg (point))))))))

(evil-make-intercept-map startup-mode-map 'normal)

(defun startup-resize-on-hook (&rest _)
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
  (add-hook 'after-init-hook (lambda ()
                               (switch-to-buffer startup-buffer-name)
                               (startup-mode)
                               (startup-insert-items)
                               (redisplay))))

(provide 'startup)
