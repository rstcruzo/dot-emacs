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
    map)
  "Keymap for startup mode.")

(define-derived-mode startup-mode special-mode "Startup"
  "Startup major mode for startup screen.
\\<startup-mode-map>
"
  (run-hooks 'startup-mode-hook)
  (startup-insert-items))

(defcustom startup-buffer-name "*startup*"
  "Startup buffer name.")

(defcustom startup-list
  '((:name "Projects" :text "Switch project" :icon "developer_board" :shortcut "p"
           :action counsel-projectile-switch-project)
    (:name "Bookmarks" :text "Jump to bookmark" :icon "bookmark" :shortcut "b"
           :action counsel-bookmark)
    (:name "Agenda" :text "Open org-agenda" :icon "view_agenda" :shortcut "a"
           :action org-agenda)
    (:name "Recents" :text "Recently opened files" :icon "insert_drive_file" :shortcut "r"
           :action counsel-recentf)
    (:name "Config" :text "Open emacs configuration" :icon "settings" :shortcut "d"
           :action (lambda () (interactive) (find-file "~/.emacs.d/config.org")))
    (:name "Org" :text "Open org files" :icon "note" :shortcut "o"
           :action (lambda () (interactive) (counsel-find-file "~/Dropbox/org"))))
  "Items that will be displayed in startup.")

(defcustom startup-banner-files
  '("~/.emacs.d/banner-0.txt"
    "~/.emacs.d/banner-1.txt"
    "~/.emacs.d/banner-2.txt")
  "Files containing the banner text. One of them will be picked randomly.")

(defun startup-center ()
  (let ((fill-column (window-width))
        (buffer-read-only nil))
    (align-regexp (mark) (point) "\\(\\s-*\\)(" nil 12)
    (center-region 1 (point-max))))

(defun startup-insert-banner ()
  (let* ((total (length startup-banner-files))
         (random-index (random total))
         (random-filename (nth random-index startup-banner-files)))

    (insert-file-contents random-filename)
    (goto-char (point-max))))

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

(defun startup-insert-top-margin ()
  (insert "\n\n\n\n\n\n\n"))

(defun startup-insert-welcome-text ()
  (insert (propertize "Welcome rstcruzo!" 'face 'startup-dim)))

(defun startup-insert-items ()
  "Insert the list of items into the buffer."
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (startup-insert-top-margin)
    (startup-insert-banner)
    (startup-insert-separator)
    (startup-insert-welcome-text)
    (startup-insert-separator)
    (set-mark (point))
    (mapc (lambda (el)
            (startup-insert-item el)
            (startup-insert-separator))
          startup-list)
    (startup-center)
    (deactivate-mark)
    (goto-char 0)))

(evil-make-intercept-map startup-mode-map 'normal)

(defun startup-resize-on-hook (&rest _)
  (let ((startup-window (get-buffer-window startup-buffer-name))
        (selected-window (frame-selected-window)))
    (when (and startup-window
               (not (window-minibuffer-p selected-window)))
      (with-selected-window startup-window
        (startup-center)))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-size-change-functions 'startup-resize-on-hook)
            (startup-resize-on-hook)))

(defun startup-setup-hook ()
  (add-hook 'after-init-hook (lambda ()
                               (switch-to-buffer startup-buffer-name)
                               (startup-mode)
                               (startup-insert-items))))

(provide 'startup)
