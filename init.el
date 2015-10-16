(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))

;; Deactivate UI before initialization to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode 0))

;; Shutup warning message
(setq ad-redefinition-action 'accept)

(setq inhibit-startup-message nil)
(setq initial-scratch-message nil)

;;; Set up package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(dolist
  (package '(use-package req-package))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(require 'use-package)
(require 'req-package)

(org-babel-load-file (concat user-emacs-directory "config.org"))
(req-package-finish)
