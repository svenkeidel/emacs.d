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

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

;;; Set up package
(load-file (format "%s/.nix-profile/share/emacs/site-lisp/site-start.el" (getenv "HOME")))
(require 'package)
(package-initialize)
(require 'use-package)
(require 'req-package)

(org-babel-load-file (concat user-emacs-directory "config.org"))
(req-package-finish)

(setq custom-file (format "%s/.emacs.d/custom.el" (getenv "HOME")))
(load custom-file)
