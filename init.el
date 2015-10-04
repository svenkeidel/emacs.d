(require 'package)

(setq package-list
      '(spacemacs-theme
        helm
        smartparens
        magit
        buffer-move
        rainbow-delimiters
        haskell-mode
        company
        w3m))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; custom functions
(defun insert-line-above (times)
  "Insert a newline above the line containing the cursor."
  (interactive "p")
  (save-excursion
    (move-beginning-of-line 1)
    (newline times)))

(defun join-line-below (times)
  "joins the next line into the line containing the cursor."
  (interactive "p")
  (save-excursion
    (next-line times)
    (dotimes (x times)
      (join-line))))

(defun open-emacs-init-el ()
  "open the .emacs.d/init.el configuration file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Keybindings
(global-set-key (kbd "C-c i") 'open-emacs-init-el)
(global-set-key (kbd "<f5>") 'recompile)

(setq browse-url-browser-function 'w3m-browse-url)

;;;;;;;;;;;;;;;;;;;;;;;;;; PLUGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme
(load-theme 'spacemacs-dark t)
(set-face-attribute 'default nil :height 120)

;; disable gui
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq inhibit-startup-message t)

(require 'helm)
(require 'helm-config)
(setq helm-move-to-line-cycle-in-source t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "C-x b") 'helm-mini)
(helm-mode 1)

(require 'smartparens)
(require 'smartparens-config)
(sp-use-smartparens-bindings)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(add-hook 'emacs-lisp-mode-hook
          'smartparens-strict-mode)

(require 'magit)
(global-unset-key (kbd "C-x s"))
(global-set-key (kbd "C-x s") 'magit-status)

(winner-mode 1)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>") 'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>") 'buf-move-right)

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode 1)
(setq-default indent-tabs-mode nil)

(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook
          'rainbow-delimiters-mode)

(require 'speedbar)
(speedbar-add-supported-extension ".hs")

(require 'nixos)
(setq nixos-nixpkgs-path nil)
;; (setq nixos-nixpkgs-path "/home/sven/.nix-defexpr/channels/unstable/")

(require 'flycheck "~/flycheck/flycheck.el")
(setq flycheck-command-wrapper-function
        (lambda (cmd args) (apply 'nix-shell-command (nixos-current-sandbox) cmd args))
      flycheck-executable-find
        (lambda (cmd) (nixos-executable-find (nixos-current-sandbox) cmd)))

(require 'haskell-mode)
(setq haskell-process-type 'cabal-repl
      haskell-tags-on-save t
      haskell-process-wrapper-function
        '(lambda (args) (apply 'nix-shell-command (nixos-current-sandbox) args)))
(add-hook 'haskell-mode-hook 'flycheck-mode)
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

(add-to-list 'load-path "/home/sven/structured-haskell-mode/elisp/")
(require 'shm)
(setq shm-program-name "/home/sven/structured-haskell-mode/.cabal-sandbox/bin/structured-haskell-mode")
(add-hook 'haskell-mode 'structured-haskell-mode)

(require 'shm-case-split)
(define-key shm-map (kbd "C-c C-s") 'shm/case-split)
