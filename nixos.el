;;; nixos.el --- Utility functions to talk with nix-shell sandboxes

;; Author: Sven Keidel <svenkeidel@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((dash "2.11.0") (helm "1.7.9"))
;; URL: http://github.com/svenkeidel/nixos.el

;;; Code:

(provide 'nixos)
(require 'dash)
(require 'helm)

(defun nix-shell-command (sandbox &rest args)
  "Assembles a nix-shell command that gets executed in the specified sandbox."
  (list
   "nix-shell"
   "-I"
   "nixpkgs=/home/sven/.nix-defexpr/channels/unstable/nixpkgs/"
   "--run"
   (mapconcat 'identity args " ")
   sandbox))

(defun nix-shell (sandbox &rest args)
  "Runs a nix-shell command in the given sandbox and returns its output."
  (let* ((cmd (apply 'nix-shell-command sandbox args))
	 (cmd-string (mapconcat (lambda (x) (concat "'" x "'")) cmd " ")))
    (shell-command-to-string cmd-string)))

(defvar nixos-exec-path-map (make-hash-table :test 'equal
					     :size 10))

(defun nixos-exec-path (sandbox)
  "Returns the `exec-path' of the given sandbox."
  (if (gethash sandbox nixos-exec-path-map)
      (gethash sandbox nixos-exec-path-map)
    (puthash sandbox
	     (split-string (nix-shell sandbox "echo" "$PATH") ":")
	     nixos-exec-path-map)))

(defun nixos-executable-find (sandbox exe)
  "Searches for an executable in the given sandbox"
  (let ((exec-path (nixos-exec-path sandbox)))
    (and exec-path (executable-find exe))))

(defun nixos-find-sandbox (path)
  "Searches for a NixOS sandbox starting at the given path,
looking upwards."
  (map-nil 'expand-file-name
   (locate-dominating-file path
			   '(lambda (dir) (directory-files dir t ".*\.nix$")))))

(defun map-nil (f x)
  (if x
      (funcall f x)
    nil))

(defun nixos-current-sandbox ()
  "Returns the path of the NixOS sandbox that is closest
to the current working directory."
  (nixos-find-sandbox default-directory))

(defvar nixos-haskell-doc-path-map (make-hash-table :test 'equal
                                                :size 10))

(defun nixos-haskell-doc-path (sandbox)
  "Returns the `exec-path' of the given sandbox."
  (if (gethash sandbox nixos-haskell-doc-path-map)
      (gethash sandbox nixos-haskell-doc-path-map)
    (puthash sandbox
             (nix-shell sandbox "echo" "-n" "$NIX_GHC_LIBDOCDIR")
	     nixos-haskell-doc-path-map)))

(defun helm-source-haskell-doc-library (sandbox)
  (helm-build-sync-source "haskell-doc-library"
    :candidates (cddr (directory-files (nixos-haskell-doc-path sandbox)))))

(defun nixos-convert-module-file-name (module-file)
  `( ,(mapconcat 'identity (split-string module-file "-" nil "\.html") ".")
   . ,module-file))

(defun nixos-haskell-doc-filter (file)
  (not (string-match "index\\|frames\\.html\\|mini_\\|\\.js\\|\\.gif\\|\\.png\\|\\.css\\|\\.txt\\|haddock\\|src" file)))

(defun nixos-haskell-search-library-docs (sandbox)
  (let* ((lib (helm :sources (helm-source-haskell-doc-library sandbox)))
         (lib-doc-dir (concat (nixos-haskell-doc-path sandbox) lib "/html/"))
         (lib-files (cddr (directory-files lib-doc-dir)))
         (module-files (-filter 'nixos-haskell-doc-filter lib-files))
         (modules (-map 'nixos-convert-module-file-name module-files))
         (module (helm :sources (helm-build-sync-source "haskell-library-module"
                                  :candidates modules))))
    (concat lib-doc-dir module)))

(defun nixos-haskell-open-library-docs ()
  (interactive)
  (eww (concat "file://" (nixos-haskell-search-library-docs (nixos-current-sandbox)))))

;;; nixos.el ends here
