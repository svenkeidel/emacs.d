;;; nixos.el --- Utility functions to talk with nix-shell sandboxes

;; Author: Sven Keidel <svenkeidel@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((dash "2.11.0") (helm "1.7.9"))
;; URL: http://github.com/svenkeidel/nixos.el

;;; Code:

(require 'dash)
(require 'helm)

(defgroup nixos nil
  "Utilities usefull for talking to nix-shell"
  :prefix "nixos-"
  :group nil)

(defcustom nixos-nixpkgs-path nil
  "Absolute path to a nixpkgs directory.

Can be customized to select a nix-channel
e.g. /home/user/.nix-defexpr/channels/unstable/nixpkgs"
  :group 'nixos
  :type '(choice (const :tag "No channel" nil)
                 (directory "Custom path to a nixpkgs distribution")))

(defun nix-shell-command (sandbox &rest args)
  "Assembles a nix-shell command that gets executed in the specified sandbox."
  (append
   (list "nix-shell")
   (if nixos-nixpkgs-path
       (list "-I"
        (concat "nixpkgs=" nixos-nixpkgs-path)))
   (list "--run"
         (mapconcat 'identity args " ")
         sandbox)))


(defun nix-shell-string (sandbox &rest args)
  (let ((cmd (apply 'nix-shell-command sandbox args)))
    (mapconcat (lambda (x) (concat "'" x "'")) cmd " ")))


(defun nix-compile (sandbox &rest args)
  (interactive "Dsandbox: \nMcommand: ")
  (compile (apply 'nix-shell-string sandbox args)))

(defun nix-shell (sandbox &rest args)
  "Runs a nix-shell command in the given sandbox and returns its output."
  (shell-command-to-string (apply 'nix-shell-string sandbox args)))

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

(defun nixos-haskell-doc-paths (sandbox)
  "Returns a list of paths that contain haddock documentation."
  (if (gethash sandbox nixos-haskell-doc-path-map)
      (gethash sandbox nixos-haskell-doc-path-map)
    (puthash sandbox
             (list
              (nix-shell sandbox "echo -n $NIX_GHC_DOCDIR/libraries/")
              (concat (car (split-string (nix-shell sandbox "realpath $(dirname $NIX_GHC)/../share/doc/x86*/")))
                      "/"))
             nixos-haskell-doc-path-map)))

(defun nixos-filter-doc-dir (dir)
  (cddr
   (-map (lambda (f) (car f))
         (-filter (lambda (f) (cadr f))
                  (directory-files-and-attributes dir t)))))

(defun nixos-haskell-docs (sandbox)
  (apply 'append (-map 'nixos-filter-doc-dir (nixos-haskell-doc-paths sandbox))))

(defun helm-source-haskell-doc (sandbox)
  (helm-build-sync-source "haskell-doc-library"
    :candidates (-map (lambda (f) (cons (file-name-nondirectory f) f))
                      (nixos-haskell-docs sandbox))))

(defun nixos-haskell-doc-filter (files)
  (-filter
   (lambda (file)
    (not (string-match "index\\|frames\\.html\\|mini_\\|\\.js\\|\\.gif\\|\\.png\\|\\.css\\|\\.txt\\|haddock\\|src\\|LICENSE" file)))
   files))

(defun nixos-convert-module-file-name (module-file)
  (let ((filename (file-name-nondirectory module-file)))
    (cons (mapconcat 'identity (split-string filename "-" nil "\.html") ".")
          module-file)))

(defun nixos-haskell-search-doc (sandbox)
  (let* ((lib1 (helm :sources (helm-source-haskell-doc sandbox)))
         (html-dir (locate-file "html" (list lib1) nil
                                (lambda (f) (and (file-directory-p f) 'dir-ok))))
         (lib2 (or html-dir lib1))
         (ignore (print lib2))
         (doc-files (cddr (directory-files lib2 t)))
         (module-files (nixos-haskell-doc-filter doc-files))
         (modules (-map 'nixos-convert-module-file-name module-files))
         (module (helm :sources (helm-build-sync-source "haskell-library-module"
                                  :candidates modules))))
    module))

(defun nixos-haskell-open-doc ()
  (interactive)
  (let ((doc (nixos-haskell-search-doc (nixos-current-sandbox))))
    (and doc
     (browse-url (concat "file://" doc)))))

(defun nixos-clear-caches ()
  (interactive)
  (clrhash nixos-exec-path-map)
  (clrhash nixos-haskell-doc-path-map))

(provide 'nixos)

;;; nixos.el ends here
