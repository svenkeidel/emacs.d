;;; nixos.el --- Utility functions to talk with nix-shell sandboxes

;; Author: Sven Keidel <svenkeidel@gmail.com>
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: nixos
;; URL: http://github.com/svenkeidel/nixos.el

(provide 'nixos)

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
