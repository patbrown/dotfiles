(load-library "~/.emacs.d/bootstrap.el")
(mapc 'load (file-expand-wildcards "~/.emacs.d/config/*.el"))
(mapc 'load-library (file-expand-wildcards "~/.emacs.d/config/*.el"))

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "C-v"))
(global-unset-key (kbd "M-s"))

(if (eq system-type 'darwin)
    (mapc 'load (file-expand-wildcards "~/.emacs.d/macos/*.el"))
  (mapc 'load (file-expand-wildcards "~/.emacs.d/linux/*.el")))

(if (eq system-type 'darwin)
    (mapc 'load-library (file-expand-wildcards "~/.emacs.d/macos/*.el"))
    (mapc 'load-library (file-expand-wildcards "~/.emacs.d/linux/*.el")))
