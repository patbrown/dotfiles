;; Startup
(setq inhibit-startup-message t)

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (menu-bar-mode -1)
      (tooltip-mode -1)
      (set-fringe-mode 6)
      (scroll-bar-mode -1)))
;(scroll-bar-mode -1)
;(tool-bar-mode -1)
;(tooltip-mode -1)
;(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell t)
(setq package-enable-at-startup nil)

(defconst *is-a-mac*
  (eq system-type 'darwin)
  "Is this running on OS X?")

(defconst *is-linux*
  (eq system-type 'gnu/linux)
  "Is this running on Linux?")


;; start a server, unless one is already running
(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))

;; Bootstrapping STRAIGHT.EL for functional package management
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use package for convenience.
(straight-use-package 'use-package)
(require 'use-package)
(setq use-package-always-ensure t)

(provide 'bootstrapping)
