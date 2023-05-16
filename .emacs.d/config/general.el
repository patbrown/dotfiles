;; Sensible things to do
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default window-combination-resize t x-stretch-cursor t)
(unless (display-graphic-p) (xterm-mouse-mode 1))
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
(setq dired-dwim-target t)
(setq undo-limit 80000000 truncate-string-ellipsis "…" scroll-margin 2)
(display-time-mode 1)
(global-subword-mode 1)
(setq blink-cursor-interval 0.4)
(blink-cursor-mode)
(column-number-mode)
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; History
(setq savehist-file "~/.emacs.d/.savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Buffer Navigation
(setq ns-pop-up-frames nil)
(size-indication-mode t)
(transient-mark-mode 1)
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)

(when (< 26 emacs-major-version)
  (tab-bar-mode 1)
  (setq tab-bar-show 1)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-new-tab-choice "*scratch*")
  (setq tab-bar-tab-hints t)
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator)))

;; General Look and Feel
(setq dired-use-ls-dired nil)
(setq display-time-24hr-format t)
(setq display-time-load-average t)
(display-time)
(if (fboundp 'fringe-mode) (fringe-mode -1))
(global-linum-mode 1)
(setq linum-format "%d  ")
(setq frame-title-format "%b")
(show-paren-mode 1)
(global-hl-line-mode 1)
(setq utf-translate-cjk-mode nil)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

;; set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun clear-undo-tree ()
  (interactive)
  (setq buffer-undo-tree nil))
(global-set-key [(control c) u] 'clear-undo-tree)

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(setq create-lockfiles nil)
(eval-after-load 'term
  '(term-set-escape-char ?\C-x))

(use-package multiple-cursors :straight t :defer t)
(use-package smartrep :straight t)
(use-package npm :straight t :defer t)
(use-package crux :straight t :defer t)
(use-package keycast :straight t)
(use-package browse-kill-ring :straight t :defer t
  :init (global-set-key (kbd "C-c C-y") 'browse-kill-ring))
(use-package easy-kill :straight t :defer t
  :config (global-set-key [remap kill-ring-save] 'easy-kill))
(use-package clipetty :straight t
  :hook (after-init . global-clipetty-mode))

(use-package fzf :straight t
  :config
    (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "grep -nrH"
        fzf/position-bottom t
        fzf/window-height 15))

(use-package smart-mode-line :straight t :defer t
  :init
  (setq sml/name-width 40)
  (setq sml/mode-width 'full)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup)
  (sml/apply-theme 'respectful))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(use-package which-key :straight t :defer t
  :init
  (require 'which-key)
  (which-key-mode))

(use-package discover :straight t :defer t
  :config
  (global-discover-mode 1))

(use-package discover-my-major :straight t :defer t
  :config
  (global-set-key (kbd "C-h C-m") 'discover-my-major))

(rename-modeline "clojure-mode" clojure-mode "clj")
(rename-modeline "paredit-mode" paredit-mode "()")

(provide 'general)
