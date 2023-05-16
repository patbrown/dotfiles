(use-package burly :defer t)
(use-package bufler :defer t)
(use-package company :defer t)
(use-package ivy-prescient :defer t)
(use-package prescient :defer t)
(use-package transient :defer t)
(use-package smex :defer t)

(use-package magit  
  :init (if (not (boundp 'project-switch-commands)) 
	    (setq project-switch-commands nil)))

(use-package ivy  
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :init
  (ivy-prescient-mode)
  (defvar my-ivy-builders '(ivy--regex-ignore-order
                            ivy--regex-fuzzy
			    ivy--regex-plus)
  "Preferred values for `ivy--regex-function'.")

(defun my-ivy-matcher-descs ()
  "Return a brief description of `ivy--regex-function'."
  (pcase ivy--regex-function
    ('ivy--regex-fuzzy        "fuzzy")
    ('ivy--regex-ignore-order "order")
    ('ivy--regex-plus         "plus")
    (_                        "other")))

(advice-add 'ivy--matcher-desc :override #'my-ivy-matcher-descs)

(defun my-ivy-rotate-builders ()
  "Slide `ivy--regex-function' across `my-ivy-builders'."
  (when my-ivy-builders
    (setq ivy--old-re nil)
    (setq ivy--regex-function
          (or (cadr (memq ivy--regex-function my-ivy-builders))
              (car my-ivy-builders)))))

(advice-add 'ivy-toggle-fuzzy :override #'my-rotate-builders))

(use-package ivy-rich
  :after counsel
  :init (setq ivy-rich-path-style 'abbrev
              ivy-virtual-abbreviate 'full)
  :config
  (ivy-mode)
  (ivy-rich-mode))

(use-package counsel :after ivy
  :config (counsel-mode))

(use-package swiper :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-last-sexp)
(defun keymap-unset-key (key keymap)
  "Remove binding of KEY in a keymap KEY is a string or vector
representing a sequence of keystrokes."
  (interactive
   (list (call-interactively #'get-key-combo)
	 (completing-read "Which map: " minor-mode-map-alist nil t)))
  (let ((map (rest (assoc (intern keymap) minor-mode-map-alist))))
    (when map
      (define-key map key nil)
      (message  "%s unbound for %s" key keymap))))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(defun rk/copilot-disable-predicate ()
  "When copilot should not automatically show completions."
  (or rk/copilot-manual-mode
      (member major-mode rk/no-copilot-modes)
      (company--active-p)))

(add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)
(defvar rk/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

(defun rk/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode rk/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq rk/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq rk/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)
(defun rk/copilot-complete-or-accept ()
  "Command that either triggers a completion or accepts one if one
is available. Useful if you tend to hammer your keys like I do."
  (interactive)
  (if (copilot--overlay-visible)
      (progn
        (copilot-accept-completion)
        (open-line 1)
        (next-line))
    (copilot-complete)))

(define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
(define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
(define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
(define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)
(defun rk/copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (or (copilot-accept-completion)
      (company-yasnippet-or-completion)
      (indent-for-tab-command)))

(define-key global-map (kbd "<tab>") #'rk/copilot-tab)
(defun rk/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'rk/copilot-quit)

(use-package vimish-fold
  :straight t
  :init
  (vimish-fold-global-mode 1)
  (global-set-key (kbd "C-c f s") #'vimish-fold)
  (global-set-key (kbd "C-c f d") #'vimish-fold-delete)
  (global-set-key (kbd "C-c f a") #'vimish-fold-unfold-all)
  (global-set-key (kbd "C-c f A") #'vimish-fold-refold-all)
  (global-set-key (kbd "C-c f f") #'vimish-fold-toggle)
  (global-set-key (kbd "C-c f F") #'vimish-fold-toggle-all))
(use-package markdown-mode
  :straight t
  :defer t)
(use-package project
  :straight t
  :defer t)

(use-package paredit
  :straight t
  :ensure t
  :init
  (eval-after-load "paredit"
  #'(define-key paredit-mode-map (kbd "C-j") 'newline-and-indent))
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'prog-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  (define-key paredit-mode-map (kbd "M-s") nil)
  :diminish nil)

(use-package paredit-everywhere
  :straight t
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
  (add-hook 'css-mode-hook 'paredit-everywhere-mode))


(use-package rainbow-delimiters
  :straight t
  :init
  (progn (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
	 (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
         (add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
         (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)))

(use-package projectile
  :straight t
  :diminish projectile-mode
  :config (projectile-mode)
  :init
  (when (file-directory-p "~/")
    (setq projectile-project-search-path '("~/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
 :straight t
 :after projectile
 :config
 (counsel-projectile-mode 1))


(use-package yaml-mode
  :straight t
  :config
  (add-hook 'yaml-mode-hook
	    (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(setq cider-eldoc-display-for-symbol-at-point nil)

(use-package clojure-mode
  :straight t)

(use-package lsp-mode
  :straight t
  :ensure t
  :hook
  ((clojure-mode . lsp))
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :straight t)

(use-package aggressive-indent
  :straight t
  :ensure t)


(use-package cider
  :straight t
  :ensure t
  :config
  (setq cider-clojure-cli-global-options "-A:dev")
  (setq clojure-toplevel-inside-comment-form t)
  (setq clojure-align-forms-automatically t)
  (add-to-list 'auto-mode-alist '("\\.selmer\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.repl\\'" . clojure-mode))
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)
  
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojurec-mode-hook #'subword-mode)
  (add-hook 'clojurec-mode-hook #'paredit-mode)
  (add-hook 'clojurec-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojurescript-mode-hook #'subword-mode)
  (add-hook 'clojurescript-mode-hook #'paredit-mode)
  (add-hook 'clojurescript-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'subword-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook
          (lambda()
            (local-unset-key (kbd "C-<up>"))))
(add-hook 'cider-repl-mode-hook
          (lambda()
            (local-unset-key (kbd "C-<down>"))))
  (setq nrepl-log-messages t)
  (setq nrepl-hide-special-buffers t)
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-show-error-buffer nil)
  (setq cider-auto-select-error-buffer nil)
  (setq nrepl-buffer-name-show-port t)
  (setq cider-repl-display-in-current-window t)
  (setq cider-prompt-save-file-on-load nil)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 2000)
  (setq cider-ovelays-use-font-lock t)
  (setq cider-repl-display-help-banner nil))

(define-key cider-mode-map (kbd "C-c C-r") 'cider-eval-region)
(define-key cider-mode-map (kbd "C-c C-j") 'jump-to-register)

(defun cider-eval-last-sexp-in-buffer-as-current-region ()
  "Evaluate the expression preceding point and replace it with its result."
  (interactive)
  (let ((last-sexp (cider-last-sexp)))
    (cider-nrepl-sync-request:eval last-sexp)
    (region-beginning)
    (cider-interactive-eval last-sexp
                            (cider-eval-print-handler)
                            nil
                            (cider--nrepl-pr-request-map))
    (region-end)))


(defun cider-tap-last-expression ()
       (interactive)
       (cider-interactive-eval
         (format "(tap> %s)"
                 (cider-last-expression))))

(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)

(provide 'programming)
