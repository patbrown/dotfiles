(use-package disable-mouse 
  :init
  (global-disable-mouse-mode))

(use-package hydra  :defer t)
(use-package avy  :defer t)

;; Tab line mode
(global-tab-line-mode t)
(setq tab-line-new-button-show nil)
(setq tab-line-separator "  ")
(setq tab-line-close-button-show nil)

(use-package key-chord
  :defer t
  :init
  (key-chord-mode 1)
  (key-chord-define-global "fj" 'hack/body))

(use-package frog-jump-buffer  :defer t)
(use-package ace-mc  :defer t)
(use-package expand-region  :defer t)
(use-package zzz-to-char  :defer t)
(use-package goto-last-change  :defer t)
(use-package ace-window  :defer t)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(use-package ace-jump-zap  :defer t)

(defhydra hack (global-map "C-c '")
  ("c" avy-goto-char :exit t)
  ("x" er/expand-region)
  ("z" zzz-to-char)
  ("u" goto-last-change)
  ("i" crux-find-user-init-file)
  ("r" crux-rename-file-and-buffer)
  ("d" crux-delete-file-and-buffer)
  ("t" crux-transpose-windows)
  ("f" crux-recentf-find-file)
  (">" insert-register)
  ("<" append-to-register)
  ("o" view-register)
  ("m" ace-mc-add-multiple-cursors :exit t)
  ("n" npm)
  ("w" avy-goto-word-1 :exit t)
  ("l" avygoto-line :exit t)
  ("y" copy-to-buffer)
  ("g" magit-status)
  )

(defhydra hydra-find (global-map "C-c +")
  ("f" counsel-projectile-find-file)
  ("d" cider-docview-source))

(use-package ace-jump-mode
  
  :init
  (autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
  ;(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
  (setq ace-jump-mode-gray-background nil)
  (setq ace-jump-mode-move-keys
        '(?j ?f ?k ?d ?l ?s ?\; ?a ?h ?g ?u ?r ?i ?e ?o ?w ?p ?q ?y ?t ?m ?v ?, ?c ?. ?x ?/ ?z ?n ?b ?7 ?4 ?8 ?3 ?9 ?2 ?0 ?1 ?6 ?5)))
(winner-mode 1)

(use-package windmove
  
  :config
  (defun ignore-error-wrapper (fn)
    "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
    (lexical-let ((fn fn))
      (lambda ()
        (interactive)
        (ignore-errors
          (funcall fn))))))

(use-package buffer-move )

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))
                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))
                  (s1 (window-start w1))
                  (s2 (window-start w2)))
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(defun smarter-move-beginning-of-line (arg)
  (interactive "^p")
  (setq arg (or arg 1))
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(use-package multiple-cursors
  :defer t
  :bind (("C-C M->" . mc/mark-next-like-this)
         ("C-c M-<" . mc/mark-previous-like-this)
         ("C-c M-!" . mc/mark-all-like-this)
         ("C-c C-c M-!" . mc/edit-lines)))

(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :init
  (progn
    (require 'undo-tree)
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-auto-save-history nil)
    (setq undo-tree-visualizer-diff t)))

(use-package zoom
  :init
  (progn
    (custom-set-variables
     '(zoom-mode t))
    (custom-set-variables
     '(zoom-size '(0.618 . 0.618)))    
    (global-set-key (kbd "C-x +") 'zoom)))

(provide 'navigating)
