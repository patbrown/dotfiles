(use-package spacemacs-theme
  :ensure t
  :init (load-theme 'spacemacs-dark t))

(defun use-theme (theme) (load-theme theme t))

(provide 'theme)
