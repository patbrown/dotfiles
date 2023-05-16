;;; npm-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "npm" "npm.el" (0 0 0 0))
;;; Generated autoloads from npm.el

(autoload 'npm "npm" "\
Entrypoint function to the package.
This will first check to make sure there is a package.json file and then open the menu." t nil)

(register-definition-prefixes "npm" '("npm-mode"))

;;;***

;;;### (autoloads nil "npm-common" "npm-common.el" (0 0 0 0))
;;; Generated autoloads from npm-common.el

(register-definition-prefixes "npm-common" '("npm-common-"))

;;;***

;;;### (autoloads nil "npm-init" "npm-init.el" (0 0 0 0))
;;; Generated autoloads from npm-init.el

(autoload 'npm-init "npm-init" "\
Initialize a project folder as a npm project." t nil)

(register-definition-prefixes "npm-init" '("npm-init--"))

;;;***

;;;### (autoloads nil "npm-install" "npm-install.el" (0 0 0 0))
;;; Generated autoloads from npm-install.el

(autoload 'npm-install "npm-install" "\
Invoke the compile mode with the install prefix-command and ARGS if provided.

\(fn &optional ARGS)" t nil)

(register-definition-prefixes "npm-install" '("npm-install-"))

;;;***

;;;### (autoloads nil "npm-publish" "npm-publish.el" (0 0 0 0))
;;; Generated autoloads from npm-publish.el

(autoload 'npm-publish "npm-publish" "\
Invoke the compile mode with ARGS to publish an NPM package to a registry.

\(fn &optional ARGS)" t nil)

(register-definition-prefixes "npm-publish" '("npm-publish-"))

;;;***

;;;### (autoloads nil "npm-run" "npm-run.el" (0 0 0 0))
;;; Generated autoloads from npm-run.el

(autoload 'npm-run "npm-run" "\
Invoke the compile mode with the run prefix-command and ARGS if provided.

\(fn &optional ARGS)" t nil)

(register-definition-prefixes "npm-run" '("npm-run--"))

;;;***

;;;### (autoloads nil "npm-update" "npm-update.el" (0 0 0 0))
;;; Generated autoloads from npm-update.el

(autoload 'npm-update "npm-update" "\
Invoke the compile mode with the update prefix-command and ARGS if provided.

\(fn &optional ARGS)" t nil)

(register-definition-prefixes "npm-update" '("npm-update--"))

;;;***

(provide 'npm-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; npm-autoloads.el ends here
