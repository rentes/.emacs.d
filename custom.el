;;====================================================================
;; encodings
;;====================================================================
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'unix) ;; removing Byte Order Mark (BOM)

;;====================================================================
;; proxy
;;====================================================================
(setq url-proxy-services '(("no_proxy" . "efacec\\.com")
                           ("http" . "172.18.200.136:3128")
			   ("https" . "172.18.200.136:3128")))

;;====================================================================
;; other customizations
;;====================================================================
(setq auto-save-default nil)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(global-linum-mode t) ;; enable line numbers globally
(column-number-mode 1)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
(defalias 'yes-or-no-p 'y-or-n-p)
;; make mouse pointer stay out of the way of editing
(if (display-mouse-p) (mouse-avoidance-mode 'animate))

;; Navigate between windows using Alt-1, Alt-2, Shift-left,
;; shift-up, shift-right
(windmove-default-keybindings)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

(load-library "iso-transl")
;;====================================================================
;; change frame title to include complete path on open file
;;====================================================================
(setq frame-title-format
      '("emacs%@" (:eval (system-name)) ": " 
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b")) " [%*]"))

;;====================================================================
;; turn on font-lock mode (syntax highlighting)
;;====================================================================
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))
;;====================================================================
;; backup policy
;;====================================================================
(setq make-backup-files nil) ;; stop creating those backup~ files
(setq auto-save-default nil) ;; stop creating those #auto-save# files
(setq backup-by-copying t) ;; stop backup from changing file creation date
			   ;; of original file
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
;;====================================================================
;; tramp settings
;;====================================================================
(setq tramp-verbose 10)
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "ssh")

;;====================================================================
;; org-mode workflow states
;;====================================================================
(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "WORKING" "VERIFY" "|" "DONE" "DELEGATED")))

(setq org-todo-keyword-faces
      '(("WORKING" . (:foreground "light blue" :weight bold))
	("VERIFY" . (:foreground "yellow" :weight bold))))

;;====================================================================
;; load theme
;;====================================================================
(load-theme 'material t) ;; load material theme
;; (require 'moe-theme)
;; (require 'moe-theme-switcher)
;; (moe-dark)
;; (require 'powerline)
;; (powerline-moe-theme)

;;====================================================================
;; Org Mode customizations for better appearance
;;====================================================================
(setq org-hide-leading-stars t)
(setq line-spacing '0.25)
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;====================================================================
;; CUA Mode
;;====================================================================
(cua-mode)

;;====================================================================
;; Highlight indentation
;;====================================================================
(require 'highlight-indentation)
(add-hook 'enh-ruby-mode-hook
(lambda () (highlight-indentation-current-column-mode)))

(add-hook 'coffee-mode-hook
(lambda () (highlight-indentation-current-column-mode)))

;;====================================================================
;; Flyspell
;;====================================================================
(require 'flyspell)
(setq flyspell-issue-message-flag nil)
(add-hook 'enh-ruby-mode-hook
(lambda () (flyspell-prog-mode)))

(add-hook 'web-mode-hook
(lambda () (flyspell-prog-mode)))

(require 'auto-complete)
(ac-flyspell-workaround)

;;====================================================================
;; Execute the current file
;;====================================================================
(defun run-current-file()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call python xx.py in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is emacs lisp, run the byte compiled version if exist."
(interactive)
(let* (
	   (suffixMap
		`(("php" . "php")
		  ("pl" . "perl")
		  ("py" . "python")
		  ("rb" . "ruby")
		  ("js" . "node")
		  ("sh" . "bash")
		  ("ml" . "ocaml")
		  )
		)
	   (fName (buffer-file-name))
	   (fSuffix (file-name-extension fName))
	   (progName (cdr (assoc fSuffix suffixMap)))
	   (cmdStr (concat progName " \"" fName "\""))
	   )
  (when (buffer-modified-p)
	(when (y-or-n-p "Buffer modified. Do you want to save first?")
	  (save-buffer)))
  (if (string-equal fSuffix "el") ;; special case for emacs lisp
	  (load (file-name-sans-extension fName))
	(if progName
		(progn
		  (message "Running...")
		  (shell-command cmdStr "*run-current-file output*"))
	  (message "No recognized program file suffix for this file.")
	  ))))

;;====================================================================
;; fci
;;====================================================================
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(add-hook 'after-change-major-mode-hook 'fci-mode)

;;====================================================================
;;; Interactively Do Things mode
;;====================================================================
(require 'ido)
(ido-mode t)

;;====================================================================
;; Auto-complete mode
;;====================================================================
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories
             "~/.emacs.d/elpa/auto-complete-20150618.1949/dict/")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;;====================================================================
;; w3m
;;====================================================================
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

;;====================================================================
;; Emoji
;;====================================================================
;; (company-emoji-init)
;; (add-to-list 'company-backends 'company-emoji)

;;====================================================================
;; Load Ruby customizations
;;====================================================================
(load "~/.emacs.d/ruby")
;;====================================================================
;; Load Rails customizations
;;====================================================================
(load "~/.emacs.d/rails")

;;====================================================================
;; Load Python customizations
;;====================================================================
(load "~/.emacs.d/python")

(custom-set-variables
 ;;; custom-set-variables was added by Custom.
 ;;; If you edit it by hand, you could mess it up, so be careful.
 ;;; Your init file should contain only one such instance.
 ;;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c7fb35ba0e1e7f2e4b48ba1508ce5ee309192c6e5e671dba296dc259844426e6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "4a162cd971cf3c059e827d6b5aa0bd07488cb5995782c0fa0ce20621bbc4a596" default))))

(custom-set-faces
 ;;; custom-set-faces was added by Custom.
 ;;; If you edit it by hand, you could mess it up, so be careful.
 ;;; Your init file should contain only one such instance.
 ;;; If there is more than one, they won't work right.
 )

