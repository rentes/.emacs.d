; ==========================================================================
; encodings
; ==========================================================================
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'unix) ; removing Byte Order Mark (BOM)
; ==========================================================================
; other customizations
; ==========================================================================
(setq auto-save-default nil)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(column-number-mode 1)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)
; make mouse pointer stay out of the way of editing
(if (display-mouse-p) (mouse-avoidance-mode 'animate))
; ==========================================================================
; change frame title to include complete path on open file
; ==========================================================================
(setq frame-title-format
      '("emacs%@" (:eval (system-name)) ": " 
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b")) " [%*]"))

; ==========================================================================
; turn on font-lock mode (syntax highlighting)
; ==========================================================================
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))
; ==========================================================================
; An alist of archives from which to fetch
; ==========================================================================
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("elpa" . "http://tromey.com/elpa/")))
; ==========================================================================
; backup policy
; ==========================================================================
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files
(setq backup-by-copying t) ; stop backup from changing file creation date
			   ; of original file
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
; ==========================================================================
; tramp settings
; ==========================================================================
(setq tramp-verbose 10)
; ==========================================================================
; load library
; ==========================================================================
(load-library "iso-transl")
; ==========================================================================
; python jedi
; ==========================================================================
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(setq-default fill-column 80)

; ==========================================================================
; load theme
; ==========================================================================
(load-theme 'solarized-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
