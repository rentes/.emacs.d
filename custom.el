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
			 ("elpa" . "http://tromey.com/elpa/")))
; ==========================================================================
; backup policy
; ==========================================================================
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files
(setq backup-by-copying t) ; stop backup from changing file creation date 
			   ; of original file
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
