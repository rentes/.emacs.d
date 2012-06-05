; encoding stuff
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
; removing Byte Order Mark (BOM)
(set-buffer-file-coding-system 'unix)

; load-paths
(add-to-list 'load-path "c:/users/0930/.emacs.d/lisp/")
; theme path
(add-to-list 'custom-theme-load-path "c:/users/0930/.emacs.d/themes")
; other customizations
(setq auto-save-default nil)
(delete-selection-mode 1)
(global-hl-line-mode 1)
(global-linum-mode 1)
(column-number-mode 1)
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

(setq frame-title-format
  '("emacs%@" (:eval (system-name)) ": " (:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b")) " [%*]"))

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; marmalade
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))