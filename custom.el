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
;(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
;(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:scm@%h:"))))
; ==========================================================================
; load library
; ==========================================================================
(load-library "iso-transl")
; ==========================================================================
; proxy settings
; ==========================================================================
(setq url-proxy-services '(("no_proxy" . "127.0.0.1, 172.18.200.0/24, 172.18.210.0/24, 172.18.214.0/24")
			   ("http" . "proxyarrsec.efacec.pt:8080")
			   ("https" . "proxyarrsec.efacec.pt:8080")))
; ==========================================================================
; python jedi
; ==========================================================================
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

; ==========================================================================
; org-mode latex
; ==========================================================================
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
	     '("article"
	       "\\documentclass{article}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq-default fill-column 80)
;(whitespace-mode 1)

;; (set-face-attribute 'mode-line nil
;;     :foreground "black" :background "orange"
;;     :inverse-video nil :height 100
;;     :box '(:line-width 1 :color "white" :style nil))

; ==========================================================================
; load theme
; ==========================================================================
;(load-theme 'solarized-light t)
;(load-theme 'solarized-dark t)
;(load-theme 'waher t)
(load-theme 'ample-zen t)

; ==========================================================================
; w3m settings - see http://www.emacswiki.org/emacs/emacs-w3m for more info
; ==========================================================================
;; (require 'w3m-e21)
;; (provide 'w3m-e23)
(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url)
 (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
 ;; optional keyboard short-cut
 (global-set-key "\C-xm" 'browse-url-at-point)
 ;; enabling cookies
(setq w3m-use-cookies t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("0f0e3af1ec61d04ff92f238b165dbc6d2a7b4ade7ed9812b4ce6b075e08f49fe" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "7a9f392481b6e2fb027ab9d8053ab36c0f23bf5cc1271206982339370d894c74" "a37600b047da389eccc4a17b5f165d512fb1d32f18d93cffb28154b5f4eb4437" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 123 :width normal :foundry "unknown" :family "Terminus"))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#cb4b16" :height 1.0 :family "Terminus"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :height 0.9 :family "Terminus"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :height 0.85 :family "Terminus"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#b58900" :height 0.8 :family "Terminus"))))
 '(org-level-5 ((t (:inherit variable-pitch :foreground "#2aa198" :height 0.75 :family "Terminus"))))
 '(org-level-6 ((t (:inherit variable-pitch :foreground "#859900" :height 0.7 :family "Terminus"))))
 '(org-level-7 ((t (:inherit variable-pitch :foreground "#dc322f" :height 0.65 :family "Terminus"))))
 '(org-level-8 ((t (:inherit variable-pitch :foreground "#268bd2" :height 0.6 :family "Terminus")))))
