; ====================================================================
; encodings
; ====================================================================
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-buffer-file-coding-system 'unix) ; removing Byte Order Mark (BOM)
; ====================================================================
; other customizations
; ====================================================================
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

;; Navigate between windows using Alt-1, Alt-2, Shift-left,
;; shift-up, shift-right
(windmove-default-keybindings)

;; Enable copy and pasting from clipboard
(setq x-select-enable-clipboard t)

;; Tabbar/ruler
(tabbar-mode 1)
(set-face-attribute 'default nil :height 110)
;(set-face-attribute 'linum nil :height 90)

; ====================================================================
; change frame title to include complete path on open file
; ====================================================================
(setq frame-title-format
      '("emacs%@" (:eval (system-name)) ": " 
	(:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b")) " [%*]"))

; ====================================================================
; turn on font-lock mode (syntax highlighting)
; ====================================================================
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))
; ====================================================================
; An alist of archives from which to fetch
; ====================================================================
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("elpa" . "http://tromey.com/elpa/")))
; ====================================================================
; backup policy
; ====================================================================
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files
(setq backup-by-copying t) ; stop backup from changing file creation date
			   ; of original file
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))
; ====================================================================
; tramp settings
; ====================================================================
(setq tramp-verbose 10)
; ====================================================================
; load library
; ====================================================================
(load-library "iso-transl")

; ====================================================================
; python jedi
; ====================================================================
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)


; ====================================================================
; org-mode workflow states
; ====================================================================
(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "WORKING" "VERIFY" "|" "DONE" "DELEGATED")))

(setq org-todo-keyword-faces
      '(("WORKING" . (:foreground "light blue" :weight bold))
	("VERIFY" . (:foreground "yellow" :weight bold))))


(setq-default fill-column 80)

; ====================================================================
; load theme
; ====================================================================

(require 'powerline)
;(require 'moe-theme)
;(moe-dark)
;(load-theme 'flatui t)
(load-theme 'zenburn t)

; ====================================================================
; Org Mode customizations for better appearance
; ====================================================================
(setq org-hide-leading-stars t)
(setq line-spacing '0.25)
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; ====================================================================
; fill column indicator (vertical line on the 80th column)
; ====================================================================
(require 'fill-column-indicator)
(setq fci-rule-color "darkgrey")
(fci-mode)


; ====================================================================
; CUA Mode
; ====================================================================
(cua-mode)

; ====================================================================
; Auto-fill Mode
; ====================================================================
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(global-visual-line-mode t)

; ====================================================================
; w3m settings - see http://www.emacswiki.org/emacs/emacs-w3m
; ====================================================================
(require 'w3m-load)

; ====================================================================
; C/C++ Programming
; http://truongtx.me/2013/03/10/emacs-setting-up-perfect-environment-for-cc-programming/
; http://truongtx.me/2013/04/12/emacs-setting-up-perfect-cc-programming-environment/
; for further details
; ====================================================================
(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)


;; (add-to-list 'load-path
;; 			 "~/.emacs.d/elpa/yasnippet-20150413.1403")
;; (require 'yasnippet)
;; (yas-global-mode 1)


(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
			 "~/.emacs.d/elpa/auto-complete-20150408.1132/dict")
(ac-config-default)
(setq ac-ignore-case nil)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(require 'auto-complete-c-headers)
(define-key c++-mode-map (kbd "C-S-<return>") 'ac-complete-c-headers)
(require 'member-functions)
(setq mf--source-file-extension "cpp")
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)
(require 'ecb)
(require 'ecb-autoloads)
(setq ecb-layout-name "left8")
(setq ecb-examples-bufferinfo-buffer-name nil)
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-compile-window-height 12)
(require 'flymake)
(add-hook 'find-file-hook 'flymake-find-file-hook)
;(require 'flyspell)
;(setq flyspell-issue-message-flg nil)
;; (add-hook 'enh-ruby-mode-hook
;; 		  (lambda () (flyspell-prog-mode)))
;; (add-hook 'web-mode-hook
;; 		  (lambda () (flyspell-prog-mode)))
;; flyspell mode breaks auto-complete mode without this.
;(ac-flyspell-workaround)

; ====================================================================
; Highlight Indentation
; ====================================================================
(require 'highlight-indentation)
(add-hook 'enh-ruby-mode-hook
		  (lambda () (highlight-indentation-current-column-mode)))
(add-hook 'coffee-mode-hook
		  (lambda () (highlight-indentation-current-column-mode)))


; ==========================================================================
; Ruby (Enhanced Ruby Mode)
; ==========================================================================

(add-to-list 'auto-mode-alist
             '("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" .
			   enh-ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" .
			   enh-ruby-mode))

(add-to-list 'ac-modes 'enh-ruby-mode)
(add-to-list 'ac-modes 'web-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
(remove-hook 'enh-ruby-mode-hook 'erm-define-faces)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)
(require 'ruby-end)

(require 'smartparens-config)
(require 'smartparens-ruby)
(sp-with-modes '(rhtml-mode)
               (sp-local-pair "<" ">")
               (sp-local-pair "<%" "%>"))

; ====================================================================
; Rails
; ====================================================================
(setq load-path (cons
                 (expand-file-name "~/.emacs.d/lisp/rails-reloaded")
                 load-path))
(require 'rails-autoload)

; ====================================================================
; Execute the current file
; ====================================================================
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
  (if (string-equal fSuffix "el") ; special case for emacs lisp
	  (load (file-name-sans-extension fName))
	(if progName
		(progn
		  (message "Running...")
		  (shell-command cmdStr "*run-current-file output*"))
	  (message "No recognized program file suffix for this file.")
	  ))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("0ba649556dc51762e6794b92017f6f7406754ae3136eafef686d81c6da176cc5" "789844278c5a75283b5015c1fc7bebe7e4cf97843b8f8cffe21fafa05e81e90a" "282606e51ef2811142af5068bd6694b7cf643b27d63666868bc97d04422318c1" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "42ccd5eadda3546a89026b94794df7f4addadf25417b96917cf9db2f892b25a4" default)))
 '(global-hl-line-mode t)
 '(global-linum-mode t)
 '(org-todo-keyword-faces
   (quote
	(("WORKING" :foreground "light blue" :weight normal)
	 ("VERIFY" :foreground "yellow" :weight normal)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:background "#3F3F3F" :foreground "#9FC59F" :height 0.9 :width normal :family "Terminus"))))
 '(org-done ((t (:foreground "#AFD8AF" :weight normal))))
 '(org-level-1 ((t (:inherit variable-pitch :foreground "#DFAF8F" :weight bold :height 1.0 :family "Terminus"))))
 '(org-level-2 ((t (:inherit variable-pitch :foreground "#859900" :weight semi-bold :height 0.9 :family "Terminus"))))
 '(org-level-3 ((t (:inherit variable-pitch :foreground "#268bd2" :weight bold :height 0.85 :family "Terminus"))))
 '(org-level-4 ((t (:inherit variable-pitch :foreground "#b58900" :weight bold :height 0.8 :family "Terminus"))))
 '(org-level-5 ((t (:foreground "#93E0E3" :family "Terminus"))))
 '(org-level-6 ((t (:foreground "#9FC59F" :family "Terminus"))))
 '(org-level-7 ((t (:foreground "#8C5353" :family "Terminus"))))
 '(org-level-8 ((t (:foreground "#4C7073" :family "Terminus"))))
 '(org-link ((t (:foreground "#D0BF8F" :underline t :family "Terminus"))))
 '(org-priority ((t (:inherit font-lock-keyword-face :weight normal))))
 '(org-tag ((t (:weight bold))))
 '(org-todo ((t (:foreground "#CC9393" :weight normal)))))
