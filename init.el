; -*- coding: utf-8 -*-
; ==========================================================================
; load-path
; ==========================================================================
(add-to-list 'load-path "~/.emacs.d/")
(let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-subdirs-to-load-path))
; ==========================================================================
; Variables configured via the interactive 'customize' interface
; get recorded on custom.el file
; ==========================================================================
(setq custom-file "custom.el")
(load custom-file)

; ==========================================================================
; my customizations
; ==========================================================================
(load "my-custom.el")
