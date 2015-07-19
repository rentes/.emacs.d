; -*- coding: utf-8 -*-
; ==========================================================================
; Turn off mouse interface early in startup to avoid momentary display
; ==========================================================================
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; ==========================================================================
; no splash screen
; ==========================================================================
(setq inhibit-startup-message t)
; ==========================================================================
; load-path (added elpa/ and lisp/ recursively)
; ==========================================================================

(let ((default-directory "~/.emacs.d/elpa/"))
      (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/lisp/"))
      (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs.d/lisp")
; ==========================================================================
; Variables configured via the interactive 'customize' interface
; get recorded on custom.el file
; ==========================================================================
(setq custom-file "custom.el")
(load custom-file)
