;;; package --- Summary
;;; Commentary:
;;;   An Emacs configuration for VIM users. To install:
;;;     1. Clone the config with git clone https://github.com/wmedrano/hemacs .emacs.d
;;;     2. Startup and press "M-x package-refresh-contents".
;;;        Note that M-x is Alt+x. M == Alt.
;;;     3. Press "M-x package-install-selected-packages"
;;;     4. Optional: Press "M-x nerd-icons-install-fonts" for better icons in GUI mode.
;;;     4. Restart Emacs.
;;; For the initial trial period, you may notice some noisy messages caused by
;;; Emacs byte compiling packages for the first time.
;;;
;;; Fonts:
;;;   For icons in the terminal, install a Nerd Font and set it as the terminal's
;;;   font. Nerd fonts are normal fonts patched to have
;;;   icons. https://www.nerdfonts.com/font-downloads. To get the font to apply
;;;   in GUI mode, it must also be set as the default font. This can be edited in
;;;   the custom-set-faces variable. The default value is "JetBrainsMono Nerd
;;;   Font" which works fine if you downloaded and installed the "JetBrainsMono
;;;   Nerd Font"
;;;
;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(markdown-mode diff-hl company rust-mode which-key magit doom-modeline nerd-icons-ivy-rich ivy-rich counsel ivy typescript-mode eglot atom-one-dark-theme evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrainsMono Nerd Font" :foundry "JB" :slant normal :weight normal :height 120 :width normal)))))

(require 'package)
;; Add the melpa.org package archive. You can browse packages at melpa.org.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help
;;
;; Which key is used to display a popup of valid keys after a prefix and the
;; functions that they run.
;;
;; To view documentation for a function, use the Emacs built-in keybinding "C-h
;; f" to jump to the documentation for a function and "C-h v" to jump to the
;; documentation for a variable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'which-key)
(which-key-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming
;;
;; For theming we use atom dark theme. For the modeline we use doom since it
;; looks pretty. We also remove some very retro stuff and add some basics like
;; the line number.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'atom-one-dark-theme)
(require 'doom-modeline)
(setq-default
 ;; Don't show retro startup screen.
 inhibit-startup-screen t
 ;; Don't show buffer encoding. For example "UTF-8".
 doom-modeline-buffer-encoding nil
 ;; Show mini-scrollbar in far left-most space of modeline.
 doom-modeline-hud t)
(load-theme 'atom-one-dark t)
(doom-modeline-mode t)

(setq-default display-line-numbers-grow-only t)
(global-display-line-numbers-mode t)
(global-hl-line-mode t)
(column-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings
;;
;; For keybindings we use evil to provide a VIM like experience. We also add
;; additional keybindings that are similar to VSCode. To learn what a function
;; does, use "C-h f". For example: "C-h f eglot-code-actions RET" will show
;; documentation for the function eglot-code-actions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'evil)
(evil-mode t)

;; Modes that you cannot insert text into.
(add-to-list 'evil-motion-state-modes 'dired-mode)
(add-to-list 'evil-motion-state-modes 'magit-diff-mode)
(add-to-list 'evil-motion-state-modes 'special-mode)

;; The below are taken from VSCode.
(require 'eglot)
(context-menu-mode t) ;; Enables right clicking in GUI mode.
(define-key eglot-mode-map   (kbd "C-.")     #'eglot-code-actions)
(define-key eglot-mode-map   (kbd "<f2>")    #'eglot-rename)
(define-key flymake-mode-map (kbd "<f8>")    #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "S-<f8>")  #'flymake-goto-prev-error)
(define-key eglot-mode-map   (kbd "<f12>")   #'xref-find-definitions)
(define-key eglot-mode-map   (kbd "S-<f12>") #'xref-find-references)
;; For some reason, Emacs links C-SPC and C-@ so we have to set both.
(evil-define-key 'insert company-mode-map (kbd "C-@")   #'company-complete)
(evil-define-key 'insert company-mode-map (kbd "C-SPC") #'company-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For some reason, visual-line-mode + disabling truncate lines helps smooth scrolling.
;; (global-visual-line-mode t)
(toggle-truncate-lines 1)
(setq-default scroll-conservatively 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 backup-inhibited t
 ring-bell-function 'ignore)
(auto-save-mode nil)
(global-auto-revert-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq-default
 indent-tabs-mode nil
 fill-column 80
 ;; TODO: Consider changing the default tab width.
 tab-width 2)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDE Features
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'eglot)   ;; Most of the "IDE" stuff.
(require 'company) ;; Autocompletion frontend.
(global-company-mode t)

;; Emacs Lisp
(defun set-up-emacs-lisp-mode ()
  "Set up Emacs Lisp mode."
  (dolist (path load-path)
    (add-to-list 'elisp-flymake-byte-compile-load-path path)))
(add-hook 'emacs-lisp-mode-hook #'set-up-emacs-lisp-mode)

;; Rust
(defun set-up-rust-mode ()
  "Set up Rust mode."
  (eglot-ensure)
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))
(add-hook 'rust-mode-hook #'set-up-rust-mode)

;; Typescript
(defun set-up-typescript-mode ()
  "Set up TypeScript mode."
  (eglot-ensure)
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))
(add-hook 'typescript-mode-hook #'set-up-typescript-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'counsel)
(require 'ivy)
(require 'ivy-rich)
(require 'nerd-icons-ivy-rich)
(ivy-mode t)
(ivy-rich-mode t)
(nerd-icons-ivy-rich-mode t)
(counsel-mode t)
;; Counsel does not set this one by default.
(global-set-key (kbd "C-x b") #'counsel-switch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'diff-hl)
(defun set-up-diff-hl-mode ()
  "Set up diff hl.
Diff HL provides the state (+/-/modified) to the left of the line numbers."
  (diff-hl-flydiff-mode t)
  ;; Margin mode should usually be enabled for terminal. GUI mode automatically
  ;; uses the special "fringe" to display the information.
  (unless (display-graphic-p)
    (diff-hl-margin-mode t)))
(add-hook 'diff-hl-mode-hook #'set-up-diff-hl-mode)
(global-diff-hl-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;; Some custom utilities. Functions that are labeled `(interactive)` can be run
;; with "M-x <name-of-function>"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hm-focus()
  "Helps you focus."
  (interactive)
  (message "Focus... Your dad's here."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init)
;;; init.el ends here
