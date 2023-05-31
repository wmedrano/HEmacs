;; Dependencies


;; [[file:README.org::*Dependencies][Dependencies:1]]
(custom-set-variables
 '(package-selected-packages
   '(auto-highlight-symbol-mode evil-anzu ivy-emoji htmlize markdown-mode diff-hl company rust-mode which-key magit doom-modeline nerd-icons-ivy-rich ivy-rich counsel ivy typescript-mode eglot atom-one-dark-theme evil yaml-mode)))
;; Dependencies:1 ends here

;; Fonts

;; For icons in the terminal, install a Nerd Font and set it as the terminal's
;; font. Nerd fonts are normal fonts patched to have
;; icons. https://www.nerdfonts.com/font-downloads. To get the font to apply in GUI
;; mode, it must also be set as the default font. This can be edited in the
;; custom-set-faces variable. The default value is "JetBrainsMono Nerd Font" which
;; works fine if you downloaded and installed the "JetBrainsMono Nerd Font"


;; [[file:README.org::*Fonts][Fonts:1]]
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrainsMono Nerd Font" :foundry "JB" :slant normal :weight normal :height 120 :width normal)))))
;; Fonts:1 ends here

;; Package Manager

;; We point our package manager to [[https://melpa.org]]. Melpa contains lots of useful
;; packages.


;; [[file:README.org::*Package Manager][Package Manager:1]]
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; Package Manager:1 ends here

;; Help

;; Which key is used to display a popup of valid keys after a prefix and the
;; functions that they run.

;; To view documentation for a function, use the Emacs built-in keybinding "C-h
;; f" to jump to the documentation for a function and "C-h v" to jump to the
;; documentation for a variable.


;; [[file:README.org::*Help][Help:1]]
(require 'which-key)
(which-key-mode t)
;; Help:1 ends here

;; Theming

;; For theming we use atom dark theme. For the modeline we use doom since it
;; looks pretty. We also remove some very retro stuff and add some basics like
;; the line number.


;; [[file:README.org::*Theming][Theming:1]]
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
;; Theming:1 ends here

;; VIM

;; For keybindings we use evil to provide a VIM like experience. We also add
;; additional keybindings that are similar to VSCode. To learn what a function
;; does, use "C-h f". For example: "C-h f eglot-code-actions RET" will show
;; documentation for the function eglot-code-actions.


;; [[file:README.org::*VIM][VIM:1]]
;; Usually we require first but evil needs to know some of the variables at init
;; time.
(setq-default evil-want-C-u-scroll t)
(require 'evil)
(require 'evil-anzu)
(require 'anzu)
(evil-mode t)
(global-anzu-mode t);; To show number of search matches in modeline.
;; VIM:1 ends here

;; Motion State

;; Evil motion state is similar to normal state but does not allow entering insert
;; mode.


;; [[file:README.org::*Motion State][Motion State:1]]
(add-to-list 'evil-motion-state-modes 'dired-mode)
(add-to-list 'evil-motion-state-modes 'magit-diff-mode)
(add-to-list 'evil-motion-state-modes 'magit-status-mode)
(add-to-list 'evil-motion-state-modes 'special-mode)

(defun hm-evil-define-normal-key-only (key fn)
  "Maps KEY to FN, but only in normal mode.
Motion state will be unbounded."
  (define-key evil-motion-state-map key nil)
  (define-key evil-normal-state-map key fn))
(hm-evil-define-normal-key-only (kbd "RET") #'evil-ret)
;; Motion State:1 ends here

;; VSCode

;; The below are taken from VSCode.


;; [[file:README.org::*VSCode][VSCode:1]]
(require 'eglot)
(context-menu-mode t)  ;; Enables right clicking in GUI mode.
(define-key eglot-mode-map   (kbd "C-.")     #'eglot-code-actions)
(define-key eglot-mode-map   (kbd "<f2>")    #'eglot-rename)
(define-key flymake-mode-map (kbd "<f8>")    #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "S-<f8>")  #'flymake-goto-prev-error)
(define-key eglot-mode-map   (kbd "<f12>")   #'xref-find-definitions)
(define-key eglot-mode-map   (kbd "S-<f12>") #'xref-find-references)
;; Emacs links C-SPC and C-@ so we have to set both.
(evil-define-key 'insert company-mode-map (kbd "C-@")   #'company-complete)
(evil-define-key 'insert company-mode-map (kbd "C-SPC") #'company-complete)
;; VSCode:1 ends here



;; For some reason, visual-line-mode + disabling truncate lines helps smooth scrolling.


;; [[file:README.org::*VSCode][VSCode:2]]
(global-visual-line-mode t)
(toggle-truncate-lines 1)
(setq-default scroll-conservatively 100)
;; VSCode:2 ends here

;; Editing


;; [[file:README.org::*Editing][Editing:1]]
(setq-default
 indent-tabs-mode nil
 fill-column 80
 ;; TODO: Consider changing the default tab width.
 tab-width 2)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-highlight-symbol-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; Editing:1 ends here

;; Files


;; [[file:README.org::*Files][Files:1]]
(setq-default
 backup-inhibited t
 ring-bell-function 'ignore)
(auto-save-mode nil)
(global-auto-revert-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; Files:1 ends here

;; IDE Features

;; IDE features are powered by Eglot. Eglot interacts with LSP servers. Although
;; Eglot interacts with LSP servers, it delegates the frontend to other
;; packages. Those packages are:

;; - Eldoc for displaying documentation for the current symbol in the echo area.
;; - Flymake for displaying syntax errors.
;; - Company for displaying and selecting code completions.
;; - xref for jumping to definitions and references.


;; [[file:README.org::*IDE Features][IDE Features:1]]
(require 'eglot)    ;; Most of the "IDE" stuff.
(require 'company)  ;; Autocompletion frontend.
(global-company-mode t)
;; IDE Features:1 ends here

;; Emacs Lisp


;; [[file:README.org::*Emacs Lisp][Emacs Lisp:1]]
(defun set-up-emacs-lisp-mode ()
  "Set up Emacs Lisp mode."
  (dolist (path load-path)
    (add-to-list 'elisp-flymake-byte-compile-load-path path)))
(add-hook 'emacs-lisp-mode-hook #'set-up-emacs-lisp-mode)
;; Emacs Lisp:1 ends here

;; Rust


;; [[file:README.org::*Rust][Rust:1]]
(defun set-up-rust-mode ()
  "Set up Rust mode."
  (eglot-ensure)
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))
(add-hook 'rust-mode-hook #'set-up-rust-mode)
;; Rust:1 ends here

;; TypeScript


;; [[file:README.org::*TypeScript][TypeScript:1]]
(defun set-up-typescript-mode ()
  "Set up TypeScript mode."
  (eglot-ensure)
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))
(add-hook 'typescript-mode-hook #'set-up-typescript-mode)
;; TypeScript:1 ends here

;; Completions

;; Note: This refers to completions within Emacs, and not within code. Completions
;; within Emacs involves things like selecting a file or buffer.


;; [[file:README.org::*Completions][Completions:1]]
(require 'counsel)
(require 'ivy)
(ivy-mode t)
(counsel-mode t)
 ;; Counsel does not set this one by default.
(global-set-key (kbd "C-x b") #'counsel-switch-buffer)
;; Completions:1 ends here



;; We use some "ivy rich" modes to apply better styling to some of the completion
;; functions. This includes things like adding icons, file size information, and
;; inline documentation.


;; [[file:README.org::*Completions][Completions:2]]
(require 'ivy-rich)
(require 'nerd-icons-ivy-rich)
;; Since we initialize some rich variables later in the file, we must defer
;; enabling the modes.
(add-hook 'after-init-hook #'ivy-rich-mode)
(add-hook 'after-init-hook #'nerd-icons-ivy-rich-mode)
;; Completions:2 ends here

;; Version Control


;; [[file:README.org::*Version Control][Version Control:1]]
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
;; Version Control:1 ends here

;; Focus


;; [[file:README.org::*Focus][Focus:1]]
(defun hm-focus ()
  "Helps you focus."
  (interactive)
  (message "Focus! Your dad's here."))
;; Focus:1 ends here

;; Open File In Chrome

;; ~hm-find-file-in-chrome~ prompts for a file and opens it in chrome.


;; [[file:README.org::*Open File In Chrome][Open File In Chrome:1]]
(defun hm-clone-ivy-display-transformers (src dst)
  "Applies ivy completion styling from function SRC to function DST.
Note: This must be run before the mode is enabled."
  (setq ivy-rich-display-transformers-list
        (plist-put
         ivy-rich-display-transformers-list dst
         (plist-get ivy-rich-display-transformers-list src)))
  (setq nerd-icons-ivy-rich-display-transformers-list
        (plist-put
         nerd-icons-ivy-rich-display-transformers-list dst
         (plist-get nerd-icons-ivy-rich-display-transformers-list src))))

(defun hm-find-file-in-chrome ()
  "Open FILE in Chrome."
  (interactive)
  (counsel--find-file-1 "Open in chrome: " nil #'browse-url-chrome 'hm-find-file-in-chrome))
(hm-clone-ivy-display-transformers 'counsel-find-file 'hm-find-file-in-chrome)
;; Open File In Chrome:1 ends here

;; End

;; Required to signal that this file is providing the ~'init~ package.


;; [[file:README.org::*End][End:1]]
(provide 'init)
;;; init.el ends here
;; End:1 ends here
