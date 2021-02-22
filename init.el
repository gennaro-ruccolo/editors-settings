(load "~/.emacs.d/elisp/functions")

; default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

; Make frame transparency overridable
(defvar gr/frame-transparency '(95 . 95))
(defvar gr/font-size 101)


(setq frame-title-format '("%f")) ; set title format for the frame
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq initial-scratch-message "") ; clean the scratch message
(setq-default cursor-type 'bar)   ; make the cursor a vertical bar

(setq display-time-default-load-average nil)
(display-time-mode 1)             ; display time in the modeline
(setq display-time-format "%a %d %b %Y %H:%M")

(setq backup-inhibited t)         ; disable backup
(setq auto-save-default nil)      ; disable autosave

(cua-mode 1)                      ; enable common shortcut Ctrl-X, Crtl-C(cua-mode 1)
(delete-selection-mode 1)         ; override current selection

(scroll-bar-mode -1)        
(tool-bar-mode -1)          
(tooltip-mode -1)           
(menu-bar-mode -1)          

(set-fringe-mode 10)        

(set-face-attribute 'default nil :family "Fira Code" :height gr/font-size) ; font for the frame

(fset 'yes-or-no-p 'y-or-n-p)               ; change yes-or-no to y-n

(column-number-mode)

(global-display-line-numbers-mode t)
(dolist (mode '(dired-mode-hook             ; Disable line numbers for some modes
		org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;set env variable WORKON_HOME 
(setenv "WORKON_HOME"
  (concat
	 "/hdd/Data/Studio/Python/ws/envs"
  )
)

; set frame transparency
(set-frame-parameter (selected-frame) 'alpha gr/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,gr/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;Initialize package sources
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; fix bug gnu repository : it's not necessary for emacs >= 26.3 
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package better-defaults)

(use-package which-key
  :init
  (which-key-mode))

(use-package ace-window
  :diminish
  :config
  (global-set-key (kbd "M-o") 'ace-window)
)

(use-package drag-stuff
  :config
  (drag-stuff-global-mode 1) ; enable drag-staff globally
  (drag-stuff-define-keys)   ; default key bindings 
)

(use-package autopair
  :config
  (autopair-global-mode) 
)

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)      ; undo-tree
  (global-set-key (kbd "C-z") 'undo)
  ; make ctrl-Z redo
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-S-z") 'redo)
)

(use-package company
  :config
  (global-company-mode t)
  ;(setq company-global-modes '(not markdown-mode dired-mode)) ;except for the modes in list
)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1)
)

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1)
)

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1)
)

(use-package zenburn-theme)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(defun gr/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . gr/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
)

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python)
)

(use-package python-black
  :diminish
  :demand t
  :after python-mode
  :commands python-black-on-save-mode
  :init
  (bind-key "C-b" 'python-black-on-save-mode)
)

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1)
)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets zenburn-theme
                        elpy which-key vterm visual-fill-column use-package undo-tree typescript-mode rainbow-delimiters python-mode python-black org-bullets no-littering lsp-ui lsp-ivy ivy-rich ivy-prescient helpful general forge eterm-256color eshell-git-prompt drag-stuff doom-themes doom-modeline dired-single dired-open dired-hide-dotfiles dap-mode counsel-projectile company company-box command-log-mode better-defaults autopair auto-package-update all-the-icons-dired))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
