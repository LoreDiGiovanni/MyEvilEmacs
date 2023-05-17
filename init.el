(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)            ; Disable the menu bar
;;(setq visible-bell t)

;; Mac os specific settings 
(setq mac-right-option-modifier nil) 
(setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin/")) ;; shell don't have path 
;; line number
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		treemacs-mode-hook
		neotree-mode-hook
		shell-mode-hook
		eshell-mode-hook))
   (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Disable backup
(setq make-backup-files nil)

;; font
(set-face-attribute 'default nil :font "Mononoki Nerd Font" :height 170)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Style
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

(use-package all-the-icons
  :ensure t)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;;keymap
(use-package general
  :config
  (general-evil-setup t))

(nvmap :prefix "SPC"
       "SPC"   '(counsel-M-x :which-key "M-x")
       "h r r" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")) :which-key "Reload emacs config")
       ;;Buffer
       "b b"   '(ibuffer :which-key "Ibuffer")
       "b s"   '(counsel-switch-buffer :which-key "switch buffer list")
       "b k"   '(kill-buffer :which-key "kill buffer list")
       "b c"   '(hydra-buffer-cycle/body :which-key "cycle through buffer")
       ;;File
       "."     '(counsel-find-file :which-key "Find file")
       "d d" '(dired :which-key "Open dired")
       "d j" '(dired-jump :which-key "Dired jump to current")
       ;;Window
       "v s"   '(evil-window-vsplit :which-key "Open vertical split")
       "h s"   '(evil-window-split :which-key "Open horizontal split")
       "w w"   '(evil-window-next :which-key "Move to the next window")
       "w t"   '(shell :which-key "Run terminal")
       ;; text scale
       "t s"   '(hydra-text-scale/body :which-key "Hydra for text scaling")
       ;; project
       "o p"   '(projectile-switch-to-buffer :which-key "Open project")
       "f f"   '(project-find-file :which-key "fuzzy find file in Prj")
       "f s"   '(counsel-projectile-rg :which-key "fuzzy find string in Prj")
       ;; latex
       "l p" '(org-latex-preview :which-key "show latex in line")
       ;;org-agenda
       "o a"   '(org-agenda :which-key "Org agenda")
       "a l" '(org-agenda-list :which-key "Org agenda list")
       "t l"   '(org-tags-view :which-key "Org tasks")
)

;;Ivy
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
  :init (ivy-mode 1)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind ( ("M-x" . counsel-M-x)
	  ("C-x b" . counsel-ibuffer)
	  ("C-x C-f" . counsel-find-file)))

;;which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq wich-key-idle-delay 0.3))

;; Rainbow delimiters 
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;Evil
(use-package evil
  :init      
  (setq evil-want-integration t) 
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer snake))
  (evil-collection-init))

;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
; Make horizontal movement cross lines                                    
(setq-default evil-cross-lines t)

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-buffer-cycle (:timeout 4)
  "cycle buffer"
  ("j" switch-to-next-buffer)
  ("k" switch-to-prev-buffer)
  ("f" nil "finished" :exit t))

(defhydra hydra-resize-split (:timeout 4)
  "resize current split"
  ("j" evil-window-next)
  ("k" enlarge-window)
  ("f" nil "finished" :exit t))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Prj")
    (setq projectile-project-search-path '("~/Prj")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;;Org-mode

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))
(efs/org-font-setup)

(defun efs/org-mode-setup()
  (org-indent-mode)
  ;;(variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis "⤵")
  (setq org-agenda-files
	'("~/Org"))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.70)))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package org-fragtog)

;;Org bable
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
(setq org-src-tab-acts-natively t)
(setq python-indent-guess-indent-offset t)  
(setq python-indent-guess-indent-offset-verbose nil)

(use-package org-alert
  :ensure t
  :custom (alert-default-style 'osx-notifier)
  :config
  (setq org-alert-notification-title "Org agenda!")
  (org-alert-enable))

(setq org-image-actual-width 500)

;;ispell
(setq ispell-program-name "aspell") 
(setq ispell-list-command "list")
(setq ispell-dictionary "italiano")


(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package dashboard
  :init      ;; tweak dashboard config before loading it
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "~/.emacs.d/emacs-dash.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice #'(lambda () (get-buffer-create "*dashboard*")))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-alert dashboard all-the-icons-ibuffer all-the-icons-dired org-fragtog org-bullets counsel-projectile projectile hydra counsel ivy-rich which-key general ivy evil-collection evil doom-modeline all-the-icons doom-themes use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
