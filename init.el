(setq inhibit-startup-message t) ;; Disable the startup Emacs message

(scroll-bar-mode -1) ;; Disabe visible scroolbar
(tool-bar-mode -1)   ;; Disable the toolbar
(tooltip-mode -1)    ;; Disable tooltips
(set-fringe-mode 10) ;; Give some breathing room

(menu-bar-mode -1)   ;; Disable the menu bar

;;Set visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 110)

;;Use line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)  ;; Activate relative numbers on lines

;;Truncate lines
(defun my-c-mode-config ()
  "Called in `c-mode-hook'."
  (setq truncate-lines nil))

(add-hook 'c-mode-hook #'my-c-mode-config)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Agenda fancy view
(setq
   ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
   ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
   org-fancy-priorities-list '("🟥" "🟧" "🟨")
   org-priority-faces
   '((?A :foreground "#ff6c6b" :weight bold)
     (?B :foreground "#98be65" :weight bold)
     (?C :foreground "#c678dd" :weight bold))
   org-agenda-block-separator 8411)

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          (tags "customtag"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks marked with customtag:")))

          (agenda "")
          (alltodo "")))))

;; This makes that when you set a TODO item DONE it sets the time when you close the item.
(setq org-log-done 'time)
;;(setq org-agenda-span 'month)  ;; Show a full month

;; See white spaces
;;(setq whitespace-style '(face tabs spaces newline space-mark tab-mark newline-mark))
;;(setq whitespace-style '(face tabs newline space-mark tab-mark newline-mark))
;;(setq whitespace-space-regexp "\\(\u200B\\|[ \t]+\\)")
(global-whitespace-mode 1)

;; Define the whitespace style.
(setq-default whitespace-style
              '(face empty tabs spaces newline trailing space-mark tab-mark newline-mark))

;; Whitespace color corrections.
(require 'color)
(let* ((ws-lighten 30) ;; Amount in percentage to lighten up black.
       (ws-color (color-lighten-name "#3c3836" ws-lighten)))
  (custom-set-faces
   `(whitespace-newline                ((t (:foreground ,ws-color))))
   `(whitespace-missing-newline-at-eof ((t (:foreground ,ws-color))))
   `(whitespace-space                  ((t (:foreground ,ws-color))))
   `(whitespace-space-after-tab        ((t (:foreground ,ws-color))))
   `(whitespace-space-before-tab       ((t (:foreground ,ws-color))))
   `(whitespace-tab                    ((t (:foreground ,ws-color))))
   `(whitespace-trailing               ((t (:foreground ,ws-color))))))

;; Make these characters represent whitespace.
(setq-default whitespace-display-mappings
      '(
        ;; space -> · else .
        (space-mark 32 [183] [46])
        ;; new line -> ¬ else $
        (newline-mark ?\n [172 ?\n] [36 ?\n])
        ;; carriage return (Windows) -> ¶ else #
        (newline-mark ?\r [182] [35])
        ;; tabs -> » else >
        (tab-mark ?\t [187 ?\t] [62 ?\t])))

;; Don't enable whitespace for.
(setq-default whitespace-global-modes
              '(not shell-mode
                    help-mode
                    magit-mode
                    magit-diff-mode
                    ibuffer-mode
                    dired-mode
                    occur-mode))


;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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

;; Enable the lookup from all the keybinds on emacs, for example when using M-x and holding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; For the use of ivy-rich and things like that
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c k" . counsel-rg)
         ("C-c g" . counsel-git))
  :config
  (counsel-mode 1))

(use-package swiper) ;; Optional, but provides `swiper` search

;; Autocompletion on M-x menu 
(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Documentos/org/kurOrgFiles/school.org"
     "/home/kur/Documentos/org/kurOrgFiles/ToDo.org"))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package general)
  :config
  (general-evil-setup t)

  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))
  ;; To change the default state of some buffers
  ;;(evil-set-initial-state 'messages-buffer-mode 'normal)
  ;;(evil-set-initial-state 'dashboard-mode 'normal))
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(setq org-return-follows-link t)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-capture-templates
       '(("d" "default" plain
          "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n")
          :unnarrowed t)
         ("i" "LearningNotesInternet" plain
          "%?"
          :if-new (file+head "LearningNotesInternet/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n#+STARTUP: inlineimages\n\n- tags :: \n- source :: \n")
          :unnarrowed t)
         ("n" "notebook" plain
          "%?"
          :if-new (file+head "notebook/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n")
          :unnarrowed t)
         ("c" "Ciberseguridad" plain
          "%?"
          :if-new (file+head "Ciberseguridad/General/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n")
          :unnarrowed t)
         ("h" "Hack4U" plain
          "%?"
          :if-new (file+head "Ciberseguridad/Hack4u/IntroduccionHacking/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n#+STARTUP: inlineimages\n")
          :unnarrowed t)
         ("p" "ElRinconDelHacker" plain
          "%?"
          :if-new (file+head "Ciberseguridad/elRinconDelHacker/PreparacionParaLaCertificacionEJPTV2/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n#+STARTUP: inlineimages\n")
          :unnarrowed t)
         ("l" "linuxThings" plain
          "%?"
          :if-new (file+head "linuxThings/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n")
          :unnarrowed t)
         ("y" "python" plain
          "%?"
          :if-new (file+head "coding/python/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n")
          :unnarrowed t)
         ("j" "python|Django" plain
          "%?"
          :if-new (file+head "coding/python/django/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n")
          :unnarrowed t)
         ("t" "linuxThingsCommands" plain
          "%?"
          :if-new (file+head "linuxThings/commands/%<%Y%m%d%H%M%S>-${slug}.org"
                             "${title}\n#+date: %U\n#+STARTUP: inlineimages\n")
          :unnarrowed t)))


(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/Documentos/05_Notes/orgRoam"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (org-roam-setup))
