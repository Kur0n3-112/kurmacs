;;; CONFIG
(setq custom-file "~/.config/emacs/init.custom") ;; Loads all the custom variable crap on this file
(setq inhibit-startup-message t) ;; Disable the startup Emacs message

(scroll-bar-mode -1) ;; Disabe visible scroolbar
(tool-bar-mode -1)   ;; Disable the toolbar
(tooltip-mode -1)    ;; Disable tooltips
(set-fringe-mode 10) ;; Give some breathing room

(menu-bar-mode -1)   ;; Disable the menu bar

;;Set visible bell
(setq visible-bell t)

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 110)

;;Set spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;Use line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)  ;; Activate relative numbers on lines

;;Set clipboard for term emacs
(when (executable-find "cliphist") ;;Ensure·we're·in·a·Wayland·session·with·cliphist·available

;;·Copy·to·clipboard·using·cliphist
  (defun copy-to-clipboard (start end)
    (interactive "r")
    (let ((selection (buffer-substring-no-properties start end)))
      (call-process-region start end "cliphist" nil nil nil "store")
      (message "Copied to clipboard!")))

;;·Paste·from·clipboard·using·cliphist
  (defun paste-from-clipboard ()
    (interactive)
    (let ((output (shell-command-to-string "cliphist list | head -n 1 | cliphist decode")))
    (insert output)))

;;·Bind·to·keys
(global-set-key (kbd "C-c M-y") 'copy-to-clipboard)
(global-set-key (kbd "C-c M-p") 'paste-from-clipboard))

;;Truncate lines
(setq-default truncate-lines nil)
(setq-default global-visual-line-mode nil)

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil))) ;; for org mode
;;(defun my-c-mode-config ()
;;  "Called in `c-mode-hook'."
;;  (setq truncate-lines nil))
;;(add-hook 'c-mode-hook #'my-c-mode-config)

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

;; Ido for autocompletion
(ido-mode 1)
(ido-everywhere 1)

;;; PACKAGES
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

;; Theming with doom-themes
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

;; Evil mode (vim kebyinds on emacs)
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
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; To change the default state of some buffers
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

(setq org-return-follows-link t)
(evil-set-undo-system 'undo-redo)

;; Enhance evil mode with more human keybinds
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Org-roam config and setup (for taking notes)
(setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

(setq org-roam-capture-templates
       '(("d" "default" plain
          "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n")
          :unnarrowed t)
         ("i" "LearningNotesInternet" plain
          "%?"
          :if-new (file+head "LearningNotesInternet/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+date: %U\n#+STARTUP: inlineimages\n\n- tags :: \n- source :: \n")
          :unnarrowed t)
         ("n" "notebook" plain
          "%?"
          :if-new (file+head "notebook/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+date: %U\n")
          :unnarrowed t)
         ("c" "Ciberseguridad" plain
          "%?"
          :if-new (file+head "Ciberseguridad/General/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+date: %U\n")
          :unnarrowed t)
         ("h" "Hack4U" plain
          "%?"
          :if-new (file+head "Ciberseguridad/Hack4u/IntroduccionHacking/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+date: %U\n#+STARTUP: inlineimages\n")
          :unnarrowed t)
         ("p" "ElRinconDelHacker" plain
          "%?"
          :if-new (file+head "Ciberseguridad/elRinconDelHacker/PreparacionParaLaCertificacionEJPTV2/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}_elrincondelhacker_preparacionescertificacioneejptv2\n#+date: %U\n#+STARTUP: inlineimages\n")
          :unnarrowed t)
         ("l" "linuxThings" plain
          "%?"
          :if-new (file+head "linuxThings/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+date: %U\n")
          :unnarrowed t)
         ("y" "python" plain
          "%?"
          :if-new (file+head "coding/python/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+date: %U\n")
          :unnarrowed t)
         ("j" "python|Django" plain
          "%?"
          :if-new (file+head "coding/python/django/%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+title: ${title}\n#+date: %U\n")
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

;;Org mode configuration
(use-package org
  :config
  (setq org-ellipsis " v")
  (setq org-agenda-files
    '("~/Documentos/org/kurOrgFiles/school.org"
      "~/Documentos/org/kurOrgFiles/ToDo.org"
      "~/Documentos/org/kurOrgFiles/Week.org")))

;;Custom agenda config
    ;;org-hide-emphasis-markers t))
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;;set the tags for counsel-org-tag list
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))
   ("v" "A better agenda view"
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

;; Helpful package to more indeep help utility
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Setup ledger
(use-package ledger-mode
  :ensure t
  :init
  (setq ledger-clear-whole-transactions 1)

  :config
  (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
  :mode "\\.dat\\'")

;; LSP Mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
         (rust-mode . lsp)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
    ;; What to use when checking on-save. "check" is default, I prefer clippy
    (lsp-rust-analyzer-cargo-watch-command "clippy")
    ;;(lsp-eldoc-render-all t)
    (lsp-idle-delay 0.6)
    ;; Enable/disable the hints as you prefer:
    (lsp-inlay-hint-enable t)
    ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
    (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
    (lsp-rust-analyzer-display-chaining-hints t)
    (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
    (lsp-rust-analyzer-display-closure-return-type-hints t)
    (lsp-rust-analyzer-display-parameter-hints nil)
    (lsp-rust-analyzer-display-reborrow-hints nil)
  :commands lsp)

;;> OPTIONALLY
(use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom))
;; if you are helm user
;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;;(use-package which-key
;;    :config
;;    (which-key-mode))


;; Rust mode
(use-package rust-mode)

(use-package rustic ;; Better rust mode
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; Uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; Comment to disable rustfmt on save
  ;;(setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; So that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documentos/03_Code")
    (setq projectile-project-search-path '("~/Documentos/03_Code")))
  (setq projectile-switch-project-action #'projectile-dired))

;;(use-package counsel-projectile
;;    :config(counsel-projectile-mode))

;; Harpoon
(use-package harpoon)
(global-set-key (kbd "C-c a") 'harpoon-quick-menu-hydra)
(global-set-key (kbd "C-c h a") 'harpoon-add-file)

;; And the vanilla commands
(global-set-key (kbd "C-c h f") 'harpoon-toggle-file)
(global-set-key (kbd "C-c h h") 'harpoon-toggle-quick-menu)
(global-set-key (kbd "C-c h c") 'harpoon-clear)
(global-set-key (kbd "C-c h 1") 'harpoon-go-to-1)
(global-set-key (kbd "C-c h 2") 'harpoon-go-to-2)
(global-set-key (kbd "C-c h 3") 'harpoon-go-to-3)
(global-set-key (kbd "C-c h 4") 'harpoon-go-to-4)
(global-set-key (kbd "C-c h 5") 'harpoon-go-to-5)
(global-set-key (kbd "C-c h 6") 'harpoon-go-to-6)
(global-set-key (kbd "C-c h 7") 'harpoon-go-to-7)
(global-set-key (kbd "C-c h 8") 'harpoon-go-to-8)

;; Magit
(use-package magit
    :commands (magit-status magit-get-current-branch)
    :custom
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Python lsp
(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
