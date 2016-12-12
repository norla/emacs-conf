;; Melpa
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; Path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(use-package smyx-theme
  :config
  (load-theme 'smyx t)
  (enable-theme 'smyx))

(use-package default-text-scale)

(use-package hydra
  :config
  (defhydra hydra-git-gutter ()
    "git-gutter"
    ("n" git-gutter:next-hunk "next")
    ("p" git-gutter:previous-hunk "prev")
    ("k" git-gutter:revert-hunk "revert")
    ("s" git-gutter:stage-hunk "stage")
    ("m" magit-status "magit-status" :exit t)
    ("g" git-gutter "refresh")
    ("q" nil "quit" :exit t))
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" default-text-scale-increase "in")
    ("l" default-text-scale-decrease "out"))
  (global-set-key (kbd "<f2>") 'hydra-zoom/body)
  (global-set-key (kbd "<f8>") 'hydra-git-gutter/body)
  )

(use-package ivy
  :bind (
	 ("M-x" . counsel-M-x)
	 ("M-y" . counsel-yank-pop)
	 ("C-x C-f" . counsel-find-file)
	 )
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))


(use-package highlight-symbol
  :config
  (require 's)
  (setq highlight-symbol-idle-delay 0.3)
  (highlight-symbol-mode))

(use-package yasnippet
  :config
  (require 's)
  (yas-global-mode 1)
  (define-key yas-keymap (kbd "<return>") 'yas-next-field)
  ;; Avoid collision with auto-complete TAB by using shift-tab
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand))

;;; Load linum before git gutter to avoid conflict
(global-linum-mode 1)
;; Git gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(use-package hydra
  :config
  (defhydra hydra-git-gutter (global-map "C-h C-g")
    "git-gutter"
    ("n" git-gutter:next-hunk "next")
    ("p" git-gutter:previous-hunk "prev")
    ("k" git-gutter:revert-hunk "revert")
    ("s" git-gutter:stage-hunk "stage")
    ("m" magit-status "magit-status" :exit t)
    ("g" git-gutter "refresh")
    ("q" nil "quit" :exit t)))

;; Autocomplete
(use-package auto-complete-config
  :config
  (global-auto-complete-mode t)
  (ac-config-default)
  (setq-default ac-sources (append ac-sources '(ac-source-filename ac-source-files-in-current-dir)))
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))
  
;; Projectile
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching nil))

(use-package persp-projectile
  :config
  (persp-mode))

;; Fringes if available
(if (fboundp 'set-fringe-mode)
    (set-fringe-mode '(7 . 0)))

(use-package neotree
  :config
  (setq neo-theme 'nerd)
  (setq neo-toggle-window-keep-p t)
  (defun my/neotree-hook (_unused)
    (linum-mode -1))
  (add-hook 'neo-after-create-hook 'my/neotree-hook))


(eval-after-load 'js (define-key js-mode-map (kbd "M-.") nil))

(use-package js2-mode
  :bind (
	 ("M-." . find-tag)
	 )
  :mode ("\\.js" . js2-mode)
  :config
  ;; Js/node
;;  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  
  ;; auto-complete on "dot"
;;  (eval-after-load 'tern
 ;;   '(progn
 ;;      (require 'tern-auto-complete)
   ;;    (tern-ac-setup)))
  
  (require 'flycheck)
  (add-hook 'js2-mode-hook
	    (lambda () (flycheck-mode t)))

  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint javascript-jscs)))
  (eval-after-load 'js (define-key js-mode-map (kbd "M-.") nil))
  ;; Js2 modes error highlighting interferes with jshint/jscs, so we disable it
  (setq
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil))

;; Multiple cursors
(use-package multiple-cursors
  :bind (
	 ("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;; iy-goto-char
(use-package iy-goto-char
  :bind (
	 ("C-c f" . iy-go-to-char)
	 ("C-c F" . iy-go-to-char-backward)
	 ("C-c ;" . iy-go-to-or-up-to-continue)
	 ("C-c ," . iy-go-to-or-up-to-continue-backward)
	 )
  :config
  ;; works better with multi-cursors
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos))

;; pending-delete mode
(pending-delete-mode t)

;; Misc
(global-set-key [C-tab] 'other-window)
(setq column-number-mode t)

;; Keybindings
(global-set-key [s-return] 'counsel-projectile-find-file)
(global-set-key [f1] 'projectile-persp-switch-project)
;;(global-set-key [f2] 'balance-windows-area)
(global-set-key [f3] 'counsel-projectile-ag)
(global-set-key [f9] 'my-neotree-toggle)
(global-set-key [f10] 'my-neotree-project)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode)


(use-package hydra
  :config
  (defhydra hydra-git-gutter (global-map "C-h C-g")
    "git"
    ("n" git-gutter:next-hunk "next")
    ("p" git-gutter:previous-hunk "prev")
    ("k" git-gutter:revert-hunk "revert")
    ("s" git-gutter:stage-hunk "stage")
    ("m" magit-status "magit-status" :exit t)
    ("g" git-gutter "refresh")
    ("q" nil "quit" :exit t)))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(defun my-neotree-toggle ()
  (interactive)
  (progn
    (neotree-toggle)
    (balance-windows)))

(defun my-neotree-project ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root)))
    (if project-dir
	(progn
	  (neotree-dir project-dir)
	  (other-window 1)
	  (balance-windows)
	  ))))

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))

(show-paren-mode 1)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#E0E")
(set-face-attribute 'show-paren-match nil :weight 'bold)

;; TODO: prompt if no exakt match?
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting"
  (interactive)
  (find-tag (find-tag-default)))
(global-set-key (kbd "M-.") 'find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)

(toggle-scroll-bar -1)
(tool-bar-mode -1)
(highlight-symbol-mode 1)
(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

;; Right alt should not be meta key
(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'none))
