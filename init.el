;; Melpa
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; Path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x l") 'counsel-locate)

;; Highlight symbol
(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.3)
(add-hook 'js-mode-hook 'highlight-symbol-mode)
(add-hook 'js2-mode-hook 'highlight-symbol-mode)
(add-hook 'js3-mode-hook 'highlight-symbol-mode)

;; yas-snippets
(require 's)
(require 'yasnippet)
(setq yas-snippet-dirs '("~/dotfiles/snippets"))
(yas-global-mode 1)
(define-key yas-keymap (kbd "<return>") 'yas-next-field)
;; Avoid collision with auto-complete TAB by using shift-tab
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)

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
  (ac-set-trigger-key "<tab>")
  )
  
;; Projectile
(use-package projectile
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching nil)
  )

(use-package persp-projectile
  :config
  (persp-mode)
)

;; Fringes if available
(if (fboundp 'set-fringe-mode)
    (set-fringe-mode '(7 . 0)))

(use-package neotree
  :config
  (setq neo-theme 'nerd)
  (setq neo-toggle-window-keep-p t)
  )

(use-package js2-mode
  :mode ("\\.js" . js2-mode)
  :config
  ;; Js/node
  (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
  
  ;; auto-complete on "dot"
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))
  
  (require 'flycheck)
  (add-hook 'js2-mode-hook
	    (lambda () (flycheck-mode t)))

  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint javascript-jscs)))
  ;; Js2 modes error highlighting interferes with jshint/jscs, so we disable it
  (setq
   js2-mode-show-parse-errors nil
   js2-mode-show-strict-warnings nil)
  )

;; Multiple cursors
(use-package multiple-cursors
  :bind (
	 ("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)
	 ))

;; iy-goto-char
(use-package iy-goto-char
  :bind (
	 ("M-m" . iy-go-to-char)
	 )
  :config
  ;; works better with multi-cursors
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
  )

;; pending-delete mode
(pending-delete-mode t)

;; Misc
(global-set-key [C-tab] 'other-window)
(setq column-number-mode t)

;; Keybindings
(global-set-key [s-return] 'counsel-projectile-find-file)
(global-set-key [f1] 'projectile-persp-switch-project)
(global-set-key [f2] 'balance-windows-area)
(global-set-key [f3] 'counsel-projectile-ag)
(global-set-key [f8] 'magit-status)
(global-set-key [f9] 'my-neotree-toggle)
(global-set-key [f10] 'my-neotree-project)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode)

;; Editorconfig\
(require 'editorconfig)
(editorconfig-mode 1)

(defun my-neotree-toggle ()
  (interactive)
  (progn
    (neotree-toggle)
    (balance-windows)
    ))

(defun my-neotree-project ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root)))
    (if project-dir
	(progn
	  (neotree-dir project-dir)
	  (other-window 1)
	  (balance-windows)
	  )
      )
    ))

;; paren-mode
(require 'paren)

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

;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (smyx)))
 '(custom-safe-themes (quote
    ("9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "c697b65591ba1fdda42fae093563867a95046466285459bd4e686dc95a819310" "f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "5bcd0c26bad3303c0325d12dd6562e4f7892d39d390d7db194dd141ba971cad7" "1e3b2c9e7e84bb886739604eae91a9afbdfb2e269936ec5dd4a9d3b7a943af7f" "18e89f93cbaaac214202142d910582354d36639f21f32b04718ca6425dbc82a2" "0f002f8b472e1a185dfee9e5e5299d3a8927b26b20340f10a8b48beb42b55102" "ed5af4af1d148dc4e0e79e4215c85e7ed21488d63303ddde27880ea91112b07e" "bb15b0004ec895e8b4d5af640cdb3e283cf54271f47ec128dc0ba6af1a333925" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "7bde52fdac7ac54d00f3d4c559f2f7aa899311655e7eb20ec5491f3b5c533fe8" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "1a85b8ade3d7cf76897b338ff3b20409cb5a5fbed4e45c6f38c98eee7b025ad4" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "a233249cc6f90098e13e555f5f5bf6f8461563a8043c7502fb0474be02affeea" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default)))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
	  (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832"))))
 '(fringe-mode (quote (7 . 0)) nil (fringe))
 '(linum-format " %7i ")
 '(nav-width 98)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#E0E")
(set-face-attribute 'show-paren-match nil :weight 'bold)
