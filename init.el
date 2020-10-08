(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http""https") "://melpa.org/packages/")))
  (add-to-list'package-archives (cons"melpa" url) t))
(when (>= emacs-major-version 24)
  (add-to-list'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; UTF-8
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; USE PACKAGE
 (use-package use-package
   :custom
   (use-package-hook-name-suffix nil)
   (use-package-always-ensure t)
   (use-package-compute-statistics t))

;; EVIL MODE
(use-package evil
  :init (evil-mode 1)
  ;; :hook (after-init . evil-mode)
  :bind
  ("M-m" . evil-normal-state)
  :config
  (define-key evil-normal-state-map (kbd "u") 'undo-tree-visualize)
  (define-key evil-normal-state-map (kbd "c") 'comment-line)
  (define-key evil-normal-state-map (kbd "q") 'beginning-of-line)
  (define-key evil-normal-state-map (kbd "ñ") 'end-of-line)
  (define-key evil-normal-state-map (kbd "+") 'text-scale-increase)
  (define-key evil-normal-state-map (kbd "-") 'text-scale-decrease)
  (define-key evil-normal-state-map (kbd "z l") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "z j") 'evil-join)
  (define-key evil-normal-state-map (kbd "z k") 'kill-current-buffer)
  (define-key evil-normal-state-map (kbd "z n") 'restclient-jump-next)
  (define-key evil-normal-state-map (kbd "z p") 'restclient-jump-prev)
  (define-key evil-visual-state-map (kbd "<tab>") 'indent-region)
  )
(setq evil-search-wrap t
      evil-regexp-search t)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c f") 'make-frame)
(global-set-key (kbd "C-c t") 'shell)
(global-set-key (kbd "C-c p") 'other-frame)
(global-set-key (kbd "C-x f") 'delete-frame)
(global-set-key (kbd "M-k") 'enlarge-window)
(global-set-key (kbd "M-j") 'shrink-window)
(global-set-key (kbd "M-h") 'shrink-window-horizontally)
(global-set-key (kbd "M-l") 'enlarge-window-horizontally)


;; EVIL TERMINAL CURSOR
 (unless (display-graphic-p)
             (require 'evil-terminal-cursor-changer)
             (evil-terminal-cursor-changer-activate) ; or (etcc-on)
             )
(setq evil-motion-state-cursor 'box)  ;; █
(setq evil-visual-state-cursor 'box)  ;; █
(setq evil-normal-state-cursor 'box)  ;; █
(setq evil-insert-state-cursor 'bar)  ;; ⎸
(setq evil-replace-state-cursor 'hbar) ;;_
(setq evil-emacs-state-cursor  'bar)
;; UNDO TREE
(use-package undo-tree
  :defer t
  :commands undo-tree-mode
  :init (global-undo-tree-mode)
  ;; :custom
  ;; (undo-tree-visualizer-diff t)
  ;; (undo-tree-visualizer-timestamps t)
  )
;; INDENT GUIDE
  (indent-guide-global-mode)
;; EMMET
(use-package emmet-mode
  :bind ("M-o" . emmet-expand-line)
  :init
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq-local emmet-expand-jsx-className? t)))
  (add-hook 'sgml-mode-hook
			(lambda ()
			  (setq-local emmet-expand-jsx-className? nil)))
  :hook
  ((html-mode sgml-mode css-mode rjsx-mode). emmet-mode))
;; js IDE
(use-package js2-mode
  :commands js2-mode
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js2-strict-trailing-comma-warning nil)
  (add-hook 'js2-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
			  (setq js-indent-level 4))))
;; react
(use-package rjsx-mode
  :defer t
  :mode "\\.jsx\\'")
;; speedbar
(use-package sr-speedbar
  :bind
  ("C-c o" . sr-speedbar-open)
  ("C-x c" . sr-speedbar-close)
  )

;; indent guide
(indent-guide-global-mode)
;; (set-face-background 'indent-guide-face "dimgray")
;; (setq indent-guide-char ":")

;; mode line
(use-package telephone-line
  :init (telephone-line-mode 1)
  )
;; snippets
(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-snippet-dirs '("~/.emacs.d/private/snippets"))
  ;; :hook
  ;; ((c++-mode) . yas-minor-mode)
  :bind ("C-c n" . yas-new-snippet)
  :config
  (yas-reload-all))
;; rust mode
(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :config (setq rust-format-on-save t))
;; json
(use-package json-mode
  :mode ("\\.json\\'" . json-mode)
  :init
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
			  (setq js-indent-level 4))))
;; restclient http
(use-package restclient
  :bind
  ("C-c s" . restclient-http-send-current)
  ("C-c r" . restclient-http-send-current-raw)
  ("C-c i" . restclient-show-info)
  :mode "\\.http\\'"
  )
;; company
(use-package company
  :diminish company-mode
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  ;; (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "SPC") #'company-abort)
  :hook
  ((rjsx-mode js2-mode sgml-mode html-mode css-mode) . company-mode))
;; org
(load "~/.emacs.d/programing/org.el")
