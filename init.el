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
(set-cursor-color "black")

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

;; EVI MODE
(use-package evil
  :init (evil-mode 1)
  ;; :hook (after-init . evil-mode)
  :bind
  ("M-m" . evil-normal-state)
  :config
  (define-key evil-insert-state-map (kbd "C-r") 'isearch-backward)
  (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
  (define-key evil-normal-state-map (kbd "u") 'undo-tree-visualize)
  (define-key evil-normal-state-map (kbd "c") 'comment-line)
  (define-key evil-normal-state-map (kbd "q") 'beginning-of-line)
  (define-key evil-normal-state-map (kbd "ñ") 'end-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "C-r") 'isearch-backward)
  (define-key evil-normal-state-map (kbd "+") 'evil-scroll-line-up)
  (define-key evil-normal-state-map (kbd "-") 'evil-scroll-line-down)
  (define-key evil-normal-state-map (kbd "g +") 'text-scale-increase)
  (define-key evil-normal-state-map (kbd "g -") 'text-scale-decrease)
  (define-key evil-normal-state-map (kbd "z l") 'switch-to-buffer)
  (define-key evil-normal-state-map (kbd "z j") 'evil-join)
  (define-key evil-normal-state-map (kbd "z k") 'kill-current-buffer)
  (define-key evil-normal-state-map (kbd "z n") 'restclient-jump-next)
  (define-key evil-normal-state-map (kbd "z p") 'restclient-jump-prev)
  (define-key evil-normal-state-map (kbd "z f") 'other-frame)
  (define-key evil-normal-state-map (kbd "TAB") 'indent-region)
  (define-key evil-visual-state-map (kbd "TAB") 'indent-region)
  )
(setq evil-search-wrap t
      evil-regexp-search t)
;; (global-set-key (kbd "C-c p") 'sgml-electric-tag-pair)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c f") 'make-frame)
(global-set-key (kbd "C-c t") 'shell)
(global-set-key (kbd "C-c p") 'other-frame)
(global-set-key (kbd "C-x f") 'delete-frame)
(global-set-key (kbd "M-k") 'enlarge-window)
(global-set-key (kbd "M-j") 'shrink-window)
;; (global-set-key (kbd "M-h") 'shrink-window-horizontally)
(global-set-key (kbd "M-l") 'enlarge-window-horizontally)
(global-set-key (kbd "<dead-acute>") ">")
(global-set-key (kbd "<dead-diaeresis>") "<")

;; EVIL TERMINAL CURSOR
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate) ;; or (etcc-on)
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
;; (setq indent-guide-recursive t)
;; EMMEmT
(use-package emmet-mode
  :bind ("M-o" . emmet-expand-line)
  :init
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq-local emmet-expand-jsx-className? t)))
  (add-hook 'sgml-mode-hook
			(lambda ()
			  (setq-local emmet-expand-jsx-className? nil)))
  )
  ;; :hook
  ;; ((html-mode sgml-mode css-mode rjsx-mode). emmet-mode))
;; js IDE
(use-package js2-mode
  :commands js2-mode
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("\\.js\\'" . js2-mode)
  :init
  (setq js2-basic-offset 4)
  (setq js2-strict-trailing-comma-warning nil)

  (add-hook 'js2-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
			  (setq js-indent-level 4))))
;; react
(use-package rjsx-mode
  :defer t
  :interpreter ("node" . js2-jsx-mode)
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
  :demand t
  :custom (yas-snippet-dirs '("~/.emacs.d/private/snippets"))
  ;; :hook
  ;; ((c++-mode) . yas-minor-mode)
  :hook
  (after-init . yas-global-mode)
  :bind ("C-c n" . yas-new-snippet)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  )
;; :config
;; (yas-reload-all))
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
(global-set-key (kbd "M-h") 'company-capf)
(use-package company
  :diminish company-mode
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  ;; :bind
  ;; (:map company-active-map
  ;; ("<tab>" . company-complete-common-or-cycle))
  :custom
  (company-idle-delay .2)
  (company-tooltip-idle-delay .2)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  :config
  ;; (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "SPC") 'company-abort)
  )
;; org
(load "~/.emacs.d/programing/org.el")
