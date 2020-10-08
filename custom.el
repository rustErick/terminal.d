(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'before-save-hook 'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (global-whitespace-mode t)
(show-paren-mode 1)
;; (display-time-mode 1)
;; (display-battery-mode 1)
;; (global-hl-line-mode 1)
(column-number-mode -1)
;; (global-linum-mode t)
(line-number-mode -1)
(winner-mode t)
(global-font-lock-mode 1)
;; (size-indication-mode 1)
(transient-mark-mode 1)
(delete-selection-mode 1)
(global-prettify-symbols-mode t)
(electric-pair-mode t)
(set-fringe-mode 0)

(setq-default speedbar-indentation-width 2)
(setq-default speedbar-show-unknown-files t)
;; (setq-default fringe-mode (quote (1 . 1))
(setq inhibit-startup-message t)
(setq-default display-line-numbers 'relative)
(setq ring-bell-function 'ignore)
(setq require-final-newline t)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-line-function 'insert-tab)
(setq-default js-switch-indent-offset 4)
(setq-default sgml-basic-offset 4)

(setq make-backup-files nil)
;; (setq x-select-enable-clipboard t)
(setq auto-save-default nil)
(setq-default electric-indent-inhibit t)
;; (setq backward-delete-char-untabify-method 'nil)
(setq electric-pair-pairs '(
                            (?\{ . ?\})
                            (?\` . ?\`)
                            (?\[ . ?\])
                            (?\' . ?\')
                            ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dichromacy)))
 '(package-selected-packages
   (quote
	(json-mode rust-mode yasnippet sr-speedbar company restclient emmet-mode indent-guide telephone-line rjsx-mode evil-terminal-cursor-changer evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
