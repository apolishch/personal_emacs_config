(require 'cl)

(setq cfg-var:packages '(
  clojure-mode
  cider
  ruby-mode
  haskell-mode
  inf-ruby
  auto-complete
  ac-nrepl
  paredit
  rainbow-delimiters
  rainbow-mode
  rinari
  js2-mode
  json-mode
  coffee-mode
  haml-mode
  rust-mode
  markdown-mode
  autopair
  smex
  undo-tree
  ace-jump-mode
  ac-js2
  ac-inf-ruby
  ac-html-bootstrap
  ac-math
  ac-html
  multiple-cursors
  powerline
  ))

(defun cfg:install-packages ()
  (let ((pkgs (remove-if #'package-installed-p cfg-var:packages)))
    (when pkgs
      (message "%s" "Emacs refresh packages database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p cfg-var:packages)
        (package-install p)))))

;; Don't autoindent with electric mode
(electric-indent-mode 0)
;; Show line numbers
(global-linum-mode 1)
;; separate line numbers
(setq linum-format "%4d \u2502")

;;Enable Ruby mode for ruby files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist
  '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode))

;;Enable Emacs Lisp mode for elisp files
(add-to-list 'auto-mode-alist
  '("\\(?:\\.emacs\\)\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist
  '("\\.\\(?:el\\)\\'" . emacs-lisp-mode))

;;Enable Haskell mode for haskell files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:hs\\|lhs\\)\\'" . haskell-mode))

;;Enable CoffeeScript mode for coffee files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:coffee\\)\\'" . coffee-mode))

;;Enable javascript mode for js files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:js\\)\\'" . js2-mode))

;;Enable sql mode for sql files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:sql\\)\\'" . sql-mode))

;;Enable json mode for json files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:json\\)\\'" . json-mode))

;;Enable shell mode for shell scripts
(add-to-list 'auto-mode-alist
  '("\\.\\(?:sh\\)\\'" . sh-mode))

;;Enable haml mode for haml files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:haml\\)\\'" . haml-mode))

;;Enable rust mode for rust files
(add-to-list 'auto-mode-alist
  '("\\.\\(?:rs\\)\\'" . rust-mode))

(add-to-list 'auto-mode-alist
  '("\\.\\(?:md\\)\\'" . markdown-mode))

;; Add more package archives
(require 'package)
(add-to-list 'package-archives
  '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Set convenient font and size
(set-default-font "Courier New-13")

;; Pair everything
(autopair-global-mode 1)

;;Lisp hooks
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook (lambda () (autopair-global-mode -1)))
(add-hook 'emasc-lisp-mode-hook (lambda () (autopair-global-mode -1))) 

(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "<em>nrepl</em>")

;; General Auto-Complete
(require 'auto-complete-config)
(setq ac-delay 0.0)
(setq ac-quick-help-delay 0.5)
(ac-config-default)

;; ac-nrepl (Auto-complete for the nREPL)
(require 'ac-nrepl)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'cider-mode)
(add-to-list 'ac-modes 'cider-repl-mode)

;; Popping-up contextual documentation
(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

;; rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Noctilus Theme
(load-theme 'noctilux t)

;; General visual config
(setq frame-title-format "emacs")
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'cursor-type 'hbar)

;; General helpful modes
;; Columns/lines in mode bar
;; highlight close/open parens on hover
;; highlight current line
;; move windows with C-c + direction
;; ido
(column-number-mode 1)
(ido-mode 1)
(line-number-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(winner-mode t)

(windmove-default-keybindings)

;;Replace default shell with smex (requirement for ido). Add a fallback
(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;Auto completion in smex
(ac-config-default)

;; Better undo
(global-undo-tree-mode)
(global-set-key (kbd "M-/") 'undo-tree-visualize)

(global-set-key (kbd "C-c M-.") 'ace-jump-mode)

;;Multi cursors
(global-set-key (kbd "C-c M-[") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c M-]") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-p") 'mc/edit-lines)

;; Powerline
(powerline-center-theme)
(setq powerline-default-separator 'wave)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/")))))

;;Enable System Clipboard

(setq x-select-enable-clipboard t 
      x-select-enable-primary t)


(cfg:install-packages)
