(let ((gc-cons-threshold most-positive-fixnum))
(require 'cl)
  (setq cfg-var:packages '(
                          clj-refactor
                          smartparens
                          idris-mode
			  auctex
                          projectile
			  projectile-rails
                          magit
			  w3
			  ag
			  clojure-mode
			  cider
			  ruby-mode
			  haskell-mode
			  inf-ruby
			  auto-complete
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
			  ido-ubiquitous
			  flx-ido
			  undo-tree
			  ace-jump-mode
			  ac-js2
			  ac-inf-ruby
			  ac-html-bootstrap
			  ac-math
			  ac-html
			  multiple-cursors
			  powerline
			  company-ghc
			  noctilux-theme
			  multi-term
			  midje-mode
			  ))
 ;;new
 (add-hook 'term-mode-hook (lambda () (define-key term-raw-map (kbd "C-z") 'self-insert-command)))
 ;; new end

 ;;new

 (setq js-indent-level 2)
 (setq mac-command-modifier 'super)
 ;;new end
 (defun cfg:install-packages ()
   (let ((pkgs (remove-if #'package-installed-p cfg-var:packages)))
     (when pkgs
       (message "%s" "Emacs refresh packages database...")
       (package-refresh-contents)
       (message "%s" " done.")
       (dolist (p cfg-var:packages)
	 (package-install p)))))
 ;; Add more package archives
 (require 'package)
 (add-to-list 'package-archives
	      '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
 (add-to-list 'package-archives
	      '("melpa" . "http://melpa.milkbox.net/packages/"))
 (package-initialize)
 (custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(company-ghc-show-info t)
  '(package-archives
    (quote
     (("gnu" . "http://elpa.gnu.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")
      ("melpa" . "http://melpa.org/packages/")
      ("melpa-stable" . "http://stable.melpa.org/packages/"))))
  '(package-selected-packages
    (quote
     (flx-ido ido-ubiquitous midje-mode js2-mode auto-complete inf-ruby haskell-mode ruby-mode clojure-mode w3 undo-tree smex rust-mode rinari rainbow-mode rainbow-delimiters powerline paredit noctilux-theme multiple-cursors multi-term markdown-mode json-mode haml-mode company-ghc coffee-mode cider autopair ag ace-jump-mode ac-math ac-js2 ac-inf-ruby ac-html-bootstrap ac-html))))

 (cfg:install-packages)
 ;; Don't autoindent with electric mode
 (electric-indent-mode 0)
 ;; Show line numbers
 (global-linum-mode 1)
 ;; separate line numbers
 (setq linum-format "%4d \u2502")
  ;; subword mode
 (global-subword-mode 1)

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

 ;;Enable Clojure mode for .clj files
 (add-to-list 'auto-mode-alist
	      '("\\.\\(?:\\.clj\\)\\'" . clojure-mode))

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

 ;; Set convenient font and size
 (set-default-font "Courier New-13")

 ;; Pair everything
 (autopair-global-mode 1)

 ;; Company autocomplete for haskell
 (require 'company)
 (add-to-list 'company-backends 'company-ghc)

 ;;Lisp hooks
 (add-hook 'clojure-mode-hook 'eldoc-mode)
 (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
 (add-hook 'clojure-mode-hook 'paredit-mode)
 (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
 (add-hook 'clojure-mode-hook (lambda () (autopair-global-mode -1)))
 (add-hook 'emasc-lisp-mode-hook (lambda () (autopair-global-mode -1)))
 (add-hook 'clojure-mode-hook 'midje-mode) 

 (setq nrepl-popup-stacktraces nil)
 (add-to-list 'same-window-buffer-names "<em>nrepl</em>")

 ;;Haskell hooks
 (add-hook 'haskell-mode-hook (lambda () structured-haskell-mode t))
 (eval-after-load 'haskell-mode '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
 (add-hook 'haskell-mode-hook 'haskell-stylish-mode)
 (add-hook 'haskell-mode-hook 'company-mode)

 ;; General Auto-Complete
 (require 'auto-complete-config)
 (setq ac-delay 0.0)
 (setq ac-quick-help-delay 0.5)
 (ac-config-default)

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
 ;;new
 (require 'paren)
 ;;new end
 (require 'flx-ido)
 (column-number-mode 1)
 (ido-mode 1)
 (ido-everywhere 1)
 (flx-ido-mode 1)
 (setq ido-enable-flex-matching t)
 (setq ido-use-faces nil)
 (setq )
 (line-number-mode 1)
 (show-paren-mode 1)

 (defun my-minibuffer-setup-hook ()
   (setq gc-cons-threshold most-positive-fixnum))

 (defun my-minibuffer-exit-hook ()
   (setq gc-cons-threshold 800000))

 (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
 (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

 ;;new
 (setq show-paren-style 'mixed)
 (set-face-background 'show-paren-match-face "#aaaaaa")
 (set-face-attribute 'show-paren-match-face nil :weight 'bold :underline nil :overline nil :slant 'normal)
 ;;new end
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

 ;;Enable System Clipboard
;; (setq x-select-enable-clipboard t 
;;       x-select-enable-primary t)

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

(require 'erc))
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#emacs" "#clojure-emacs" "#haskell" "#haskell-beginners" "#tasty" "#clojure" "#ruby" "##javascript" "#emacs-beginners" "#clojure-beginners" "#javascript" "#node.js" "#reactjs")))

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"))

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun djcb-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "apolishc" :full-name "Abraham Polishchuk"))))

(global-set-key (kbd "C-c e") 'djcb-erc-start-or-switch)

(require 'projectile)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(projectile-global-mode)











(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "http://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
    (magit projectile-rails projectile auctex idris-mode smartparens golden-ratio clj-refactor flx-ido ido-ubiquitous midje-mode js2-mode auto-complete inf-ruby haskell-mode ruby-mode clojure-mode w3 undo-tree smex rust-mode rinari rainbow-mode rainbow-delimiters powerline paredit noctilux-theme multiple-cursors multi-term markdown-mode json-mode haml-mode company-ghc coffee-mode cider autopair ag ace-jump-mode ac-math ac-js2 ac-inf-ruby ac-html-bootstrap ac-html))))
