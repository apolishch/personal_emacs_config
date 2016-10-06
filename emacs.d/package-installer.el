(require 'cl)
(require 'package)

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
  company-ghc
  web-mode
  robe-mode
  impatient-mode))

(defun cfg:install-packages ()
  (let ((pkgs (remove-if #'package-installed-p cfg-var:packages)))
    (when pkgs
      (message "%s" "Emacs refresh packages database...")
      (package-refresh-contents)
      (message "%s" " done.")
      (dolist (p cfg-var:packages)
        (package-install p)))))

(custom-set-variables
  '(package-archives
    (quote
      (("gnu" . "http://elpa.gnu.org.packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa-milkbox-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("melpa-milkbox" . "http://melpa.milkbox.net/packages/")))))

(package-initialize)
(cfg:install-packages)
