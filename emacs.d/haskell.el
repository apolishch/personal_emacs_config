(require 'company)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

(add-hook 'haskell-mode-hook (lambda () structured-haskell-mode t))
(eval-after-load 'haskell-mode '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(add-hook 'haskell-mode-hook 'haskell-stylish-mode)
(add-hook 'haskell-mode-hook 'company-mode)
