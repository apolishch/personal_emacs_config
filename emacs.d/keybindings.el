(global-set-key (kbd "C-c M-[") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c M-]") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-p") 'mc/edit-lines)

(global-set-key (kbd "C-c M-s") 'ace-jump-mode)
(global-set-key (kbd "M-/") 'undo-tree-visualize)

(global-unset-key (kbd "M-x"))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key [f2] 'comment-region)
(global-set-key [f3] 'uncomment-region)
(global-set-key [f4] 'indent-region)
