
(add-hook 'nxml-mode-hook
          (lambda () (rng-validate-mode 0) )
          t)
