(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp))

;; (eval-after-load 'swank-clojure
;;   '(add-to-list 'swank-clojure-extra-vm-args
;;                 "-agentlib:jdwp=transport=dt_socket,address=8021,server=y,suspend=n"))

(add-hook 'slime-repl-mode-hook 'turn-on-paredit)
(add-hook 'clojure-mode-hook 'turn-on-whitespace)

(setq inferior-lisp-program
      "java -cp /home/phil/src/clojure/clojure.jar clojure.main")

;; unfortunately some codebases use tabs. =(
(set-default 'tab-width 4)
(set-default 'c-basic-offset 2)

(add-hook 'xml-mode-hook 'run-coding-hook)

(eval-after-load 'cc-mode
  '(progn
     (define-key cc-mode-map (kbd "C-M-h") 'backward-kill-word)))
