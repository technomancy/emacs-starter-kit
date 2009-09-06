;;; bug271497.el --- For bug report 271497 at Launchpad
(require 'ada-mode)
(require 'mumamo)

(eval-and-compile
  (defun mumamo-chunk-embjava (pos min max)
    "Find JAVA_ON ... JAVA_OFF, return range and java-mode."
    (mumamo-quick-static-chunk pos min max "JAVA_ON" "JAVA_OFF" nil 'java-mode nil))
  )

(define-mumamo-multi-major-mode bug271497-mumamo
  "docstring"
  ("ADA Mode" ada-mode
   (mumamo-chunk-embjava)))
