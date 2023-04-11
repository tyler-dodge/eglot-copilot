;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'org)
(require 'el-mock)

(when (require 'undercover nil t)
  (undercover "*.el"))
(require 'eglot-copilot (expand-file-name "eglot-copilot.el"))

(ert-deftest eglot-copilot-exists ()
  "Sanity check to make sure expected symbols are exported."
  (should (functionp 'eglot-copilot-setup)))

(provide 'eglot-copilot-test)
