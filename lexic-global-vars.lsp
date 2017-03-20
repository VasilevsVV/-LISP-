;; (eval-when (:compile-toplevel :load-toplevel)
;;   (compile-file "/home/vv/study/-LISP-/lexic-package.lsp"))

(in-package :lexic)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter *path-to-info* "~/study/-LISP-/")

  (defparameter *delimiters* nil)
  (defparameter *mult-delimiters* nil)
  (defparameter *key-words* nil)
  (defparameter *constants* nil)
  (defparameter *identifiers* nil)
  (defparameter *errors* nil)
  (defparameter *position* nil)
  (defparameter *line* nil)
  (defparameter *stream* nil))
