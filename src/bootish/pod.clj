(ns bootish.pod
  (:require [boot.from.backtick :as bt]))

(defmacro with-eval-in
  "Given a pod and an expr, evaluates the body in the pod and returns the
  result to the caller. The body may be a template containing the ~ (unquote)
  and ~@ (unquote-splicing) reader macros. These will be evaluated in the
  calling scope and substituted in the template like the ` (syntax-quote)
  reader macro.
  Note: Unlike syntax-quote, no name resolution is done on the template
  forms.
  Note2: The macro returned value will be nil unless it is printable/readable.
  For instance, returning File objects will not work as they are not printable
  and readable by Clojure."
  [& body]
  `(eval (bt/template (do ~@body))))
