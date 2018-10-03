(ns bootish.util)

(def cyan nil)
(def bold-cyan nil)
(def bold nil)
(def bold-yellow nil)
(def bold-red nil)

(def ^:dynamic *verbosity*
  "Atom containing the verbosity level, 1 is lowest, 3 highest. Level 2
  corresponds to the -v boot option, level 3 to -vv, etc.

  Levels:
    1. Print INFO level messages or higher, colorize and prune stack traces
        for maximum readability.
    2. Print DEBUG level messages or higher, don't colorize stack traces and
        prune some trace elements for improved readablility.
    3. Print DEBUG level messages or higher, don't colorize stack traces and
        include full traces with no pruning."
  (atom 1))

(defn- print*
  [verbosity color args]
  (when (>= @*verbosity* verbosity)
    (binding [*out* *err*]
      (print ((or color identity) (apply format args)))
      (flush))))

(defmacro print**
  "Macro version of boot.util/print* but arguments are only evaluated
  when the message will be printed."
  [verbosity color fmt args]
  `(when (>= @*verbosity* ~verbosity)
     (binding [*out* *err*]
       (print ((or ~color identity) (format ~fmt ~@args)))
       (flush))))

(defmacro trace*
  "Tracing macro, arguments are only evaluated when the message will be
  printed (i.e., verbosity level >= 3)."
  [fmt & args]
  `(print** 3 cyan ~fmt ~args))

(defmacro dbug*
  "Macro version of boot.util/dbug, arguments are only evaluated when the
  message will be printed (i.e., verbosity level >= 2)."
  [fmt & args]
  `(print** 2 bold-cyan ~fmt ~args))

(defmacro info*
  "Macro version of boot.util/info, arguments are only evaluated when
  the message will be printed (i.e., verbosity level >= 1)."
  [fmt & args]
  `(print** 1 bold ~fmt ~args))

(defmacro warn*
  "Macro version of boot.util/warn, arguments are only evaluated when
  the message will be printed (i.e., verbosity level >= 1)."
  [fmt & args]
  `(print** 1 bold-yellow ~fmt ~args))

(defmacro fail*
  "Macro version of boot.util/fail, arguments are only evaluated when
  the message will be printed (i.e., verbosity level >= 1)."
  [fmt & args]
  `(print** 1 bold-red ~fmt ~args))

(defn trace
  "Print TRACE level message. Arguments of the form fmt & args suitable for
  passing to clojure.core/format.
  Note that boot.util/*verbosity* in a pod needs to be altered AFTER pod
  creation or log level won't be affected."
  [& more]
  (print* 3 cyan more))

(defn dbug
  "Print DEBUG level message. Arguments of the form fmt & args suitable for
  passing to clojure.core/format.
  Note that boot.util/*verbosity* in a pod needs to be altered AFTER pod
  creation or log level won't be affected."
  [& more]
  (print* 2 bold-cyan more))

(defn info
  "Print INFO level message. Arguments of the form fmt & args suitable for
  passing to clojure.core/format.
  Note that boot.util/*verbosity* in a pod needs to be altered AFTER pod
  creation or log level won't be affected."
  [& more]
  (print* 1 bold more))

(defn warn
  "Print WARNING level message. Arguments of the form fmt & args suitable for
  passing to clojure.core/format.
  Note that boot.util/*verbosity* in a pod needs to be altered AFTER pod
  creation or log level won't be affected."
  [& more]
  (print* 1 bold-yellow more))

(defmacro with-let
  "Binds resource to binding and evaluates body. Then, returns resource. It's
  a cross between doto and with-open."
  [[binding resource] & body]
  `(let [ret# ~resource ~binding ret#] ~@body ret#))
