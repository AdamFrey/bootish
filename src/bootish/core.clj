(ns bootish.core
  (:require [bootish.cli :as cli]
            [boot.from.clojure.tools.cli :as tools.cli]
            [bootish.tmpdir :as tmpd]
            [bootish.util :as util]
            [clojure.set :as set]
            [clojure.string :as string])
  (:import [java.util.concurrent ExecutionException]))

(declare ^{:dynamic true :doc "Count of warnings during build."} *warnings*)

(defn- do-cleanup!
  "Cleanup handler. This is run after the build process is complete. Tasks can
  register cleanup callbacks via the cleanup macro below."
  []
  #_(doseq [f @cleanup-fns] (util/guard (f)))
  #_(reset! cleanup-fns []))

;; Defining Tasks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro deftask
  "Define a boot task."
  [sym & forms]
  (let [[heads [bindings & tails]] (split-with (complement vector?) forms)]
    `(do
       (when-let [existing-deftask# (resolve '~sym)]
         (when (= *ns* (-> existing-deftask# meta :ns))
           (let [msg# (delay (format "deftask %s/%s was overridden\n" *ns* '~sym))]
             (bootish.util/warn (if (<= @util/*verbosity* 2)
                                  @msg#
                                  #_(ex/format-exception (Exception. ^String @msg#)))))))
       (cli/defclifn ~(vary-meta sym assoc ::task true)
         ~@heads
         ~bindings
         (let [provided# (->> ~'*opts* keys set)
               optspec#  (->> #'~sym meta :arglists first second)
               allowed#  (->> optspec# :keys (map (comp keyword str)) set)
               unknown#  (set/difference provided# allowed#)]
           (when (seq unknown#)
             (util/warn "%s: unknown option(s): %s\n" '~sym (string/join ", " unknown#))))
         ~@tails))))

;; Boot Lifecycle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def new-fileset (memoize (fn [] {})))

(defn commit!
  "Make the underlying temp directories correspond to the immutable fileset
  tree structure."
  [fileset]
  #_(util/with-semaphore tempdirs-lock
      (tmpd/commit! fileset)))

(defn- sync-user-dirs!
  []
  #_(util/with-semaphore-noblock sync-dirs-lock
    (let [debug-mesg (delay (util/dbug* "Syncing project dirs to temp dirs...\n"))]
      (doseq [[k d] {:asset-paths    (user-asset-dirs)
                     :source-paths   (user-source-dirs)
                     :resource-paths (user-resource-dirs)
                     :checkout-paths @checkout-dirs}]
        @debug-mesg
        (patch! (first d) (get-env k) :ignore @bootignore))
      (util/dbug* "Sync complete.\n"))))

(defn reset-fileset
  "Updates the user directories in the fileset with the latest project files,
  returning a new immutable fileset. When called with no args returns a new
  fileset containing only the latest project files."
  [& [fileset]]
  (new-fileset)
  #_(let [fileset (when fileset (rm fileset (user-files fileset)))]
    (-> (new-fileset)
      (add-user-asset    (first (user-asset-dirs)))
      (add-user-source   (first (user-source-dirs)))
      (add-user-resource (first (user-resource-dirs)))
      (update-in [:tree] merge (:tree fileset))
      (vary-meta merge (meta fileset)))))

(defn- take-subargs [open close [x & xs :as coll]]
  (if (not= x open)
    [nil coll]
    (loop [[x & xs] xs depth 1 ret []]
      (if (not x)
        [ret []]
        (cond (= x open)  (recur xs (inc depth) (conj ret x))
              (= x close) (if (zero? (dec depth))
                            [ret xs]
                            (recur xs (dec depth) (conj ret x)))
              :else       (recur xs depth (conj ret x)))))))

(defn- construct-tasks
  "Given command line arguments (strings), constructs a task pipeline by
  resolving the task vars, calling the task constructors with the arguments
  for that task, and composing them to form the pipeline."
  [ns argv & {:keys [in-order]}]
  (loop [ret [] [op-str & args :as argv] argv]
    (if-not op-str
      (apply comp (filter fn? ret))
      (case op-str
        "--" (recur ret args)
        "["  (let [[argv remainder] (take-subargs "[" "]" argv)]
               (recur (conj ret (construct-tasks argv :in-order false)) remainder))
        (let [op (resolve (symbol (str ns) op-str))]
          (when-not (and op (::task (meta op)))
            (throw (IllegalArgumentException. (format "No such task (%s)" op-str))))
          (let [spec   (:argspec (meta op))
                parsed (tools.cli/parse-opts args spec :in-order in-order)]
            (when (seq (:errors parsed))
              (throw (IllegalArgumentException. (string/join "\n" (:errors parsed)))))
            (let [[opts argv] (if-not in-order
                                [args nil]
                                (#'cli/separate-cli-opts args spec))]
              (recur (conj ret (apply (var-get op) opts)) argv))))))))

(defn- run-tasks
  "Given a task pipeline, builds the initial fileset, sets the initial build
  state, and runs the pipeline."
  [task-stack]
  (binding [*warnings* (atom 0)]
    (let [fs (commit! (reset-fileset))]
      ((task-stack #(do (sync-user-dirs!) %)) fs))))

(defn boot
  "The REPL equivalent to the command line 'boot'. If all arguments are
  strings then they are treated as if they were given on the command line.
  Otherwise they are assumed to evaluate to task middleware."
  [ns & argv]
  (try @(future ;; see issue #6
          (util/with-let [_ nil]
            (run-tasks
              (cond (every? fn? argv)     (apply comp argv)
                    (every? string? argv) (construct-tasks ns argv :in-order true)
                    :else                 (throw (IllegalArgumentException.
                                                   "Arguments must be either all strings or all fns"))))))
       (catch ExecutionException e
         (throw (.getCause e)))
       (finally (do-cleanup!))))

;; Low-Level Tasks, Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-pre-wrap
  "Given a binding and body expressions, constructs a task handler. The body
  expressions are evaluated with the current fileset bound to binding, and the
  result is passed to the next handler in the pipeline. The fileset obtained
  from the next handler is then returned up the handler stack. The body must
  evaluate to a fileset object. Roughly equivalent to:
      (fn [next-handler]
        (fn [binding]
          (next-handler (do ... ...))))
  where ... are the given body expressions."
  [bind & body]
  (let [bind (if (vector? bind) (first bind) bind)]
    `(fn [next-task#]
       (fn [fileset#]
         (assert (tmpd/tmpfileset? fileset#)
           "argument to task handler not a fileset")
         (let [~bind   fileset#
               result# (do ~@body)]
           (assert (tmpd/tmpfileset? result#)
             "task handler must return a fileset")
           (next-task# result#))))))

(defmacro with-pass-thru
  "Given a binding and body expressions, constructs a task handler. The body
  expressions are evaluated for side effects with the current fileset bound
  to binding. The current fileset is then passed to the next handler and the
  result is then returned up the handler stack."
  [bind & body]
  (let [bind (if (vector? bind) (first bind) bind)]
    `(with-pre-wrap [fs#]
       (util/with-let [~bind fs#] ~@body))))

(comment
  (deftask upload-lambda
    [f filename REGEX regex "The file name to match against and upload"]
    (assert filename "argument required:")
    (with-pass-thru fileset
      (prn "hey")))

  (type upload-lambda)

  (use 'clojure.repl)

  (upload-lambda :help true)
  (upload-lambda :filename #"filename")
  (type (upload-lambda :filename #"foo"))
  (boot (upload-lambda :filename #"foo") (upload-lambda :filename #"ok")))

