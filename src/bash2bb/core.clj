(ns bash2bb.core
  (:require
   [backtick :refer [template]]
   [clojure.walk]
   [clojure.pprint :refer [pprint]]
   [babashka.cli :as cli]
   [babashka.process :refer [shell]]
   [cheshire.core :as json]))

(def ^:dynamic *!state* nil)

(defn- swap-state! [& args]
  (apply swap! *!state* args))

(defn fixup
  [v]
  (clojure.walk/prewalk (fn [v]
                          (if-not (map? v)
                            v
                            (-> v
                                (dissoc "Position")
                                (dissoc "Pos")
                                (dissoc "OpPos")
                                (dissoc "End")
                                (dissoc "ValuePos")
                                (dissoc "ValueEnd"))))
                        v))

(defn pp
  [v]
  (pprint (fixup v)))

(defn update-shell [cmd f & args]
  (assert (= 'shell (first cmd)))
  (let [opts (apply f (if (map? (second cmd)) (second cmd) {}) args)
        args (if (map? (second cmd))
               (drop 2 cmd)
               (drop 1 cmd))]
    (if (empty? opts)
      (template (shell ~@args))
      (template (shell ~opts ~@args)))))

(defn only [xs]
  (assert (= 1 (count xs)))
  (first xs))

(declare stmt->forms)

(defn concat-if-many [xs]
  (if (> (count xs) 1)
    (template (str ~@xs))
    (first xs)))

(defn do-if-many [xs]
  (if (> (count xs) 1)
    (template (do ~@xs))
    (first xs)))

(defn unwrap-arg [{parts "Parts"}]
  (concat-if-many (map (fn [part]
                         (case (get part "Type")
                           ("Lit" "SglQuoted") (get part "Value")
                           "DblQuoted" (unwrap-arg part)
                           "CmdSubst"
                           (let [stmts (-> part (get "Stmts"))]
                             (->> stmts
                                  (mapcat #(stmt->forms % {}))
                                  (map (fn [form]
                                         (template (:out ~(update-shell form assoc :out :string)))))
                                  concat-if-many))
                           "ParamExp"
                           (let [var-name (-> part (get "Param") (get "Value"))]
                             #_(when (get part "Exp")
                                 (pp (get part "Exp"))
                                 (assert (= 70 (-> part (get "Exp") (get "Op"))))
                                 (prn (-> part (get "Exp") (get "Word") (get "Parts") only (get "Value"))))
                             (cond
                               (re-matches #"\d+" var-name)
                               (let [idx (Long/parseLong var-name)]
                                 (assert (pos? idx))
                                 (template (nth *command-line-args* ~(dec idx))))
                               (= "#" var-name)
                               '(dec (count *command-line-args*))
                               :else
                               (do
                                 (swap-state! update :vars (fn [vars] (conj (or vars #{}) (symbol var-name))))
                                 (symbol var-name))))
                           (do
                             (pp part)
                             (throw (Exception. (str "Part Type not implemented: " (get part "Type"))))))) parts)))

(defn not-not-found
  "Like clojure.core/not-empty but uses ::not-found as sentinel"
  [coll]
  (if (= ::not-found coll)
    nil
    coll))

(defn builtin [args]
  (case (first args)
    "exit"
    (do
      (assert (= 2 (count args)))
      [(template (System/exit ~(Long/parseLong (second args))))])
    "set"
    []
    ::not-found))

(defn- stmt->forms [{{type "Type", :as cmd} "Cmd",
                     redirs "Redirs"}
                    {:keys [context] :or {context :stmt}}]
  (assert (<= (count redirs) 2))
  (let [finalize
        (fn [form]
          (if (and (= :binary context) (list? form) (= 'shell (first form)))
            (template (zero? (:exit ~(update-shell form assoc :continue true))))
            form))]
    (case type
      "CallExpr"
      (let [{args "Args", assigns "Assigns"} cmd]
        (cond
          (and (empty? args) (seq assigns))
          [(template (def
                       ~(-> assigns only (get "Name") (get "Value") symbol)
                       ~(-> assigns only (get "Value") unwrap-arg)))]
          (seq args)
          (let [unwrapped-args (map unwrap-arg args)]
            (or (not-not-found (builtin unwrapped-args))
                [(-> (let [opts
                           (reduce
                            (fn [opts redir]
                              (case (get redir "Op")
                                54
                                (assoc opts (case (-> redir (get "N") (get "Value"))
                                              (nil "1") :out
                                              "2" :err)
                                       (-> redir (get "Word") (get "Parts") only (get "Value")))
                                56
                                (assoc opts :in (template (slurp ~(-> redir (get "Word") (get "Parts") only (get "Value")))))
                                59 ;; StdoutToFileDescriptor
                                (let [target (-> redir (get "Word") unwrap-arg)]
                                  (cond
                                    (and (nil? (get redir "N"))
                                         (= "2" target))
                                    (assoc opts :out 'System/err)
                                    (and (= "2" (-> redir (get "N") (get "Value")))
                                         (= "1" target))
                                    (assoc opts :err 'System/out)
                                    :else
                                    (assoc opts :out target :err :out)))

                                63 ;; here-string
                                (assoc opts :in (-> redir (get "Word") (get "Parts") only (get "Value")))
                                ;; else
                                (do
                                  (pp redir)
                                  (throw (Exception. (str "Redir Op not implemented: " (get redir "op")))))))
                            {}
                            redirs)]
                       (template (shell
                                  ~@(into (if (empty? opts) [] [opts])
                                          unwrapped-args))))
                     (update-shell (fn [opts]
                                     (reduce (fn [opts assign]
                                               (update opts
                                                       :extra-env
                                                       (fn [env]
                                                         (assoc env
                                                                (-> assign (get "Name") (get "Value"))
                                                                (-> assign (get "Value") unwrap-arg)))))
                                             opts
                                             assigns)))
                     finalize)]))
          :else
          (throw (Exception. "Unknown CallExpr"))))
      "BinaryCmd"
      [(finalize (let [{op "Op", x "X", y "Y"} cmd]
                   (case op
                     10 ;; &&
                     (template (and ~(do-if-many (stmt->forms x {:context :binary})) ~(do-if-many (stmt->forms y {}))))
                     11 ;; ||
                     (template (or ~(do-if-many (stmt->forms x {:context :binary})) ~(do-if-many (stmt->forms y {}))))
                     12 ;; |
                     (update-shell (do-if-many (stmt->forms y {})) assoc :in (template (:out ~(update-shell (do-if-many (stmt->forms x {})) assoc :out :string))))
                     (do
                       (pp cmd)
                       (throw (Exception. (str "BinaryCmd Op not implemented: " op)))))))]
      "IfClause"
      [(finalize (if (get (get cmd "Else") "Then")
                   (template
                    (if ~(do-if-many (stmt->forms (only (get cmd "Cond")) {:context :binary}))
                      ~(do-if-many (mapcat #(stmt->forms % {}) (get cmd "Then")))
                      ~(do-if-many (mapcat #(stmt->forms % {}) (get (get cmd "Else") "Then")))))
                   (template (when ~(do-if-many (stmt->forms (only (get cmd "Cond")) {:context :binary}))
                               ~(do-if-many (mapcat #(stmt->forms % {}) (get cmd "Then")))))))]
      "TestClause"
      [(case context
         (:binary :stmt)
         (let [{{type "Type", op "Op", x "X", y "Y"} "X"} cmd]
           (case type
             "BinaryTest"
             (case op
               (40 74) ;; ==
               (template (= ~(unwrap-arg x) ~(unwrap-arg y)))
               41 ;; !=
               (template (not= ~(unwrap-arg x) ~(unwrap-arg y)))
               (do
                 (pp cmd)
                 (throw (Exception. (str "BinaryTest Op not implemented: " op))))))))]
      "Block"
      [(let [[:as stmts] (-> cmd (get "Stmts"))]
         (assert (pos? (count stmts)))
         (let [forms (mapcat #(stmt->forms % {}) stmts)]
           (if (empty? forms)
             '(do)
             (template (do ~@(concat (butlast forms) [(finalize (last forms))]))))))]
      "DeclClause"
      [(let [arg (-> cmd (get "Args") only)]
         (assert (= "export" (-> cmd (get "Variant") (get "Value"))))
         (if (-> arg (get "Naked"))
           (template (alter-var-root #'babashka.process/*defaults* (fn [m] (update m :extra-env assoc ~(-> arg (get "Name") (get "Value")) ~(-> arg (get "Name") (get "Value") symbol)))))
           (template
            (do
              (def
                ~(-> arg (get "Name") (get "Value") symbol)
                ~(-> arg (get "Value") unwrap-arg))
              (alter-var-root #'babashka.process/*defaults* (fn [m] (update m :extra-env assoc ~(-> arg (get "Name") (get "Value")) ~(-> arg (get "Name") (get "Value") symbol))))))))]
      ;; else
      [(do
         (pp cmd)
         (throw (ex-info (str "Cmd type not implemented: " type) {})))])))

(defn ast->forms+state
  [ast]
  (binding [*!state* (atom {})]
    [(vec (mapcat #(stmt->forms % {}) (get ast "Stmts"))) @*!state*]))

(defn ast->forms
  [ast]
  (first (ast->forms+state ast)))

(defn declarations [state]
  (->> (:vars state)
       (map (fn [sym] (template (def ~sym (System/getenv ~(name sym))))))
       vec))

(defn bash->ast [bash]
  (json/parse-string (:out (shell {:in bash :out :string} "shfmt" "--to-json"))))

(defn preamble []
  '[(require '[babashka.process :refer [shell pipeline pb]])])

(def shebang "#!/usr/bin/env bb\n\n")

(defn bash->bb [bash]
  (let [[forms state] (ast->forms+state (bash->ast bash))]
    (->> (concat (preamble) (declarations state) forms)
         (map prn-str)
         (apply str shebang))))

;; ----------

(def cli-opts {:coerce {:ast :boolean, :zprint :boolean} :args->opts [:file]})

(defn -main [& args]
  (let [cli (cli/parse-opts args cli-opts)]
    (if (:ast cli)
      (pp (bash->ast (slurp (or (:file cli) *in*))))
      (let [bb (bash->bb (slurp (or (:file cli) *in*)))]
        (if (:zprint cli)
          ;; requiring zprint doubles startup time, so load lazily
          (print ((requiring-resolve 'zprint.core/zprint-file-str)
                  bb
                  "script"))
          (print bb))))))
