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

(declare stmt->form)

(defn concat-if-many [xs]
  (if (> (count xs) 1)
    (apply list 'str xs)
    (first xs)))

(defn do-if-many [xs]
  (if (> (count xs) 1)
    (apply list 'do xs)
    (first xs)))

(defn unwrap-arg [{parts "Parts"}]
  (concat-if-many (map (fn [part]
                         (case (get part "Type")
                           ("Lit" "SglQuoted") (get part "Value")
                           "DblQuoted" (unwrap-arg part)
                           "CmdSubst"
                           (let [stmts (-> part (get "Stmts"))]
                             (->> stmts
                                  (map (fn [stmt]
                                         (list :out (update-shell (stmt->form stmt {}) assoc :out :string))))
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
                                 (list 'nth '*command-line-args* (dec idx)))
                               (= "#" var-name)
                               (list 'dec (list 'count '*command-line-args*))
                               :else
                               (do
                                 (swap-state! update :vars (fn [vars] (conj (or vars #{}) (symbol var-name))))
                                 (symbol var-name))))
                           (do
                             (pp part)
                             (throw (Exception. (str "Part Type not implemented: " (get part "Type"))))))) parts)))

(defn builtin [args]
  (case (first args)
    "exit"
    (do
      (assert (= 2 (count args)))
      (template (System/exit ~(Long/parseLong (second args)))))
    "set"
    '(do)
    nil))

(defn- stmt->form [{{type "Type", :as cmd} "Cmd",
                    redirs "Redirs"}
                   {:keys [context] :or {context :stmt}}]
  (assert (<= (count redirs) 2))
  (let [finalize
        (fn [form]
          (if (= :binary context)
            (list 'zero? (list :exit (update-shell form assoc :continue true)))
            form))]
    (case type
      "CallExpr"
      (let [{args "Args", assigns "Assigns"} cmd]
        (cond
          (and (empty? args) (seq assigns))
          (list 'def
                (-> assigns only (get "Name") (get "Value") symbol)
                (-> assigns only (get "Value") unwrap-arg))
          (seq args)
          (let [unwrapped-args (map unwrap-arg args)]
            (or (builtin unwrapped-args)
                (-> (let [opts
                          (reduce
                           (fn [opts redir]
                             (case (get redir "Op")
                               54
                               (assoc opts :out (-> redir (get "Word") (get "Parts") only (get "Value")))
                               56
                               (assoc opts :in (list 'slurp (-> redir (get "Word") (get "Parts") only (get "Value"))))
                               59 ;; StdoutToFileDescriptor
                               (let [target (-> redir (get "Word") unwrap-arg)]
                                 (cond
                                   (and (nil? (get "N" redir))
                                        (= "2" target))
                                   (assoc opts :out 'System/err)
                                   (and (= "2" (-> redir (get "N") (get "Value")))
                                        (= "1" target))
                                   (assoc opts :err 'System/out)
                                   :else
                                   (do
                                     (pp redir)
                                     (throw (Exception. (str "Don't know how to translate redirect"))))))

                               63 ;; here-string
                               (assoc opts :in (-> redir (get "Word") (get "Parts") only (get "Value")))
                               ;; else
                               (do
                                 (pp cmd)
                                 (throw (Exception. (str "Redir Op not implemented: " (get redir "op")))))))
                           {}
                           redirs)]
                      (apply list
                             'shell
                             (into (if (empty? opts) [] [opts])
                                   unwrapped-args)))
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
                    finalize)))
          :else
          (throw (Exception. "Unknown CallExpr"))))
      "BinaryCmd"
      (finalize (let [{op "Op", x "X", y "Y"} cmd]
                  (case op
                    10 ;; &&
                    (list 'and (stmt->form x {:context :binary}) (stmt->form y {}))
                    11 ;; ||
                    (list 'or (stmt->form x {:context :binary}) (stmt->form y {}))
                    12
                    (update-shell (stmt->form y {}) assoc :in (list :out (update-shell (stmt->form x {}) assoc :out :string)))
                    (do
                      (pp cmd)
                      (throw (Exception. (str "BinaryCmd Op not implemented: " op)))))))
      "IfClause"
      (finalize (if (get (get cmd "Else") "Then")
                  (list 'if (stmt->form (only (get cmd "Cond")) {:context :binary})
                        (do-if-many (map #(stmt->form % {}) (get cmd "Then")))
                        (do-if-many (map #(stmt->form % {}) (get (get cmd "Else") "Then"))))
                  (list 'when (stmt->form (only (get cmd "Cond")) {:context :binary})
                        (do-if-many (map #(stmt->form % {}) (get cmd "Then"))))))
      "TestClause"
      (case context
        (:binary :stmt)
        (let [{{type "Type", op "Op", x "X", y "Y"} "X"} cmd]
          (case type
            "BinaryTest"
            (case op
              (40 74) ;; ==
              (list '= (unwrap-arg x) (unwrap-arg y))
              41 ;; !=
              (list 'not= (unwrap-arg x) (unwrap-arg y))
              (do
                (pp cmd)
                (throw (Exception. (str "BinaryTest Op not implemented: " op))))))))
      "Block"
      (let [[:as stmts] (-> cmd (get "Stmts"))]
        (assert (pos? (count stmts)))
        (let [forms (map #(stmt->form % {}) stmts)]
          (apply list 'do (concat (butlast forms) [(finalize (last forms))]))))
      "DeclClause"
      (let [arg (-> cmd (get "Args") only)]
        (assert (= "export" (-> cmd (get "Variant") (get "Value"))))
        (if (-> arg (get "Naked"))
          (template (alter-var-root #'babashka.process/*defaults* (fn [m] (update m :extra-env assoc ~(-> arg (get "Name") (get "Value")) ~(-> arg (get "Name") (get "Value") symbol)))))
          (template
           (do
             (def
               ~(-> arg (get "Name") (get "Value") symbol)
               ~(-> arg (get "Value") unwrap-arg))
             (alter-var-root #'babashka.process/*defaults* (fn [m] (update m :extra-env assoc ~(-> arg (get "Name") (get "Value")) ~(-> arg (get "Name") (get "Value") symbol))))))))
      ;; else
      (do
        (pp cmd)
        (throw (ex-info (str "Cmd type not implemented: " type) {}))))))

(defn ast->forms+state
  [ast]
  (binding [*!state* (atom {})]
    [(mapv #(stmt->form % {}) (get ast "Stmts")) @*!state*]))

(defn ast->forms
  [ast]
  (first (ast->forms+state ast)))

(defn declarations [state]
  (->> (:vars state)
       (map (fn [sym] (list 'def sym (list 'System/getenv (name sym)))))
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
