(ns bash2bb.core
  (:require
   [clojure.walk]
   [clojure.pprint :refer [pprint]]
   [babashka.process :refer [shell]]
   [cheshire.core :as json]))

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
      (apply list 'shell args)
      (apply list 'shell opts args))))

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
                             (list :out (update-shell (stmt->form (only stmts) {}) assoc :out :string)))
                           "ParamExp"
                           (let [var-name (-> part (get "Param") (get "Value"))]
                             (if (= "1" var-name)
                               (list 'nth '*command-line-args* (Long/parseLong var-name))
                               (list 'System/getenv var-name)))
                           (do
                             (pp part)
                             (throw (Exception. (str "Part Type not implemented: " (get part "Type"))))))) parts)))

(defn builtin [args]
  (case (first args)
    "exit"
    (do
      (assert (= 2 (count args)))
      (list 'System/exit (Long/parseLong (second args))))
    "set"
    '(do)
    nil))

(defn stmt->form [{{type "Type", :as cmd} "Cmd",
                   redirs "Redirs"
                   :as stmt}
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
                          (reduce (fn [opts redir]
                                    (case (get redir "Op")
                                      54
                                      (assoc opts :out (-> redir (get "Word") (get "Parts") only (get "Value")))
                                      56
                                      (assoc opts :in (list 'slurp (-> redir (get "Word") (get "Parts") only (get "Value"))))
                                      59 ;; StdoutToFileDescriptor
                                      (do
                                        (let [target (-> redir (get "Word") unwrap-arg)]
                                          (assert (= "2" target))
                                          (assoc opts :out 'System/err)))

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
                                                      :env
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
      ;; else
      (do
        (pp cmd)
        (throw (ex-info (str "Cmd type not implemented: " type) {}))))))

(defn ast->forms
  [ast]
  (map #(stmt->form % {}) (get ast "Stmts")))

(defn bash->ast [bash]
  (json/parse-string (:out (shell {:in bash :out :string} "shfmt" "--to-json"))))

(defn preamble []
  '[(require '[babashka.process :refer [shell pipeline pb]])])

(defn bash->bb [bash]
  (->> (concat (preamble) (ast->forms (bash->ast bash)))
       (map prn-str)
       (apply str)))

;; ----------

(defn -main [& args]
  (if (= "--ast" (first args))
    (pp (bash->ast (slurp *in*)))
    (print (bash->bb (slurp *in*)))))
