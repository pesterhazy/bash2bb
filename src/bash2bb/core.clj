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

(defn unwrap-arg [{parts "Parts"}]
  (concat-if-many (map (fn [part]
                         (case (get part "Type")
                           ("Lit" "SglQuoted") (get part "Value")
                           "DblQuoted" (unwrap-arg part)
                           "CmdSubst"
                           (let [stmts (-> part (get "Stmts"))]
                             (list :out (update-shell (stmt->form (only stmts)) assoc :out :string)))
                           "ParamExp"
                           (list 'System/getenv (-> part (get "Param") (get "Value")))
                           (throw (ex-info "Unknown arg type" {:type (get part "Type")})))) parts)))

(defn stmt->form [{{type "Type", :as cmd} "Cmd",
                   redirs "Redirs"
                   :as stmt}]
  (assert (<= (count redirs) 2))
  (case type
    "CallExpr"
    (let [; _ (prn (fixup redirs))
          opts
          (reduce (fn [opts redir]
                    (case (get redir "Op")
                      54
                      (assoc opts :out (-> redir (get "Word") (get "Parts") only (get "Value")))
                      56
                      (assoc opts :in (list 'slurp (-> redir (get "Word") (get "Parts") only (get "Value"))))
                      63 ;; here-string
                      (assoc opts :in (-> redir (get "Word") (get "Parts") only (get "Value")))))
                  {}
                  redirs)]
      (apply list
             'shell
             (into (if (empty? opts) [] [opts])
                   (map unwrap-arg (-> cmd (get "Args"))))))
    "BinaryCmd"
    (let [{op "Op", x "X", y "Y"} cmd]
      (case op
        10
        (list 'and (list 'zero? (list :exit (update-shell (stmt->form x) assoc :continue true)))
              (stmt->form y))
        12
        (update-shell (stmt->form y) assoc :in (list :out (update-shell (stmt->form x) assoc :out :string)))))))

(defn ast->forms
  [ast]
  (map stmt->form (get ast "Stmts")))

;; ----------

(defn bash->ast [bash]
  (json/parse-string (:out (shell {:in bash :out :string} "shfmt" "--to-json"))))

(defn preamble []
  '[(require '[babashka.process :refer [shell pipeline pb]])])

(defn -main [& args]
  (if (= "--ast" (first args))
    (pp (bash->ast (slurp *in*)))
    (do
      (doseq [form (preamble)]
        (prn form))
      (doseq [form (ast->forms (bash->ast (slurp *in*)))]
        (prn form)))))
