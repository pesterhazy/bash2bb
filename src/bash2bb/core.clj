(ns bash2bb.core
  (:require
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

(declare stmt->form)

(defn concat-if-many [xs]
  (if (> (count xs) 1)
    (apply list 'str xs)
    (first xs)))

(defn unwrap-arg [arg]
  (let [parts (-> arg (get "Parts"))]
    (concat-if-many (map (fn [part]
                           (case (get part "Type")
                             ("Lit" "SglQuoted") (get part "Value")
                             "DblQuoted" (unwrap-arg part)
                             "CmdSubst"
                             (let [stmts (-> part (get "Stmts"))]
                               (assert (= 1 (count stmts)))
                               (list :out (update-shell (stmt->form (first stmts)) assoc :out :string)))
                             (throw (ex-info "Unknown arg type" {:type (get part "Type")})))) parts))))

(defn stmt->form [{{type "Type", :as cmd} "Cmd",
                   redirs "Redirs"
                   :as stmt}]
  (assert (<= (count redirs) 2))
  (case type
    "CallExpr"
    (let [; _ (prn (fixup redirs))
          opts
          (cond-> {}
            (= 54 (-> redirs first (get "Op")))
            (assoc :out (-> redirs first (get "Word") (get "Parts") first (get "Value")))
            (= 56 (-> redirs first (get "Op")))
            (assoc :in (list 'slurp (-> redirs first (get "Word") (get "Parts") first (get "Value")))))]
      (apply list
             (into (if (empty? opts) '[shell] ['shell opts])
                   (map unwrap-arg
                        (-> cmd
                            (get "Args"))))))
    "BinaryCmd"
    (do
      (assert (= 12 (get cmd "Op")))
      (let [x (get cmd "X")
            y (get cmd "Y")]
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
