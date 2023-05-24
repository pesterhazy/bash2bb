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

(declare stmt->form)

(defn unwrap-arg [arg]
  (-> arg
      (get "Parts")
      first ;; this is pretty iffy!
      (get "Value")))

(defn stmt->form [{{type "Type", :as cmd} "Cmd",
                   redirs "Redirs"
                   :as stmt}]
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
      (let [x (map unwrap-arg
                   (-> (get cmd "X")
                       (get "Cmd")
                       (get "Args")))
            y (map unwrap-arg
                   (-> (get cmd "Y")
                       (get "Cmd")
                       (get "Args")))]
        (list '-> (list 'pipeline
                        (apply list (into ['pb] x))
                        (apply list (into ['pb {:out :inherit}] y))))))))

(defn ast->forms
  [ast]
  (map stmt->form (get ast "Stmts")))

(defn bash->ast [bash]
  (json/parse-string (:out (shell {:in bash :out :string} "shfmt" "--to-json"))))

(defn preamble []
  '[(require '[babashka.process :refer [shell pipeline pb]])])

(defn -main [& _args]
  (doseq [form (preamble)]
    (prn form))
  (doseq [form (ast->forms (bash->ast (slurp *in*)))]
    (prn form)))
