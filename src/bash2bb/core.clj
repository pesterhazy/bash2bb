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

(declare stmt->form)

(defn stmt->form [{{type "Type", :as cmd} "Cmd",
                   redirs "Redirs"
                   :as stmt}]
  (case type
    "CallExpr"
    (let [opts (if redirs
                 ;; HACKITY HACK
                 {:out (-> redirs first (get "Word") (get "Parts") first (get "Value"))}
                 {})]
      (apply list
             (into (if (empty? opts) '[shell] ['shell opts])
                   (map (fn [arg]
                          (-> arg
                              (get "Parts")
                              first ;; this is pretty iffy!
                              (get "Value")))
                        (-> cmd
                            (get "Args"))))))
    "BinaryCmd"
    (do
      (assert (= 12 (get cmd "Op")))
      (let [x (map (fn [arg]
                     (-> arg
                         (get "Parts")
                         first ;; this is pretty iffy!
                         (get "Value")))
                   (-> (get cmd "X")
                       (get "Cmd")
                       (get "Args")))
            y (map (fn [arg]
                     (-> arg
                         (get "Parts")
                         first ;; this is pretty iffy!
                         (get "Value")))
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
