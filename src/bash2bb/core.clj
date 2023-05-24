(ns bash2bb.core
  (:require
   [babashka.process :refer [shell]]
   [cheshire.core :as json]))

(declare stmt->form)

(defn stmt->form [{{type "Type", :as cmd} "Cmd",
                   redirs "Redirs"
                   :as stmt}]
  (case type
    "CallExpr"
    (do
      #_(prn redirs)
      (apply list
             (into ['shell]
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
