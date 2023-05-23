(ns bash2bb.core-test
  (:require
   [cheshire.core :as json]
   [clojure.test :refer [deftest is]]))

(defn stmt->form [stmt]
  (apply list
         (into ['shell]
               (map (fn [arg]
                      (-> arg
                          (get "Parts")
                          first ;; this is pretty iffy!
                          (get "Value")))
                    (-> stmt
                        (get "Cmd")
                        (get "Args"))))))

(defn ast->forms
  [ast]
  (map
   stmt->form
   (get ast "Stmts")))

(deftest echo-one
  (is (= [(list 'shell "echo" "one")] (ast->forms (-> (slurp "examples/simple.json")
                                                      json/parse-string)))))
