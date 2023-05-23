(ns bash2bb.core-test
  (:require
   [cheshire.core :as json]
   [clojure.test :refer [deftest is]]))

(defn bla
  []
  (let [stmts (-> (slurp "examples/simple.json")
                  json/parse-string
                  (get "Stmts"))]
    (map
     (fn [v]
       (apply list
              (into ['shell]
                    (map (fn [arg]
                           (-> arg
                               (get "Parts")
                               first ;; this is pretty iffy!
                               (get "Value")))
                         (-> v
                             (get "Cmd")
                             (get "Args"))))))
     stmts)))

(deftest banana
  (is (= [(list 'shell "echo" "one")] (bla))))
