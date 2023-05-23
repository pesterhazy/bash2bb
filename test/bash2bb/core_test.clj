(ns bash2bb.core-test
  (:require
   [cheshire.core :as json]
   [clojure.test :refer [deftest is]]))

(defn bla []
  (let [stmts (-> (slurp "examples/simple.json")
                  json/parse-string
                  (get "Stmts"))]
    (map (fn [v] (map (fn [arg]
                        (-> arg (get "Parts") first (get "Value")))

                      (-> v
                          (get "Cmd")
                          (get "Args"))))
         stmts)))

(deftest banana
  (is (= [["echo" "one"]] (bla))))
