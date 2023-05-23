(ns bash2bb.core-test
  (:require
   [babashka.process :refer [shell]]
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
  (map stmt->form (get ast "Stmts")))

(defn bash->ast [bash]
  (json/parse-string (:out (shell {:in bash :out :string} "shfmt" "--to-json"))))

(deftest t-bash->ast
  (is (= {"Type" "File"} (bash->ast ""))))

(deftest empty-ast
  (is (= []
         (ast->forms (bash->ast "")))))

(deftest echo-one
  (is (= [(list 'shell "echo" "one")]
         (ast->forms (bash->ast "echo one")))))
