(ns bash2bb.core-test
  (:require
   [babashka.process :refer [shell]]
   [cheshire.core :as json]
   [clojure.test :refer [deftest is]]))

(defn cmd->form [{type "Type", :as cmd}]
  (case type
    "CallExpr"
    (apply list
           (into ['shell]
                 (map (fn [arg]
                        (-> arg
                            (get "Parts")
                            first ;; this is pretty iffy!
                            (get "Value")))
                      (-> cmd
                          (get "Args")))))
    "BinaryCmd"
    (do
      (assert (= 12 (get cmd "Op")))
      nil)))

(defn stmt->form [{cmd "Cmd"}]
  (cmd->form cmd))

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

(deftest echo-two
  (is (= [(list 'shell "echo" "one")
          (list 'shell "echo" "two")]
         (ast->forms (bash->ast "echo one\necho two")))))

#_(deftest echo-pipe
    (clojure.pprint/pprint (bash->ast "echo ab | rev"))
    (is (= :foo
           (ast->forms (bash->ast "echo ab | rev")))))
