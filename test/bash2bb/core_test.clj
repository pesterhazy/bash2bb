(ns bash2bb.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [bash2bb.core :as x]))

(deftest t-bash->ast
  (is (= {"Type" "File"} (x/bash->ast ""))))

(deftest empty-ast
  (is (= []
         (x/ast->forms (x/bash->ast "")))))

(deftest echo-one
  (is (= [(list 'shell "echo" "one")]
         (x/ast->forms (x/bash->ast "echo one")))))

(deftest echo-two
  (is (= [(list 'shell "echo" "one")
          (list 'shell "echo" "two")]
         (x/ast->forms (x/bash->ast "echo one\necho two")))))

(deftest echo-pipe
  (is (= '[(-> (pipeline (pb "echo" "ab") (pb {:out :inherit} "rev")))]
         (x/ast->forms (x/bash->ast "echo ab | rev")))))

(deftest echo-redirect
  (is (= [(list 'shell {:out "stdout.txt"} "echo" "a")]
         (x/ast->forms (x/bash->ast "echo a > stdout.txt")))))

#_(deftest echo-pipe-3
    (is (= :???
           (ast->forms (bash->ast "echo ab | cat | rev")))))
