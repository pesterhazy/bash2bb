(ns bash2bb.core-test
  (:require
   [clojure.test :refer [deftest is]]
   [bash2bb.core :as x]))

(deftest t-bash->ast
  (is (= {"Type" "File"} (x/bash->ast ""))))

(deftest update-shell-no-change
  (is (= '(shell "cat") (x/update-shell '(shell "cat") identity))))

(deftest update-shell-add-out
  (is (= '(shell {:out :string} "cat") (x/update-shell '(shell "cat") assoc :out :string))))

(deftest update-shell-no-change-with-opt
  (is (= '(shell {:out :string} "cat") (x/update-shell '(shell {:out :string} "cat") identity))))

(deftest update-shell-remove-out
  (is (= '(shell "cat") (x/update-shell '(shell {:out :string} "cat") dissoc :out))))

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

(deftest echo-double-quotes
  (is (= [(list 'shell "echo" "a b")]
         (x/ast->forms (x/bash->ast "echo \"a b\"")))))

(deftest echo-single-quotes
  (is (= [(list 'shell "echo" "a b")]
         (x/ast->forms (x/bash->ast "echo 'a b'")))))

(deftest echo-pipe
  (is (= ['(shell {:in (:out (shell {:out :string} "echo" "ab"))} "rev")]
         (x/ast->forms (x/bash->ast "echo ab | rev")))))

(deftest echo-redirect-stdout
  (is (= [(list 'shell {:out "stdout.txt"} "echo" "a")]
         (x/ast->forms (x/bash->ast "echo a > stdout.txt")))))

(deftest echo-redirect-stdin
  (is (= '[(shell {:in (slurp "stdin.txt")} "cat")]
         (x/ast->forms (x/bash->ast "cat < stdin.txt")))))

(deftest echo-pipe-3
  (is (= ['(shell {:in (:out (shell {:in (:out (shell {:out :string} "echo" "ab")) :out :string} "cat"))} "rev")]
         (x/ast->forms (x/bash->ast "echo ab | cat | rev")))))

#_(deftest echo-cmd-subst
    (is (= :???
           (x/ast->forms (x/bash->ast "echo $(echo a)")))))
