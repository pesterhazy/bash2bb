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

(deftest echo-cmd-subst
  (is (= '[(shell "echo" (:out (shell {:out :string} "echo" "a")))]
         (x/ast->forms (x/bash->ast "echo $(echo a)")))))

(deftest echo-2-parts
  (is (= '[(shell "echo" (str (:out (shell {:out :string} "echo" "a")) "="))]
         (x/ast->forms (x/bash->ast "echo \"$(echo a)=\"")))))

(deftest here-string
  (is (= '[(shell {:in "abc"} "cat")]
         (x/ast->forms (x/bash->ast "cat <<< abc")))))

(deftest param-exp
  (is (= '[(shell "echo" (System/getenv "VAR"))]
         (x/ast->forms (x/bash->ast "echo $VAR")))))

(deftest binary-and
  (is (= '[(and (zero? (:exit (shell {:continue true} "true"))) (shell "echo" "a"))]
         (x/ast->forms (x/bash->ast "true && echo a")))))

(deftest binary-or
  (is (= '[(or (zero? (:exit (shell {:continue true} "true"))) (shell "echo" "a"))]
         (x/ast->forms (x/bash->ast "true || echo a")))))

(deftest conditional
  (is (= '[(if (zero? (:exit (shell {:continue true} "true")))
             (shell "echo" "a")
             (shell "echo" "b"))]
         (x/ast->forms (x/bash->ast "if true; then echo a; else echo b; fi")))))

(deftest conditional-multiple-stmts
  (is (= '[(if (zero? (:exit (shell {:continue true} "true")))
             (do
               (shell "echo" "a")
               (shell "echo" "b"))
             (do
               (shell "echo" "c")
               (shell "echo" "d")))]
         (x/ast->forms (x/bash->ast "if true; then echo a; echo b; else echo c; echo d; fi")))))

(deftest conditional-no-else
  (is (= '[(when (zero? (:exit (shell {:continue true} "true")))
             (shell "echo" "a"))]
         (x/ast->forms (x/bash->ast "if true; then echo a; fi")))))

(deftest conditional-no-else-multiple-stmts
  (is (= '[(when (zero? (:exit (shell {:continue true} "true")))
             (do
               (shell "echo" "a")
               (shell "echo" "b")))]
         (x/ast->forms (x/bash->ast "if true; then echo a; echo b; fi")))))

(deftest conditional-expr-==
  (is (= ['(= "x" "x")] (x/ast->forms (x/bash->ast "[[ x == x ]]")))))

(deftest conditional-expr-!=
  (is (= ['(not= "x" "x")] (x/ast->forms (x/bash->ast "[[ x != x ]]")))))

(deftest conditional-expr-and
  (is (= '[(and (= "x" "x") (shell "echo" "a"))] (x/ast->forms (x/bash->ast "[[ x == x ]] && echo a")))))

(deftest var-assignment
  (is (= '[(def var "a")] (x/ast->forms (x/bash->ast "var=a")))))

(deftest envvar
  (is (= '[(shell {:env {"ENVVAR" "a"}} "bash" "-c" "echo $ENVVAR")]
         (x/ast->forms (x/bash->ast "ENVVAR=a bash -c 'echo $ENVVAR'")))))

(deftest exit
  (is (= '[(System/exit 0)]
         (x/ast->forms (x/bash->ast "exit 0")))))

(deftest block
  (is (= ['(do (shell "echo" "one") (shell "echo" "two"))]
         (x/ast->forms (x/bash->ast "{ echo one; echo two; }")))))

(deftest block-boolean
  (is (= ['(or (do (zero? (:exit (shell {:continue true} "false")))) (shell "echo" "a"))]
         (x/ast->forms (x/bash->ast "{ false; } || echo a")))))

#_(deftest set-builtin
    (is (= [:???]
           (x/ast->forms (x/bash->ast "set -e")))))

;; TODO:
;;
;; blocks: [[ "${DB_STACK_NAME-}" = ""  ]] && { echo >&2 "DB_STACK_NAME has to be set."; exit 1; }
;; vars vs environment vars
;; for loop
;; export
;; set -euo pipefail
;; echo >&2 myerror
;; ( cd xxx; echo $PWD )
