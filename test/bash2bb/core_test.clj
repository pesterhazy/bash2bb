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
  (is (= '[(shell {:out "stdout.txt"} "echo" "a")]
         (x/ast->forms (x/bash->ast "echo a > stdout.txt")))))

(deftest echo-redirect-stderr
  (is (= '[(shell {:err "stderr.txt"} "echo" "a")]
         (x/ast->forms (x/bash->ast "echo a 2> stderr.txt")))))

(deftest echo-redirect-stdin
  (is (= '[(shell {:in (slurp "stdin.txt")} "cat")]
         (x/ast->forms (x/bash->ast "cat < stdin.txt")))))

(deftest echo-redirect-stdout-to-fd
  (is (= ['(shell {:out System/err} "echo" "a")]
         (x/ast->forms (x/bash->ast "echo a >&2")))))

(deftest echo-redirect-stderr-to-stdout
  (is (= ['(shell {:err System/out} "echo" "a")]
         (x/ast->forms (x/bash->ast "echo a 2>&1")))))

(deftest echo-pipe-3
  (is (= ['(shell {:in (:out (shell {:in (:out (shell {:out :string} "echo" "ab")) :out :string} "cat"))} "rev")]
         (x/ast->forms (x/bash->ast "echo ab | cat | rev")))))

(deftest echo-cmd-subst
  (is (= '[(shell "echo" (:out (shell {:out :string} "echo" "a")))]
         (x/ast->forms (x/bash->ast "echo $(echo a)")))))

(deftest echo-cmd-subst-2-stmts
  (is (= '[(shell "echo" (str (:out (shell {:out :string} "echo" "a"))
                              (:out (shell {:out :string} "echo" "b"))))]
         (x/ast->forms (x/bash->ast "echo $(echo a; echo b)")))))

(deftest echo-2-parts
  (is (= '[(shell "echo" (str (:out (shell {:out :string} "echo" "a")) "="))]
         (x/ast->forms (x/bash->ast "echo \"$(echo a)=\"")))))

(deftest here-string
  (is (= '[(shell {:in "abc"} "cat")]
         (x/ast->forms (x/bash->ast "cat <<< abc")))))

(deftest param-exp
  (is (= '[(shell "echo" VAR)]
         (x/ast->forms (x/bash->ast "echo $VAR")))))

(deftest param-dollar-1
  (is (= '[(shell "echo" (nth *command-line-args* 0))]
         (x/ast->forms (x/bash->ast "echo $1")))))

(deftest param-dollar-2
  (is (= '[(shell "echo" (nth *command-line-args* 1))]
         (x/ast->forms (x/bash->ast "echo $2")))))

(deftest param-dollar-hash
  (is (= '[(shell "echo" (dec (count *command-line-args*)))]
         (x/ast->forms (x/bash->ast "echo $#")))))

#_(deftest param-dollar-at
    (is (= '[(apply shell "echo" *command-line-args*)]
           (x/ast->forms (x/bash->ast "echo $@")))))

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

(deftest conditional-expr-=
  (is (= ['(= "x" "x")] (x/ast->forms (x/bash->ast "[[ x = x ]]")))))

(deftest conditional-expr-!=
  (is (= ['(not= "x" "x")] (x/ast->forms (x/bash->ast "[[ x != x ]]")))))

(deftest conditional-expr-and
  (is (= '[(and (= "x" "x") (shell "echo" "a"))] (x/ast->forms (x/bash->ast "[[ x == x ]] && echo a")))))

(deftest var-assignment
  (is (= '[(def var "a")] (x/ast->forms (x/bash->ast "var=a")))))

(deftest var-expansion
  (is (= '[(def var "a") (shell "echo" var)]
         (x/ast->forms (x/bash->ast "var=a; echo $var")))))

(deftest envvar
  (is (= '[(shell {:extra-env {"ENVVAR" "a"}} "bash" "-c" "echo $ENVVAR")]
         (x/ast->forms (x/bash->ast "ENVVAR=a bash -c 'echo $ENVVAR'")))))

(deftest export-var
  (is (= '[(def VAR "a") (alter-var-root (var babashka.process/*defaults*) (fn [m] (update m :extra-env assoc "VAR" VAR)))] (x/ast->forms (x/bash->ast "VAR=a; export VAR")))))

(deftest export-and-set-var
  (is (= '[(do (def VAR "a") (alter-var-root (var babashka.process/*defaults*) (fn [m] (update m :extra-env assoc "VAR" VAR))))] (x/ast->forms (x/bash->ast "export VAR=a")))))

(deftest exit
  (is (= '[(System/exit 0)]
         (x/ast->forms (x/bash->ast "exit 0")))))

(deftest block
  (is (= ['(do (shell "echo" "one") (shell "echo" "two"))]
         (x/ast->forms (x/bash->ast "{ echo one; echo two; }")))))

(deftest block-boolean
  (is (= ['(or (do (zero? (:exit (shell {:continue true} "false")))) (shell "echo" "a"))]
         (x/ast->forms (x/bash->ast "{ false; } || echo a")))))

(deftest set-builtin
  (is (= ['(do)]
         (x/ast->forms (x/bash->ast "set -e")))))

#_(deftest var-default
    (is (= [:???]
           (x/ast->forms (x/bash->ast "echo ${1-mydefault}")))))

;; ----------------------

(deftest has-state
  (is (map? (second (x/ast->forms+state (x/bash->ast ""))))))

(deftest var-remembered-in-state
  (is (= {:vars #{'VAR}} (second (x/ast->forms+state (x/bash->ast "echo $VAR"))))))

(deftest declarations-none
  (is (= [] (x/declarations {}))))

(deftest declarations-var
  (is (= '[(def VAR (System/getenv "VAR"))] (x/declarations {:vars #{'VAR}}))))

(deftest bash->bb
  (is (= (str x/shebang "(require (quote [babashka.process :refer [shell pipeline pb]]))\n(shell \"echo\" \"a\")\n")
         (x/bash->bb "echo a"))))

(deftest bash->bb-var
  (is (= (str x/shebang "(require (quote [babashka.process :refer [shell pipeline pb]]))\n(def VAR (System/getenv \"VAR\"))\n(shell \"echo\" VAR)\n")
         (x/bash->bb "echo $VAR"))))

;; TODO:
;;
;; should we use trim-newline on shell output?
;; for loop
;; export VAR=a
;; ( cd xxx; echo $PWD )
