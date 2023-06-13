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

(deftest cmd-one
  (is (= [(list 'shell "cmd" "one")]
         (x/ast->forms (x/bash->ast "cmd one")))))

(deftest cmd-two
  (is (= [(list 'shell "cmd" "one")
          (list 'shell "cmd" "two")]
         (x/ast->forms (x/bash->ast "cmd one\ncmd two")))))

(deftest cmd-double-quotes
  (is (= [(list 'shell "cmd" "a b")]
         (x/ast->forms (x/bash->ast "cmd \"a b\"")))))

(deftest cmd-single-quotes
  (is (= [(list 'shell "cmd" "a b")]
         (x/ast->forms (x/bash->ast "cmd 'a b'")))))

(deftest cmd-pipe
  (is (= ['(shell {:in (:out (shell {:out :string} "cmd" "ab"))} "rev")]
         (x/ast->forms (x/bash->ast "cmd ab | rev")))))

(deftest cmd-redirect-stdout
  (is (= '[(shell {:out "stdout.txt"} "cmd" "a")]
         (x/ast->forms (x/bash->ast "cmd a > stdout.txt")))))

(deftest cmd-redirect-stderr
  (is (= '[(shell {:err "stderr.txt"} "cmd" "a")]
         (x/ast->forms (x/bash->ast "cmd a 2> stderr.txt")))))

(deftest cmd-redirect-stdin
  (is (= '[(shell {:in (slurp "stdin.txt")} "cat")]
         (x/ast->forms (x/bash->ast "cat < stdin.txt")))))

(deftest cmd-redirect-both
  (is (= '[(shell {:out "both.txt", :err :out} "cmd" "a")]
         (x/ast->forms (x/bash->ast "cmd a >& both.txt")))))

(deftest cmd-redirect-stdout-to-fd
  (is (= ['(shell {:out System/err} "cmd" "a")]
         (x/ast->forms (x/bash->ast "cmd a >&2")))))

(deftest cmd-redirect-stderr-to-stdout
  (is (= ['(shell {:err System/out} "cmd" "a")]
         (x/ast->forms (x/bash->ast "cmd a 2>&1")))))

(deftest cmd-pipe-3
  (is (= ['(shell {:in (:out (shell {:in (:out (shell {:out :string} "cmd" "ab")) :out :string} "cat"))} "rev")]
         (x/ast->forms (x/bash->ast "cmd ab | cat | rev")))))

(deftest cmd-cmd-subst
  (is (= '[(shell "cmd" (:out (shell {:out :string} "cmd" "a")))]
         (x/ast->forms (x/bash->ast "cmd $(cmd a)")))))

(deftest cmd-cmd-subst-2-stmts
  (is (= '[(shell "cmd" (str (:out (shell {:out :string} "cmd" "a"))
                             (:out (shell {:out :string} "cmd" "b"))))]
         (x/ast->forms (x/bash->ast "cmd $(cmd a; cmd b)")))))

(deftest cmd-2-parts
  (is (= '[(shell "cmd" (str (:out (shell {:out :string} "cmd" "a")) "="))]
         (x/ast->forms (x/bash->ast "cmd \"$(cmd a)=\"")))))

(deftest here-string
  (is (= '[(shell {:in "abc"} "cat")]
         (x/ast->forms (x/bash->ast "cat <<< abc")))))

(deftest here-doc
  (is (= '[(shell {:in "hello\nworld\n"} "cat")]
         (x/ast->forms (x/bash->ast "cat <<END\nhello\nworld\nEND")))))

(deftest param-exp
  (is (= '[(shell "cmd" VAR)]
         (x/ast->forms (x/bash->ast "cmd $VAR")))))

(deftest param-dollar-1
  (is (= '[(shell "cmd" (nth *command-line-args* 0))]
         (x/ast->forms (x/bash->ast "cmd $1")))))

(deftest param-dollar-2
  (is (= '[(shell "cmd" (nth *command-line-args* 1))]
         (x/ast->forms (x/bash->ast "cmd $2")))))

(deftest param-dollar-hash
  (is (= '[(shell "cmd" (dec (count *command-line-args*)))]
         (x/ast->forms (x/bash->ast "cmd $#")))))

#_(deftest param-dollar-at
    (is (= '[(apply shell "cmd" *command-line-args*)]
           (x/ast->forms (x/bash->ast "cmd $@")))))

(deftest binary-and
  (is (= '[(and (zero? (:exit (shell {:continue true} "true"))) (shell "cmd" "a"))]
         (x/ast->forms (x/bash->ast "true && cmd a")))))

(deftest binary-or
  (is (= '[(or (zero? (:exit (shell {:continue true} "true"))) (shell "cmd" "a"))]
         (x/ast->forms (x/bash->ast "true || cmd a")))))

(deftest binary-and-3
  (is (= '[(and (and (zero? (:exit (shell {:continue true} "a"))) (shell "b")) (shell "c"))]
         (x/ast->forms (x/bash->ast "a && b && c")))))

(deftest conditional
  (is (= '[(if (zero? (:exit (shell {:continue true} "true")))
             (shell "cmd" "a")
             (shell "cmd" "b"))]
         (x/ast->forms (x/bash->ast "if true; then cmd a; else cmd b; fi")))))

(deftest conditional-multiple-stmts
  (is (= '[(if (zero? (:exit (shell {:continue true} "true")))
             (do
               (shell "cmd" "a")
               (shell "cmd" "b"))
             (do
               (shell "cmd" "c")
               (shell "cmd" "d")))]
         (x/ast->forms (x/bash->ast "if true; then cmd a; cmd b; else cmd c; cmd d; fi")))))

(deftest conditional-no-else
  (is (= '[(when (zero? (:exit (shell {:continue true} "true")))
             (shell "cmd" "a"))]
         (x/ast->forms (x/bash->ast "if true; then cmd a; fi")))))

(deftest conditional-no-else-multiple-stmts
  (is (= '[(when (zero? (:exit (shell {:continue true} "true")))
             (do
               (shell "cmd" "a")
               (shell "cmd" "b")))]
         (x/ast->forms (x/bash->ast "if true; then cmd a; cmd b; fi")))))

(deftest conditional-expr-==
  (is (= ['(= "x" "x")] (x/ast->forms (x/bash->ast "[[ x == x ]]")))))

(deftest conditional-expr-=
  (is (= ['(= "x" "x")] (x/ast->forms (x/bash->ast "[[ x = x ]]")))))

(deftest conditional-expr-!=
  (is (= ['(not= "x" "x")] (x/ast->forms (x/bash->ast "[[ x != x ]]")))))

(deftest conditional-expr-and
  (is (= '[(and (= "x" "x") (shell "cmd" "a"))] (x/ast->forms (x/bash->ast "[[ x == x ]] && cmd a")))))

(deftest var-assignment
  (is (= '[(def var "a")] (x/ast->forms (x/bash->ast "var=a")))))

(deftest var-expansion
  (is (= '[(def var "a") (shell "cmd" var)]
         (x/ast->forms (x/bash->ast "var=a; cmd $var")))))

(deftest envvar
  (is (= '[(shell {:extra-env {"ENVVAR" "a"}} "bash" "-c" "cmd $ENVVAR")]
         (x/ast->forms (x/bash->ast "ENVVAR=a bash -c 'cmd $ENVVAR'")))))

(deftest export-var
  (is (= '[(def VAR "a") (alter-var-root (var babashka.process/*defaults*) (fn [m] (update m :extra-env assoc "VAR" VAR)))] (x/ast->forms (x/bash->ast "VAR=a; export VAR")))))

(deftest export-and-set-var
  (is (= '[(do (def VAR "a") (alter-var-root (var babashka.process/*defaults*) (fn [m] (update m :extra-env assoc "VAR" VAR))))] (x/ast->forms (x/bash->ast "export VAR=a")))))

(deftest exit
  (is (= '[(System/exit 0)]
         (x/ast->forms (x/bash->ast "exit 0")))))

(deftest block
  (is (= ['(do (shell "cmd" "one") (shell "cmd" "two"))]
         (x/ast->forms (x/bash->ast "{ cmd one; cmd two; }")))))

(deftest block-boolean
  (is (= ['(or (do (zero? (:exit (shell {:continue true} "false")))) (shell "cmd" "a"))]
         (x/ast->forms (x/bash->ast "{ false; } || cmd a")))))

(deftest set-builtin
  (is (= []
         (x/ast->forms (x/bash->ast "set -e")))))

(deftest set-builtin-in-block
  (is (= '[(do)]
         (x/ast->forms (x/bash->ast "{ set -e; }")))))

#_(deftest echo
    (is (= '[(println "hi")]
           (x/ast->forms (x/bash->ast "echo hi")))))

#_(deftest echo-n
    (is (= '[(print "hi")]
           (x/ast->forms (x/bash->ast "echo -n hi")))))

#_(deftest var-default
    (is (= [:???]
           (x/ast->forms (x/bash->ast "cmd ${1-mydefault}")))))

;; ----------------------

(deftest has-state
  (is (map? (second (x/ast->forms+state (x/bash->ast ""))))))

(deftest var-remembered-in-state
  (is (= {:vars #{'VAR}} (second (x/ast->forms+state (x/bash->ast "cmd $VAR"))))))

(deftest declarations-none
  (is (= [] (x/declarations {}))))

(deftest declarations-var
  (is (= '[(def VAR (System/getenv "VAR"))] (x/declarations {:vars #{'VAR}}))))

(deftest bash->bb
  (is (= (str x/shebang "(require (quote [babashka.process :refer [shell pipeline pb]]))\n(shell \"cmd\" \"a\")\n")
         (x/bash->bb "cmd a"))))

(deftest bash->bb-var
  (is (= (str x/shebang "(require (quote [babashka.process :refer [shell pipeline pb]]))\n(def VAR (System/getenv \"VAR\"))\n(shell \"cmd\" VAR)\n")
         (x/bash->bb "cmd $VAR"))))

;; TODO:
;;
;; should we use trim-newline on shell output?
;; for loop
;; export VAR=a
;; ( cd xxx; echo $PWD )
