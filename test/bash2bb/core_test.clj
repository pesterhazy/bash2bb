(ns bash2bb.core-test
  (:require
   [babashka.process :refer [shell]]
   [cheshire.core :as json]
   [clojure.test :refer [deftest is]]))

(declare stmt->form)

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
      (let [x (map (fn [arg]
                     (-> arg
                         (get "Parts")
                         first ;; this is pretty iffy!
                         (get "Value")))
                   (-> (get cmd "X")
                       (get "Cmd")
                       (get "Args")))
            y (map (fn [arg]
                     (-> arg
                         (get "Parts")
                         first ;; this is pretty iffy!
                         (get "Value")))
                   (-> (get cmd "Y")
                       (get "Cmd")
                       (get "Args")))]
        (list '-> (list 'pipeline
                        (apply list (into ['pb] x))
                        (apply list (into ['pb {:out :inherit}] y))))))))

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

(deftest echo-pipe
  (is (= '[(-> (pipeline (pb "echo" "ab") (pb {:out :inherit} "rev")))]
         (ast->forms (bash->ast "echo ab | rev")))))
