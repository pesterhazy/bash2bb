(ns integration
  (:require
   [babashka.fs :as fs]
   [babashka.process :refer [shell]]
   [bash2bb.core :as x]
   [clojure.test :refer [deftest is run-tests]]))

(deftest integration
  (doseq [fname (fs/glob "integration" "*.bash")]
    (let [bash (slurp (str fname))
          bb (str (x/bash->bb bash) "\n" "nil")]
      (println "•" (fs/file-name fname))
      (is (= (:out (shell {:out :string :extra-env {"WELLKNOWNVAR" "abc"}}
                          "bb" "-e" bb))
             (:out (shell {:out :string :extra-env {"WELLKNOWNVAR" "abc"}}
                          "bash" "-c" bash)))))))

(defn -main []
  (run-tests 'integration))
