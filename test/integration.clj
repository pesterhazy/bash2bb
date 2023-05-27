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
      (is (= (:out (shell {:out :string} "bb" "-e" bb))
             (:out (shell {:out :string} "bash" "-c" bash)))))))

(defn -main []
  (run-tests 'integration))
