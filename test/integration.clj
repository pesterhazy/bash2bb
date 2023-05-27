(ns integration
  (:require [clojure.test :refer [deftest is run-tests]]
            [babashka.process :refer [shell]]
            [bash2bb.core :as x]))

(deftest integration
  (let [bash (slurp "integration/envvar")
        bb (str (x/bash->bb bash) "\n" "nil")]
    (is (= (:out (shell {:out :string} "bb" "-e" bb))
           (:out (shell {:out :string} "bash" "-c" bash))))))

(defn -main []
  (run-tests 'integration))
