(ns runner
  (:require [cognitect.test-runner]))

(defn -main [& args]
  (apply cognitect.test-runner/-main args))
