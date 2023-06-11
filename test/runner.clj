(ns runner
  (:require [babashka.fs :as fs]
            [clojure.stacktrace :as stacktrace]
            [clojure.string :as str]
            [clojure.test :as test]
            [cognitect.test-runner]
            [sci.core :as sci]))

(defmacro try-expr
  "Used by the 'is' macro to catch unexpected exceptions.
  You don't call this."
  {:added "1.1"}
  [msg form]
  `(try ~(clojure.test/assert-expr msg form)
        (catch ~(with-meta 'Exception {:sci/error true}) t#
          (clojure.test/do-report {:type :error, :message ~msg,
                                   :expected '~form, :actual t#}))))

(alter-var-root #'clojure.test/try-expr (constantly @#'try-expr))

(defn right-pad [s n]
  (let [n (- n (count s))]
    (str s (str/join (repeat n " ")))))

(defn format-stacktrace [st]
  (let [st (force st)
        data (keep (fn [{:keys [:file :ns :line :column :sci/built-in
                                :local]
                         nom :name}]
                     (when (or line built-in)
                       {:name (str (if nom
                                     (str ns "/" nom)
                                     ns)
                                   (when local
                                     (str "#" local)))
                        :loc (str (or (some->> file (fs/relativize (fs/cwd)))
                                      (if built-in
                                        "<built-in>"
                                        "<expr>"))
                                  (when line
                                    (str ":" line ":" column)))}))
                   st)
        max-name (reduce max 0 (map (comp count :name) data))]
    (mapv (fn [{:keys [:name :loc]}]
            (str (right-pad name max-name) " - " loc))
          data)))

(defn testing-vars-str
  [m]
  (let [{:keys [file line]} m]
    (str
     (reverse (map #(:name (meta %)) clojure.test/*testing-vars*))
     " (" (when (and file (not= "<expr>" file))
            (fs/relativize (fs/cwd) file)) ":" line ")")))

(defn print-stack-trace [e]
  (stacktrace/print-throwable (.getCause e))
  (newline)
  (->> e
       (sci/stacktrace)
       (format-stacktrace)
       (run! println)))

(defn report-error [m]
  (test/inc-report-counter :error)
  (println "\nERROR in" (testing-vars-str m))
  (when-let [message (:message m)] (println message))
  (println "expected:" (pr-str (:expected m)))
  (print "  actual: ")
  (let [actual (:actual m)]
    (if (instance? Throwable actual)
      (if (= :sci/error (-> actual ex-data :type))
        (print-stack-trace actual)
        (clojure.stacktrace/print-cause-trace actual))
      (prn actual))))

(defn with-error-reporting [f]
  (let [original-report clojure.test/report]
    (with-redefs [clojure.test/report
                  (fn [event]
                    (if (and (= :error (:type event))
                             (instance? Throwable (:actual event)))
                      (report-error event)
                      (original-report event)))]
      (f))))

(defn -main [& args]
  (with-error-reporting (fn [] (apply cognitect.test-runner/-main args))))
