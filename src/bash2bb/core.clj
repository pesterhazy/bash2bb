(ns bash2bb.core)

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
