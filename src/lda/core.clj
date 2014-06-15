(ns lda.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn read-csv [input-file-path] ; read a data file and returns [[id, word, word-count],...]
  (with-open [input-file (io/reader input-file-path)]
    (doall (map (fn[[id word c]][id word (Integer/valueOf c)])
                (rest (csv/read-csv input-file))))))

(defn unique-words [data]
  (count (set (map (fn[[id word count]] word) data))))

(defn assign-topic [data k]
  (into {} (map #(vector % (int (* (rand) k))) data)))

(defn count-words [f topiced-data] ; this consumes huge time! should be rewritten.
  (->> (filter (fn[[[id word _] topic]](f topic id word)) topiced-data)
       (map (fn[[[_ _ count] _]] count))(apply +)))

(defn update-probs [topic id word alpha beta v topiced-data]
  (let [n_td (count-words (fn[t d w](and (= topic t)(= id d))) topiced-data)
        n_wt (count-words (fn[t d w](and (= topic t)(= word w))) topiced-data)
        n_t  (count-words (fn[t d w](= topic t)) topiced-data)]
    (+ alpha (* n_td (+ beta n_wt) (/ 1 (+ (* beta v) n_t))))))

(defn gibbs [[id word _] alpha beta v k topiced-data] ; collapsed gibbs sampling
  (let [raw-probs (pmap #(update-probs % id word alpha beta v topiced-data) (range k))
        cum-probs (reductions + raw-probs)]
    (count (filter #(< % (rand)) (map #(/ % (last cum-probs)) cum-probs)))))

(defn reassign-topic [alpha beta v k topiced-data i]
  (println (new java.util.Date) "iteration:" (inc i))
  (into {} (pmap (fn[[datum topic]][datum (gibbs datum alpha beta v k topiced-data)]) topiced-data)))

(defn probabilities [k samples]
  (->> (map #(vector % (map (fn[m](get m %)) samples))(keys (first samples)))
       (map (fn[[datum freq]][datum (map (fn[i](float (/ (count (filter #(= % i) freq)) (count samples)))) (range k))]))))

(defn lda [alpha beta k iteration input-file-path]
  (let [data (read-csv input-file-path)
        v (unique-words data)
        samples (rest (reductions (fn[dt i](reassign-topic alpha beta v k dt i))
                                  (assign-topic data k)(range iteration)))]
    (probabilities k samples)))

(defn -main []
  (println (lda (/ 50 5) 0.1 5 50 "./resources/in1_small.csv"))
  (shutdown-agents))
