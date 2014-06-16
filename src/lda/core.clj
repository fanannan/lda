(ns lda.core
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn read-csv [input-file-path] 
; read a file and returns a data set (vector of datum: [[id, word, word-count],...])
  (with-open [input-file (io/reader input-file-path)]
    (doall (map (fn[[id word c]][id word (Integer/valueOf c)])(rest (csv/read-csv input-file))))))

(defn unique-words [data] 
; remove duplicated words by 'set'.
  (count (set (map (fn[[id word count]] word) data))))

(defn assign-topic [data k] 
; k: num of topics, create a map with pairs of datum and topic number.
  (into {} (map #(vector % (int (* (rand) k))) data)))

(defn count-words [f topiced-data] 
; this consumes huge time! should be rewritten.
  (apply + (map (fn[[[id word count] topic]](if (f topic id word) count 0)) topiced-data)))
  
(defn update-probs [topic id word alpha beta v topiced-data] 
; v: num of unique words
  (let [n_td (dec (count-words (fn[t d w](and (= topic t)(= id d))) topiced-data)),
        n_wt (dec (count-words (fn[t d w](and (= topic t)(= word w))) topiced-data)),
        n_t  (dec (count-words (fn[t d w](= topic t)) topiced-data))]
    (* (+ alpha n_td) (+ beta n_wt) (/ 1 (+ (* beta v) n_t)))))

(defn gibbs [[id word _] alpha beta v k topiced-data] 
; pararelly executes collapsed gibbs sampling and returns topic number.
  (let [raw-probs (pmap #(update-probs % id word alpha beta v topiced-data) (range k))
        cum-probs (reductions + raw-probs)]
    (count (filter #(< % (rand))(map #(/ % (last cum-probs)) cum-probs)))))

(defn reassign-topic [alpha beta v k topiced-data i] 
; pararelly calls gibbs and returns a revised map with pairs of datum and topic number. 
  (println (new java.util.Date) "iteration:" (inc i))
  (into {} (pmap (fn[[datum topic]][datum (gibbs datum alpha beta v k topiced-data)]) topiced-data)))

(defn probabilities [k samples] 
; sum up the number of topics by word and then returns the distribution by word.
  (->> (map #(vector % (map (fn[m](get m %)) samples))(keys (first samples))) ; iteration by datum
       (map (fn[[datum freq]][datum (map (fn[i](float (/ (count (filter #(= % i) freq)) (count samples)))) (range k))]))))

(defn lda [alpha beta k iteration input-file-path]
  (let [data (read-csv input-file-path),
        v (unique-words data),
        samples (rest (reductions (fn[dt i](reassign-topic alpha beta v k dt i)) ; omit the initial values by 'rest'
                                  (assign-topic data k)(range iteration)))]
    (probabilities k samples)))

(defn -main []
  (clojure.pprint/pprint (lda (/ 50 5) 0.1 5 50 "./resources/in1_small.csv"))
  (shutdown-agents))
