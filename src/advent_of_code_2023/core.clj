(ns advent-of-code-2023.core
  (:require [clojure.string :as string]
            [clojure.pprint :as pprint])
  (:gen-class))

(defn clean-input
  [file]
  "Sample input example from day 1. Splits each entry into a separate vector by new line character,
  then splits that into just the numbers in that string"
  (into [] (map #(string/split % #"[a-z]")
                (-> (slurp file)
                    (string/split #"\n")))))


(defn day1a
  [input-file]

  (let [regex-pattern #"[a-z]"
        init-coll (-> (slurp input-file)
                      (string/split #"\n"))
        cleaned (map (fn [x]
                       (-> (string/join (string/split x regex-pattern))
                           (string/split #"")))
                     init-coll)
        _ (clojure.pprint/pprint " ")
        _ (clojure.pprint/pprint (str "cleaned Count: ", (count cleaned)))
        cal-vals (map (fn [x]
                        (let [vec-coll (->> (remove empty? x)
                                            vec)
                              int-coll (into [] (map #(Integer/parseInt %) vec-coll))
                              cal-coll (str (first int-coll) (last int-coll))]
                          (Integer/parseInt cal-coll)))
                      cleaned)]
    (clojure.pprint/pprint (str "cal-vals Count: ", (count cal-vals)))
    (reduce + cal-vals)))

(defn day1b
  [input-file]

  (let [find-pattern (re-pattern "one|two|three|four|five|six|seven|eight|nine")
        replace-pattern {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6" "seven" "7" "eight" "8" "nine" "9"}
        init-coll (-> (slurp input-file)
                      (string/split #"\n"))
        cleaned (map (fn [x]
                       (string/join (remove empty? (-> (string/replace x find-pattern replace-pattern)
                                                       (string/split #"[a-z]")))))
                     init-coll)
        cal-vals (map (fn [x]
                        (let [vec-coll (->> (string/split x #"")
                                            vec)
                              int-coll (into [] (map #(Integer/parseInt %) vec-coll))
                              cal-coll (str (first int-coll) (last int-coll))]
                          (pprint/pprint (str "int-coll: " int-coll ", cal-coll: " cal-coll))
                          (Integer/parseInt cal-coll)))
                      cleaned)]
    (pprint/pprint (count cal-vals))
    (reduce + cal-vals)))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
