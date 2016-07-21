(ns number-to-english-phrase.core
  (:gen-class))

(def constants {0 "zero" 1  "one" 2 "two" 3 "three" 4 "four" 
             5 "five" 6 "six" 7 "seven" 8 "eight" 9 "nine" 10 "ten"
             11 "eleven" 12 "twelve" 13 "thirteen" 14 "fourteen"
             15 "fifteen" 16 "sixteen" 17 "seventeen" 18 "eighteen"
             19 "nineteen" 20 "twenty" 30 "thirty" 40 "fourty"
             50 "fifty" 60 "sixty" 70 "seventy" 80 "eighty" 
             90 "ninety" 100 "one hundred" 1000 "one thousand"})

(defn parse-number
  "Parses number to english phrase ranging from 0 to 1000"
  [x]
  (cond 
    (constants? x) (constants x)
    (and (>= x 100) (< x 1000)) (parse-hundredth-position x)
    (and (>= x 10) (< x 100)) (parse-tenth-position x)))

(defn- parse-hundredth-position
  "Parses hundredth position to english phrase"
  [x]
  (str (constants (get-digit x 2)) " hundred" (hundredth-tenth-joiner x)))

(defn- hundredth-tenth-joiner
  "Concatenates hundredth place with tenth place"
  [x]
  (if (and (= 0 (get-digit x 1)) (= 0 (get-digit x 0))) 
    (str "")
    (str " and " (parse-tenth-position x))))

(defn- parse-tenth-position
  "Parses tenth position to english phrase"
  [x]
  (if (constants? (read-string (str (get-digit x 1) (get-digit x 0))))
    (str (constants (read-string (str (get-digit x 1) (get-digit x 0)))))
    (str (constants (* (get-digit x 1) 10)) (tenth-unit-joiner x))))

(defn- tenth-unit-joiner
  "Concatenates tenth place with unit place"
  [x]
  (if (= 0 (get-digit x 0))
    (str "")
    (str "-" (constants (get-digit x 0)))))

(defn- get-digit
  "Retrieves digit from the number for a given position from right to left"
  [x pos]
  (Character/digit (nth (reverse (str x)) pos) 10))

(defn- constants?
  "Checks if the constants has an entry for a given key"
  [x]
  (contains? constants x))

