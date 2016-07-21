(ns number-to-english-phrase.core-test
  (:require [clojure.test :refer :all]
            [number-to-english-phrase.core :refer :all]))

(deftest parse-number-test
  (is (= "zero" (parse-number 0)))
  (is (= "one" (parse-number 1)))
  (is (= "nine" (parse-number 9)))
  (is (= "ten" (parse-number 10)))
  (is (= "eleven" (parse-number 11)))
  (is (= "seventeen" (parse-number 17)))
  (is (= "twenty" (parse-number 20)))
  (is (= "twenty-one" (parse-number 21)))
  (is (= "thirty" (parse-number 30)))
  (is (= "thirty-seven" (parse-number 37)))
  (is (= "sixty-eight" (parse-number 68)))
  (is (= "seventy-five" (parse-number 75)))
  (is (= "eighty-eight" (parse-number 88)))
  (is (= "ninety" (parse-number 90)))
  (is (= "ninety-nine" (parse-number 99)))
  (is (= "one hundred" (parse-number 100)))
  (is (= "one hundred and one" (parse-number 101)))
  (is (= "one hundred and six" (parse-number 106)))
  (is (= "one hundred and eleven" (parse-number 111)))
  (is (= "two hundred and fifty" (parse-number 250)))
  (is (= "three hundred and eighty-two" (parse-number 382)))
  (is (= "five hundred" (parse-number 500)))
  (is (= "six hundred and one" (parse-number 601)))
  (is (= "seven hundred and seventy-seven" (parse-number 777)))
  (is (= "eight hundred and ninety-one" (parse-number 891)))
  (is (= "nine hundred and ninety-nine" (parse-number 999)))
  (is (= "one thousand" (parse-number 1000)))
  (is (= nil (parse-number 1001)))
  (is (= nil (parse-number -1)))
)

