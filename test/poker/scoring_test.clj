(ns poker.scoring_test
	(:require [clojure.test :refer :all]
	[poker.scoring :refer :all]))

;; auto-generated by testgen - see https://github.com/simon-brooke/testgen

(deftest
 test-position
 (testing
  "position"
  (is (thrown? clojure.lang.ArityException (position nil)))
  (is (thrown? clojure.lang.ArityException (position ())))
  (is (thrown? clojure.lang.ArityException (position '(a :b "c"))))
  (is (thrown? clojure.lang.ArityException (position true)))
  (is (thrown? clojure.lang.ArityException (position "test")))
  (is (thrown? clojure.lang.ArityException (position :test)))
  (is (thrown? clojure.lang.ArityException (position 0)))
  (is
   (thrown? clojure.lang.ArityException (position Integer/MAX_VALUE)))
  (is (thrown? clojure.lang.ArityException (position 22/7)))
  (is (thrown? clojure.lang.ArityException (position 1.0E-4)))
  (is (thrown? clojure.lang.ArityException (position -1.0E-4)))
  (is (thrown? clojure.lang.ArityException (position ranks)))
  (is (thrown? clojure.lang.ArityException (position suits)))
  (is (thrown? clojure.lang.ArityException (position five-kind-hand)))
  (is (thrown? clojure.lang.ArityException (position four-kind-hand)))
  (is (thrown? clojure.lang.ArityException (position full-house-hand)))
  (is (thrown? clojure.lang.ArityException (position flush-hand)))
  (is
   (thrown?
    clojure.lang.ArityException
    (position straight-flush-hand)))
  (is
   (thrown? clojure.lang.ArityException (position royal-flush-hand)))
  (is
   (thrown?
    clojure.lang.ArityException
    (position
     "Returns the position of elt in this list, or nil if not present")))
  (is (thrown? clojure.lang.ArityException (position nil)))
  (is (thrown? clojure.lang.ArityException (position true)))
  (is (thrown? clojure.lang.ArityException (position 0)))
  (is (thrown? clojure.lang.ArityException (position 1)))
  (is (thrown? clojure.lang.ArityException (position -1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-member?
 (testing
  "member?"
  (is (thrown? clojure.lang.ArityException (member? nil)))
  (is (thrown? clojure.lang.ArityException (member? ())))
  (is (thrown? clojure.lang.ArityException (member? '(a :b "c"))))
  (is (thrown? clojure.lang.ArityException (member? true)))
  (is (thrown? clojure.lang.ArityException (member? "test")))
  (is (thrown? clojure.lang.ArityException (member? :test)))
  (is (thrown? clojure.lang.ArityException (member? 0)))
  (is
   (thrown? clojure.lang.ArityException (member? Integer/MAX_VALUE)))
  (is (thrown? clojure.lang.ArityException (member? 22/7)))
  (is (thrown? clojure.lang.ArityException (member? 1.0E-4)))
  (is (thrown? clojure.lang.ArityException (member? -1.0E-4)))
  (is (thrown? clojure.lang.ArityException (member? ranks)))
  (is (thrown? clojure.lang.ArityException (member? suits)))
  (is (thrown? clojure.lang.ArityException (member? five-kind-hand)))
  (is (thrown? clojure.lang.ArityException (member? four-kind-hand)))
  (is (thrown? clojure.lang.ArityException (member? full-house-hand)))
  (is (thrown? clojure.lang.ArityException (member? flush-hand)))
  (is
   (thrown? clojure.lang.ArityException (member? straight-flush-hand)))
  (is (thrown? clojure.lang.ArityException (member? royal-flush-hand)))
  (is
   (thrown?
    clojure.lang.ArityException
    (member? "True if list contains at least one instance of elt")))
  (is (thrown? clojure.lang.ArityException (member? false)))
  (is (thrown? clojure.lang.ArityException (member? true)))
  (is (thrown? clojure.lang.ArityException (member? true)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-wild?
 (testing
  "wild?"
  (is (= (wild? nil) 'false))
  (is (= (wild? ()) 'false))
  (is (= (wild? '(a :b "c")) 'false))
  (is (= (wild? true) 'false))
  (is (= (wild? "test") 'false))
  (is (= (wild? :test) 'false))
  (is (= (wild? 0) 'false))
  (is (= (wild? Integer/MAX_VALUE) 'false))
  (is (= (wild? 22/7) 'false))
  (is (= (wild? 1.0E-4) 'false))
  (is (= (wild? -1.0E-4) 'false))
  (is (= (wild? ranks) 'false))
  (is (= (wild? suits) 'false))
  (is (= (wild? five-kind-hand) 'false))
  (is (= (wild? four-kind-hand) 'false))
  (is (= (wild? full-house-hand) 'false))
  (is (= (wild? flush-hand) 'false))
  (is (= (wild? straight-flush-hand) 'false))
  (is (= (wild? royal-flush-hand) 'false))
  (is (= (wild? "True if this card is the joker") 'false))
  (is (= (wild? :joker) 'true))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-n-kind?
 (testing
  "n-kind?"
  (is (thrown? clojure.lang.ArityException (n-kind? nil)))
  (is (thrown? clojure.lang.ArityException (n-kind? ())))
  (is (thrown? clojure.lang.ArityException (n-kind? '(a :b "c"))))
  (is (thrown? clojure.lang.ArityException (n-kind? true)))
  (is (thrown? clojure.lang.ArityException (n-kind? "test")))
  (is (thrown? clojure.lang.ArityException (n-kind? :test)))
  (is (thrown? clojure.lang.ArityException (n-kind? 0)))
  (is
   (thrown? clojure.lang.ArityException (n-kind? Integer/MAX_VALUE)))
  (is (thrown? clojure.lang.ArityException (n-kind? 22/7)))
  (is (thrown? clojure.lang.ArityException (n-kind? 1.0E-4)))
  (is (thrown? clojure.lang.ArityException (n-kind? -1.0E-4)))
  (is (thrown? clojure.lang.ArityException (n-kind? ranks)))
  (is (thrown? clojure.lang.ArityException (n-kind? suits)))
  (is (thrown? clojure.lang.ArityException (n-kind? five-kind-hand)))
  (is (thrown? clojure.lang.ArityException (n-kind? four-kind-hand)))
  (is (thrown? clojure.lang.ArityException (n-kind? full-house-hand)))
  (is (thrown? clojure.lang.ArityException (n-kind? flush-hand)))
  (is
   (thrown? clojure.lang.ArityException (n-kind? straight-flush-hand)))
  (is (thrown? clojure.lang.ArityException (n-kind? royal-flush-hand)))
  (is
   (thrown?
    clojure.lang.ArityException
    (n-kind?
     "True if this hand has N cards of this rank (jokers wild)")))
  (is (thrown? clojure.lang.ArityException (n-kind? :rank)))
  (is
   (thrown?
    clojure.lang.ArityException
    (n-kind?
     "True if this hand has N cards of same rank (jokers wild)")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-two-pair?
 (testing
  "two-pair?"
  (is (= (two-pair? nil) 'false))
  (is (= (two-pair? ()) 'false))
  (is (= (two-pair? '(a :b "c")) 'true))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? true)))
  (is (= (two-pair? "test") 'true))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? :test)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 0)))
  (is
   (thrown?
    java.lang.IllegalArgumentException
    (two-pair? Integer/MAX_VALUE)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 22/7)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 1.0E-4)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? -1.0E-4)))
  (is (= (two-pair? ranks) 'true))
  (is (= (two-pair? suits) 'true))
  (is (= (two-pair? five-kind-hand) 'false))
  (is (= (two-pair? four-kind-hand) 'false))
  (is (= (two-pair? full-house-hand) 'true))
  (is (= (two-pair? flush-hand) 'false))
  (is (= (two-pair? straight-flush-hand) 'false))
  (is (= (two-pair? royal-flush-hand) 'false))
  (is
   (=
    (two-pair?
     "True if hand is two pair (i.e. two cards of one suit, two of another) (jokers wild)")
    'true))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 2)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 3)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 1)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 2)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 3)))
  (is (thrown? java.lang.IllegalArgumentException (two-pair? 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-full-house?
 (testing
  "full-house?"
  (is (= (full-house? nil) 'nil))
  (is (= (full-house? ()) 'nil))
  (is (= (full-house? '(a :b "c")) 'nil))
  (is (thrown? java.lang.IllegalArgumentException (full-house? true)))
  (is (= (full-house? "test") 'nil))
  (is (thrown? java.lang.IllegalArgumentException (full-house? :test)))
  (is (thrown? java.lang.IllegalArgumentException (full-house? 0)))
  (is
   (thrown?
    java.lang.IllegalArgumentException
    (full-house? Integer/MAX_VALUE)))
  (is (thrown? java.lang.IllegalArgumentException (full-house? 22/7)))
  (is
   (thrown? java.lang.IllegalArgumentException (full-house? 1.0E-4)))
  (is
   (thrown? java.lang.IllegalArgumentException (full-house? -1.0E-4)))
  (is (= (full-house? ranks) 'nil))
  (is (= (full-house? suits) 'nil))
  (is (= (full-house? five-kind-hand) 'false))
  (is (= (full-house? four-kind-hand) 'false))
  (is (= (full-house? full-house-hand) 'true))
  (is (= (full-house? flush-hand) 'nil))
  (is (= (full-house? straight-flush-hand) 'nil))
  (is (= (full-house? royal-flush-hand) 'nil))
  (is
   (=
    (full-house?
     "True if hand is full house (i.e. three cards of one suit, two of another) (jokers wild)")
    'nil))
  (is (thrown? java.lang.IllegalArgumentException (full-house? 3)))
  (is (thrown? java.lang.IllegalArgumentException (full-house? 4)))
  (is (thrown? java.lang.IllegalArgumentException (full-house? 2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-flush?
 (testing
  "flush?"
  (is (= (flush? nil) 'false))
  (is (= (flush? ()) 'false))
  (is (= (flush? '(a :b "c")) 'true))
  (is (thrown? java.lang.IllegalArgumentException (flush? true)))
  (is (= (flush? "test") 'true))
  (is (thrown? java.lang.IllegalArgumentException (flush? :test)))
  (is (thrown? java.lang.IllegalArgumentException (flush? 0)))
  (is
   (thrown?
    java.lang.IllegalArgumentException
    (flush? Integer/MAX_VALUE)))
  (is (thrown? java.lang.IllegalArgumentException (flush? 22/7)))
  (is (thrown? java.lang.IllegalArgumentException (flush? 1.0E-4)))
  (is (thrown? java.lang.IllegalArgumentException (flush? -1.0E-4)))
  (is (= (flush? ranks) 'true))
  (is (= (flush? suits) 'true))
  (is (= (flush? five-kind-hand) 'false))
  (is (= (flush? four-kind-hand) 'false))
  (is (= (flush? full-house-hand) 'false))
  (is (= (flush? flush-hand) 'false))
  (is (= (flush? straight-flush-hand) 'true))
  (is (= (flush? royal-flush-hand) 'true))
  (is
   (=
    (flush?
     "True if cards in hand all have the same suit (jokers wild)")
    'true))
  (is (thrown? java.lang.IllegalArgumentException (flush? :suit)))
  (is (thrown? java.lang.IllegalArgumentException (flush? :joker)))
  (is (thrown? java.lang.IllegalArgumentException (flush? 1)))
  (is (thrown? java.lang.IllegalArgumentException (flush? 2)))
  (is (thrown? java.lang.IllegalArgumentException (flush? 0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-all-ranks
 (testing
  "all-ranks"
  (is (= (all-ranks nil) '#{}))
  (is (= (all-ranks ()) '#{}))
  (is (= (all-ranks '(a :b "c")) '#{nil}))
  (is (thrown? java.lang.IllegalArgumentException (all-ranks true)))
  (is (= (all-ranks "test") '#{nil}))
  (is (thrown? java.lang.IllegalArgumentException (all-ranks :test)))
  (is (thrown? java.lang.IllegalArgumentException (all-ranks 0)))
  (is
   (thrown?
    java.lang.IllegalArgumentException
    (all-ranks Integer/MAX_VALUE)))
  (is (thrown? java.lang.IllegalArgumentException (all-ranks 22/7)))
  (is (thrown? java.lang.IllegalArgumentException (all-ranks 1.0E-4)))
  (is (thrown? java.lang.IllegalArgumentException (all-ranks -1.0E-4)))
  (is (= (all-ranks ranks) '#{nil}))
  (is (= (all-ranks suits) '#{nil}))
  (is (= (all-ranks five-kind-hand) '#{nil :ace}))
  (is (= (all-ranks four-kind-hand) '#{2 3}))
  (is (= (all-ranks full-house-hand) '#{2 3}))
  (is (= (all-ranks flush-hand) '#{2 3 4 5 6}))
  (is (= (all-ranks straight-flush-hand) '#{2 3 4 5 6}))
  (is (= (all-ranks royal-flush-hand) '#{:queen :king :jack 10 :ace}))
  (is
   (=
    (all-ranks "The set of ranks in a hand (joker counts as rank)")
    '#{nil}))
  (is (thrown? java.lang.IllegalArgumentException (all-ranks :rank)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-n-ranks?
 (testing
  "n-ranks?"
  (is (thrown? clojure.lang.ArityException (n-ranks? nil)))
  (is (thrown? clojure.lang.ArityException (n-ranks? ())))
  (is (thrown? clojure.lang.ArityException (n-ranks? '(a :b "c"))))
  (is (thrown? clojure.lang.ArityException (n-ranks? true)))
  (is (thrown? clojure.lang.ArityException (n-ranks? "test")))
  (is (thrown? clojure.lang.ArityException (n-ranks? :test)))
  (is (thrown? clojure.lang.ArityException (n-ranks? 0)))
  (is
   (thrown? clojure.lang.ArityException (n-ranks? Integer/MAX_VALUE)))
  (is (thrown? clojure.lang.ArityException (n-ranks? 22/7)))
  (is (thrown? clojure.lang.ArityException (n-ranks? 1.0E-4)))
  (is (thrown? clojure.lang.ArityException (n-ranks? -1.0E-4)))
  (is (thrown? clojure.lang.ArityException (n-ranks? ranks)))
  (is (thrown? clojure.lang.ArityException (n-ranks? suits)))
  (is (thrown? clojure.lang.ArityException (n-ranks? five-kind-hand)))
  (is (thrown? clojure.lang.ArityException (n-ranks? four-kind-hand)))
  (is (thrown? clojure.lang.ArityException (n-ranks? full-house-hand)))
  (is (thrown? clojure.lang.ArityException (n-ranks? flush-hand)))
  (is
   (thrown?
    clojure.lang.ArityException
    (n-ranks? straight-flush-hand)))
  (is
   (thrown? clojure.lang.ArityException (n-ranks? royal-flush-hand)))
  (is
   (thrown?
    clojure.lang.ArityException
    (n-ranks?
     "True if hand contains exactly n ranks (joker counts as rank)")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest
 test-rank-values
 (testing
  "rank-values"
  (is (= (rank-values nil) '()))
  (is (= (rank-values ()) '()))
  (is (= (rank-values '(a :b "c")) '(0 0 0)))))
  