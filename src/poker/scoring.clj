(ns poker.scoring)

;; card is a map {:suit s :rank r}; a hand is a tuple (card card card card card)

;; all valid ranks. Two leading nils ensure (nth ranks n) returns the card with 
;; face value n.
(def ranks '(nil nil 2 3 4 5 6 7 8 9 10 :jack :queen :king :ace))

(def suits '(:heart :diamond :club :spade))

(def five-kind-hand '({:suit :heart :rank :ace}
		{:suit :diamond :rank :ace}
		{:suit :club :rank :ace}
		{:suit :spade :rank :ace}
		:joker))

(def four-kind-hand '({:suit :heart :rank 2}
		{:suit :diamond :rank 2}
		{:suit :club :rank 2}
		{:suit :spade :rank 2}
		{:suit :spade :rank 3}))

(def full-house-hand '({:suit :heart :rank 2}
		{:suit :diamond :rank 2}
		{:suit :club :rank 2}
		{:suit :heart :rank 3}
		{:suit :spade :rank 3}))

(def straight-hand '({:suit :heart :rank 2}
		{:suit :heart :rank 3}
		{:suit :diamond :rank 4}
		{:suit :club :rank 5}
		{:suit :spade :rank 6}))

(def flush-hand '({:suit :heart :rank 2}
		{:suit :heart :rank 3}
		{:suit :heart :rank :jack}
		{:suit :heart :rank :queen}
		{:suit :heart :rank :ace}))

(def straight-flush-hand '({:suit :heart :rank 2}
		{:suit :heart :rank 3}
		{:suit :heart :rank 4}
		{:suit :heart :rank 5}
		{:suit :heart :rank 6}))

(def royal-flush-hand '({:suit :heart :rank 10}
		{:suit :heart :rank :jack}
		{:suit :heart :rank :queen}
		{:suit :heart :rank :king}
		{:suit :heart :rank :ace}))


(defn position 
	"Returns the position of elt in this list, or nil if not present"
	([list elt n]
		(cond
			(empty? list) nil
			(= (first list) elt) n
			true (position (rest list) elt (inc n))))
	([list elt]
		(position list elt 0)))

(defn member? [list elt]
	"True if list contains at least one instance of elt"
	(cond 
		(empty? list) false
		(= (first list) elt) true
		true (member? (rest list) elt)))

(defn wild? [card]
	"True if this card is the joker"
	(= card :joker))

(defn n-kind? 
	([hand n rank]
		"True if this hand has N cards of this rank (jokers wild)"
		(>= 
			(count (filter #(or (= (:rank %) rank)(wild? %)) hand))
			n))
	([hand n]
		"True if this hand has N cards of same rank (jokers wild)"
		(first (filter #(n-kind? hand n %) ranks))))

(defn two-pair? [hand]
	"True if hand is two pair (i.e. two cards of one suit, two of another) (jokers wild)"
	(>= (count (remove #(not %) (map #(n-kind? hand 2 %) ranks))) 2))

(defn full-house? [hand]
	"True if hand is full house (i.e. three cards of one suit, two of another) (jokers wild)"
	(and 
		(n-kind? hand 3) ;; there's a 3-of-a-kind there
		(two-pair? hand) ;; there are two or more 2-of-kinds here
		))

(defn flush? [hand]
	"True if cards in hand all have the same suit (jokers wild)"
	(= (count (set (map :suit (remove #(= % :joker) hand)))) 1))

(defn all-ranks [hand]
	"The set of ranks in a hand (joker counts as rank)"
	(set (map :rank hand)))

(defn n-ranks? [hand n]
	"True if hand contains exactly n ranks (joker counts as rank)"
	(= (count (all-ranks hand)) n))

(defn rank-values [hand]
	"Returns as a list the numeric values of card in a hand (joker is zero)"
	(map #(position ranks (:rank %)) hand)) 

(defn face-value [hand]
	"Returns the sum of the face values of cards in a hand"
	(apply + (rank-values hand)))

(defn straight? [hand]
	"True if hand is a straight (i.e. card values in sequence) (jokers wild)"
	(let [n-ranks (count (all-ranks hand))
		values (rank-values hand)
		no-joke (remove zero? values) 
		max (apply max no-joke)
		min (apply min no-joke)]
		(and (= n-ranks 5) (<= (- max min) 5))))

(defn straight-flush? [hand]
	"True if hand is a straight flush (all same suit, values in sequence) 
	(jokers wild)"
	(and (straight? hand) (flush? hand)))

(defn royal-flush? [hand]
	"True if hand is a royal flush (straight flush including ace) 
	(jokers wild)"
	(let [hand-ranks (all-ranks hand)]
	(and (straight-flush? hand)
		(or
			(member? hand-ranks :ace)
			;; joker substitutes as ace
			(and (member? hand :joker) 
				(member? hand-ranks :king)))))) 

(defn class-value [hand]
	"Return a value based on the class of the hand. Values are essentially 
	arbitrary but maintain the correct ordering."
	(cond
		(n-kind? hand 5) 110
		(royal-flush? hand) 100
		(straight-flush? hand) 90
		(n-kind? hand 4) 80
		(full-house? hand) 70
		(flush? hand) 60
		(straight? hand) 50
		(n-kind? hand 3) 40
		(two-pair? hand) 30
		(n-kind? hand 2) 20
		true (apply max (rank-values hand))))

(defn value [hand]
	"Return a value for the hand. Values are essentially arbitrary but 
	maintain the correct ordering."
	(+
		(* (class-value hand) 100)
		(face-value hand)))
 
 
(defn beats? [hand1 hand2]
	"True if hand1 scores higher than hand2"
	(> (value hand1)(value hand2)))


