(ns ^{:doc "The Auction house program."
      :author "Benjamin Martinez Ibarra"}
  auction-house.core
  (:gen-class))

(declare find-winning-bid
         map-to-bids
         amount bidder
         to-bids
         sort-by-amount-desc
         same-bidder?
         winning-bidder-amount
         by-bid-amount
         minimum-winning-amount)

;Bid type
(defrecord Bid
  [bidder bid-amount])

(defn winning-auction
  "Main controller of the auction. Will compare each bid until finding a winner.
  Returns the winner of the auction with the bidding amount he will have to pay.
  A winner has the best bid amount for the auction but may not pay that amount.
  A winner will pay the last winning bid amount involving another bidder or the start price if not superior.
  May return 'no winner' message if no bidder are present and/or none can afford the start price at least."
  [start-price all-bids]
  (let [winning-bid (-> all-bids
                        (map-to-bids)
                        (find-winning-bid start-price))]
    (if (= :no-winner winning-bid)
      "No winner for this auction"
      (let [winning-amount (amount winning-bid)
            winner (bidder winning-bid)]
        (str "Winner is " winner " with a bid of " winning-amount)))))

(defn- map-to-bids
  "Transform a map where each entry represents a bidder and his bids amounts into a sequence of Bids ([bidder bid-amount] pairs)."
  [bidder-amounts-map]
  (reduce
    (fn [all-bids [bidder bid-amounts]]
      (into all-bids (to-bids bidder bid-amounts)))
    (vector)
    (seq bidder-amounts-map)))

(defn- to-bids
  "Transforms a bidder and its bids amounts into a sequence of Bids.
  A Bid is composed of a bidder and a bid-amount."
  [bidder bids-amounts]
  (reduce
    (fn [bids bid-amount]
      (conj bids (->Bid bidder bid-amount)))
    (vector)
    bids-amounts))

(defn find-winning-bid
  "Finds the winning bid among a collection of bids.
   May return :no-winner keyword if none of the 2 best bidders have a bidding amount >= start price
   or both of the two greatest bidders have same max bid amounts.
   Otherwise, iterate through the bids and find the combination of the best bidder whose bid amount is the greatest among all the bid
   and the last winning bid amount value whenever it comes from a different bidder."
  ([bids start-price]
   (let [sorted-bids (sort-by-amount-desc bids)
         top-bid (first sorted-bids)
         next-bids (rest sorted-bids)]
     (if (or (nil? top-bid)
             (< (amount top-bid) start-price)
             (and (= 2 (count sorted-bids))
                  (= (amount top-bid) (amount (first next-bids)))))
       :no-winner
       (find-winning-bid top-bid next-bids start-price))))
  ([reference-bid bids start-price]
   (let [next-bid (first bids)]
     (if (not (same-bidder? reference-bid next-bid))
       (winning-bidder-amount reference-bid next-bid start-price)
       (recur next-bid (rest bids) start-price)))))

(defn- sort-by-amount-desc
  "Sorts a list of bids by their bid-amount."
  [bids]
  (sort by-bid-amount bids))

(defn by-bid-amount
  "Simple comparator between 2 bids, comparing their amount decremently."
  [^Bid bid1 ^Bid bid2]
  (compare (amount bid2) (amount bid1)))

(defn- amount
  "Retrieve the amount part of a bid"
  [^Bid bid]
  (or (:bid-amount bid) 0))

(defn- same-bidder?
  "Returns true if both bids belong to the same bidder.False otherwise"
  [^Bid bid1 ^Bid bid2]
  (= (bidder bid1) (bidder bid2)))

(defn- bidder
  "Retrieve the bidder part of a bid"
  [^Bid bid]
  (:bidder bid))

(defn- winning-bidder-amount
  "Compare two bids and returns a combination of:
    - the bidder whose bid amount is the greatest of the two bids.
    - the minimum winning amount of the 2 bids or start-price if the amount is lower."
  [first-bid second-bid start-price]
  (let [first-bid-amount (amount first-bid)
        second-bid-amount (amount second-bid)]
    (cond
      (= first-bid-amount second-bid-amount) (->Bid (bidder first-bid) first-bid-amount)
      (> first-bid-amount second-bid-amount) (->Bid (bidder first-bid) (minimum-winning-amount second-bid-amount start-price))
      :else (->Bid (bidder second-bid) (minimum-winning-amount second-bid-amount start-price)))))

(defn- minimum-winning-amount
  "Retrieve the lowest amount between a bid amount and a start price.
   That amount can't be lower than the reference price."
  [last-winning-bid-amount start-price]
  (if (>= last-winning-bid-amount start-price)
    last-winning-bid-amount
    start-price))

(defn -main
  "Retrieving the winner of an auction."
  [& _]
  (let [bids {:bob   #{110 130}
              :eva   #{}
              :marc  #{125}
              :ben   #{105 115 90}
              :david #{132 135 140}}
        winning-bid (winning-auction 100 bids)]
    (println "auction: ")
    (println bids "\n")
    (println winning-bid)))
