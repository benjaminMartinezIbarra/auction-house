(ns auction-house.core-test
  (:require [clojure.test :refer :all]
            [auction-house.core :refer :all]))

(deftest exercice-example-test
  (testing "auction with 100 euros as reserve price and:
    Bob bidding 110, 130,
    Eva no bidding,
    Marc bidding 125
    Ben bidding 105, 115, 90
    David bidding 132, 135, 140
    should end with David as winner with a price of 130 euro"
    (is (= "Winner is :david with a bid of 130" (winning-auction 100 {:bob   #{110 130}
                                                                      :eva   #{}
                                                                      :marc  #{125}
                                                                      :ben   #{105 115 90}
                                                                      :david #{132 135 140}})))))
(deftest auction-test-suite
  (testing "empty auction with 10 Euros as reserve price and no bidder should end with no winner"
    (is (= "No winner for this auction" (winning-auction 10 {}))))

  (testing "empty auction with 0 euros as reserve price and no bidder should end with no winner"
    (is (= "No winner for this auction" (winning-auction 0 {}))))

  (testing "empty auction with 0 euros as reserve price and no auction bids should end with no winner"
    (is (= "No winner for this auction" (winning-auction 0  nil))))

  (testing "auction with 0 euros as reserve price and Bob as single bidder with 0 should end with Bob as winner"
    (is (= "Winner is :bob with a bid of 0" (winning-auction 0 {:bob #{0}}))))

  (testing "auction with 0 euros as reserve price and Bob as single bidder with 1 should end with Bob as winner"
    (is (= "Winner is :bob with a bid of 0" (winning-auction 0 {:bob #{1}}))))

  (testing "auction with 0 euros as reserve price and Bob as single bidder with negative amount value should end with no winner"
    (is (= "No winner for this auction" (winning-auction 0 {:bob #{-1}}))))

  (testing "empty auction with 0 euros as reserve price and 2 bidders with no bids should end with no winner"
    (is (= "No winner for this auction" (winning-auction 0 {:bob #{} :tommy #{}}))))

  (testing "empty auction with 0 euros as reserve price and 2 bidder with no bids should end with no winner"
    (is (= "No winner for this auction" (winning-auction 0 {:bob nil :tommy nil}))))

  (testing "auction with 0 euros as reserve price and 2 bidders with a 0 amount each should end with no winner"
    (is (= "No winner for this auction" (winning-auction 0 {:bob #{0} :tom #{0}}))))

  (testing "auction with 0 euros as reserve price and 2 bidders with amount of 1 each should end with no winner"
    (is (= "No winner for this auction" (winning-auction 0 {:bob #{1} :tom #{1}}))))

  (testing "auction with 1 euros as reserve price and Bob bidding 1, and tom bidding 2, should end with Tom as winner with a price of 1 euro"
    (is (= "Winner is :tom with a bid of 1" (winning-auction 1 {:bob #{1} :tom #{2}}))))

  (testing "auction with 2 euros as reserve price and Bob bidding 1 and David bidding 2, should end with David as winner with a price of 2 euro"
    (is (= "Winner is :david with a bid of 2" (winning-auction 2 {:bob #{1} :david #{2}}))))

  (testing "auction with 20 euros as reserve price and Bob bidding 21, 22 and tom bidding 1,9 should end with Bob as winner with the reserved price"
    (is (= "Winner is :bob with a bid of 20" (winning-auction 20 {:bob #{21 22} :tom #{19}}))))

  (testing "auction with 19 euros as reserve price and Bob bidding 21, 22, 23 and tom bidding 20, should end with Bob as winner with the last winning bid (20)"
    (is (= "Winner is :bob with a bid of 20" (winning-auction 19 {:bob #{21 22 23} :tom #{20}}))))

  (testing "unordered auction with 19 euros as reserve price and Bob bidding 22, 21, 23 and tom bidding 21, 20, 19, should end with Bob as winner with the last winning bid (21)"
    (is (= "Winner is :bob with a bid of 21" (winning-auction 19 {:bob #{22 21 23} :tom #{21 20 19}})))))
