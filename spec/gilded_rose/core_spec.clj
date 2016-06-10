(ns gilded-rose.core-spec
  (:require [clojure.test :refer :all]
            [gilded-rose.core :refer [item update-quality]]))

(def vest "+5 Dexterity Vest")
(def brie "Aged Brie")
(def elixir "Elixir of the Mongoose")
(def sulf "Sulfuras, Hand Of Ragnaros")
(def pass "Backstage passes to a TAFKAL80ETC concert")
(def conjurer "Conjured Conjurer of Conjuring")

;; for ease of testing...
(def item-name-list [vest brie elixir sulf pass conjurer])

(def inventory [(item vest 10 20)
                (item brie 2 0)
                (item elixir 5 7)
                (item sulf 0 80)
                (item pass 15 20)
                (item conjurer 10 20)])

(def filter-first (comp first filter))

;; I don't think we'll blow the stack here by testing...
(defn recur-update-quality [items n]
  (if (> n 0)
    (recur-update-quality (update-quality items) (dec n))
    items))

(defn get-item [items item-name]
  (filter-first #(= (:name %) item-name) items))

(deftest tests
  (let [one-day (recur-update-quality inventory 1)
        two-days (recur-update-quality inventory 2)
        six-days (recur-update-quality inventory 6)
        eleven-days (recur-update-quality inventory 11)
        sixteen-days (recur-update-quality inventory 16)
        eighty-days (recur-update-quality inventory 80)]

    (testing "regular sell in degredation"
      (is (= [9 1 4 0 14 9]
             (map (comp :sell-in (partial get-item one-day))
                  item-name-list)))
      (is (= [8 0 3 0 13 8]
             (map (comp :sell-in (partial get-item two-days))
                  item-name-list))))
    (testing "quality change"
      (is (= [19 1 6 80 21 18]
             (map (comp :quality (partial get-item one-day))
                  item-name-list)))
      (is (= [18 2 5 80 22 16]
             (map (comp :quality (partial get-item two-days))
                  item-name-list))))

    (testing "quality never negative"
      (is (= 0 (:quality (get-item eleven-days elixir)))))
    (testing "quality never greater than 50"
      (is (= 50 (:quality (get-item eighty-days brie)))))

    (testing "quality degrades twice as fast past sell by date"
      (is (= 8 (:quality (get-item eleven-days vest))))
      (is (= 0 (:quality (get-item six-days elixir)))))

    (testing "passes quality improve by 1 when > 10 days to sell by"
      (is (= 21 (:quality (get-item one-day pass))))
      (is (= 22 (:quality (get-item two-days pass)))))
    (testing "passes quality improve by 2 when <= 10 days to sell by but > 5"
      (is (= 27 (:quality (get-item six-days pass)))))
    (testing "passes quality improve by 3 when <= 5 days to sell by but > -1"
      (is (= 38 (:quality (get-item eleven-days pass)))))
    (testing "passes quality drops to 0 when sell by is -1"
      (is (= 0 (:quality (get-item sixteen-days pass)))))))