(ns gilded-rose.core)

;; That goblin is evil. It'd be great to add an :item-type key to the item map
;; The alternative is to have a database or store somewhere of all the items and their
;; categories. So what I did here was just plop them into a map. If needed it can be
;; decoupled and have the item-categories passed through to update-quality, but it seemed
;; excessive for this problem :)
(def item-categories {"+5 Dexterity Vest"                         :regular
                      "Aged Brie"                                 :ages-well
                      "Elixir of the Mongoose"                    :regular
                      "Sulfuras, Hand Of Ragnaros"                :legendary
                      "Backstage passes to a TAFKAL80ETC concert" :pass
                      "Conjured Conjurer of Conjuring"            :conjured})

(def ^:const min-quality 0)
(def ^:const max-quality 50)

(defn update-sell-in
  "Decrements the sell-in value for an item"
  [item]
  (update item :sell-in dec))

(defn expired?
  "Checks to see if an item's sell-in value is less than 0. Use this function to check
  after updating the sell-in"
  [item]
  (< (:sell-in item) 0))

(defn quality-regulator
  "Regulates the updated quality of an item to ensure the value is within predefined min
  and max limits"
  [quality]
  (cond
    (< quality min-quality) min-quality
    (> quality max-quality) max-quality
    :else quality))

(defn update-item-quality
  "Updates an item's quality value using a function quality-update-fn and ensuring the new
  quality is valid"
  [item quality-update-fn]
  (update item :quality
          (fn [q]
            (quality-regulator (quality-update-fn q)))))

(defn declining-quality-updater
  "Quality updater that decreases an item's quality normally, unless it is expired. If expired
  then reduces it at twice the rate"
  [item rate]
  (update-item-quality item (fn [quality]
                              (if (expired? item)
                                (- quality (* 2 rate))
                                (- quality rate)))))

(defn age-well-quality-updater
  "Quality updater that increments an item's quality each tick"
  [item]
  (update-item-quality item (fn [quality]
                              (inc quality))))

(defn pass-quality-updater
  "Quality updater for passes. When expired, sets 0 as the new quality, otherwise it increments
  it accordingly. If the the new sell-in is < 5 it increments quality by 3, if < 10 by 2,
  otherwise just by one"
  [{:keys [sell-in] :as item}]
  (update-item-quality item (fn [quality]
                              (cond
                                (expired? item) 0
                                (< sell-in 5) (+ quality 3)
                                (< sell-in 10) (+ quality 2)
                                :else (inc quality)))))

(defmulti item-updater
          "Updates an item's sell-in and quality values accordingly given the type of item
          it is. An item's type is determined by a lookup to the item-categories map. Each
          item has a unique name that is stored there and lets the fn know what type it is"
          (fn [{:keys [name]}]
            (get item-categories name)))

(defmethod item-updater :regular
  [item]
  (-> item
      update-sell-in
      (declining-quality-updater 1)))

(defmethod item-updater :conjured
  [item]
  (-> item
      update-sell-in
      (declining-quality-updater 2)))

(defmethod item-updater :legendary
  [item]
  item)

(defmethod item-updater :ages-well
  [item]
  (-> item
      update-sell-in
      age-well-quality-updater))

(defmethod item-updater :pass
  [item]
  (-> item
      update-sell-in
      pass-quality-updater))

(defmethod item-updater :default [item]
  (throw (Exception. (str "Woah there buddy, "
                          (:name item)
                          " is not anywhere in your inventory or category stock"))))

;; Note: using mapv in order to maintain vector format of original list... otherwise
;; I think regular map should do just fine
(defn update-quality
  "Updates the quality and sell-in values for the Gilded Rose's inventory"
  [inventory]
  (mapv item-updater inventory))

;; Here be goblins
(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})
;; /End here be goblins

(defn update-current-inventory []
  (let [inventory
        [(item "+5 Dexterity Vest" 10 20)
         (item "Aged Brie" 2 0)
         (item "Elixir of the Mongoose" 5 7)
         (item "Sulfuras, Hand Of Ragnaros" 0 80)
         (item "Backstage passes to a TAFKAL80ETC concert" 15 20)
         (item "Conjured Conjurer of Conjuring" 10 20)]]
    (update-quality inventory)))