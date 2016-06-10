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
  [item]
  (update item :sell-in dec))

(defn expired? [item]
  (< (:sell-in item) 0))

(defn quality-regulator
  [quality]
  (cond
    (< quality min-quality) min-quality
    (> quality max-quality) max-quality
    :else quality))

(defn update-item-quality
  [item quality-update-fn]
  (update item :quality
          (fn [q]
            (quality-regulator (quality-update-fn q)))))

(defn declining-quality-updater
  [item rate]
  (update-item-quality item (fn [quality]
                              (if (expired? item)
                                (- quality (* 2 rate))
                                (- quality rate)))))

(defn age-well-quality-updater
  [item]
  (update-item-quality item (fn [quality]
                              (inc quality))))

(defn pass-quality-updater
  [{:keys [sell-in] :as item}]
  (update-item-quality item (fn [quality]
                              (cond
                                (expired? item) 0
                                (< sell-in 5) (+ quality 3)
                                (< sell-in 10) (+ quality 2)
                                :else (inc quality)))))

(defmulti item-updater
          (fn [{:keys [name]}]
            (get item-categories name)
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
  [inventory]
  (mapv item-updater inventory))

(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(defn update-current-inventory []
  (let [inventory
        [(item "+5 Dexterity Vest" 10 20)
         (item "Aged Brie" 2 0)
         (item "Elixir of the Mongoose" 5 7)
         (item "Sulfuras, Hand Of Ragnaros" 0 80)
         (item "Backstage passes to a TAFKAL80ETC concert" 15 20)
         (item "Conjured Conjurer of Conjuring" 10 20)]]
    (update-quality inventory)))