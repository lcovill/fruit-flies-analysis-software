(ns fruit-flies-analysis-software.core
  (:require [mikera.image.core :as imagez]
            [mikera.image.colours :as colours])
  (:import java.io.File))

;; Current main idea:
;; 1. Load all frames
;; 2. Filter black/pixels (white = fly)
;; 3. Compare each frame to the previous one, note which new pixels contain white (fly)
;; 4. Find the "region of change" in which there has been movement of the fly (this is the proboscis)
;; 5. In each frame, find the lowest white pixel in the "region of change". This should be the current extension of the proboscis.

;; Potential improvements:
;; -Exclude a larger initial region (to avoid fly in lower/left region)
;; -Exclude all fly pixels in first frame and say they can't be proboscis pixels
;; (there is some slight shaking, so might not catch them all)
;; -Say that the most prevalent fly pixels cannot be proboscis pixels (to counteract slight shaking)

(def width 448)
(def height 594)
;; Values over this counts as white ( = fly), lower as black (= background)
(def white-threshold 25)

(defn index->coordinates
  "Takes a pixel index and returns [row column] (zero based)"
  ;{:test (fn []
  ;         (clojure.test/is (= (index->coordinates 0) [0 0]))
  ;         (clojure.test/is (= (index->coordinates width) [1 0]))
  ;         (clojure.test/is (= (index->coordinates (dec (* width height))) [(dec height) (dec width)])))}
  [index]
  (let [row (int (Math/floor (/ index width)))
        column (mod index width)]
    [row column]))

(defn coordinates->index
  "Takes a coordinate [row column] (zero based) and returns the pixel index"
  ;{:test (fn []
  ;         (clojure.test/is (= (coordinates->index [0 0]) 0))
  ;         (clojure.test/is (= (coordinates->index [1 0]) width))
  ;         (clojure.test/is (= (coordinates->index [(dec height) (dec width)]) (dec (* width height)))))}
  [[row column]]
  (+ (* row width) column))

;(def fly (imagez/load-image "assets/data/raw/scene00001.png"))
;(def pixels (imagez/get-pixels fly))

;(defn filter-fly []
;  (reduce (fn [white-pixels-indices index]
;            (if (> (first (colours/components-rgb (nth pixels index))) white-threshold)
;              (conj white-pixels-indices index)
;              white-pixels-indices))
;          []
;          (range (count pixels))))

(defn filter-pixels [pixels]
  (int-array (map (fn [pixel]
                    (if (> (first (colours/components-rgb pixel)) white-threshold)
                      colours/white
                      colours/black))
                  pixels)))

(defn get-directory-file-names [directory-name]
  (-> (reduce (fn [file-names file]
                (conj file-names (str (.getName file))))
              []
              (.listFiles (File. directory-name)))
      (sort)))

(def image-names (get-directory-file-names "assets/data"))

(defn show-difference [new-pixels old-pixels]
  (int-array (map (fn [index]
                    (if (and (= (nth new-pixels index) (unchecked-int colours/white))
                             (= (nth old-pixels index) (unchecked-int colours/black)))
                      colours/red
                      (nth new-pixels index)))
                  (range (count new-pixels)))))

(defn diff-and-save [new-image old-image new-name]
  (let [new-pixels (show-difference (filter-pixels (imagez/get-pixels new-image)) (filter-pixels (imagez/get-pixels old-image)))
        show-img (imagez/new-image width height)]
    (imagez/set-pixels show-img new-pixels)
    (imagez/save show-img (str "assets/output/" new-name ".png"))))

(defn get-region-of-change [images-pixels]
  (reduce (fn [changed-pixels image-index]
            (let [prev-pixels (nth images-pixels (dec image-index))
                  pixels (nth images-pixels image-index)]
              (reduce (fn [changed-pixels pixel-index]
                        (if (and (= (nth pixels pixel-index) (unchecked-int colours/white))
                                 (= (nth prev-pixels pixel-index) (unchecked-int colours/black)))
                          (conj changed-pixels pixel-index)
                          changed-pixels))
                      changed-pixels
                      (range (count pixels)))))
          #{}
          (drop 1 (range (count image-names)))))

(defn find-lowest-white-pixel [pixels region-of-change]
  (->> (keep-indexed (fn [index pixel]
                       (when (and (contains? region-of-change index)
                                  (= pixel (unchecked-int colours/white))
                                  ;; Attempt to filter out leftmost part of fly, 50 leftmost pixel columns are disregarded
                                  (> (second (index->coordinates index)) 50)
                                  )
                         index))
                     pixels)
       (apply max)
       ;(index->coordinates)
       ))

(defn find-proboscis-starting-point []
  (let [raw-pixels (imagez/get-pixels (imagez/load-image "assets/data/raw/scene00001.png"))
        modified-pixels (imagez/get-pixels (imagez/load-image "assets/data/modified/scene00001.png"))]
    (loop [i 0]
      (if (not= (nth raw-pixels i) (nth modified-pixels i))
        i
        (recur (inc i)))

      )))

(defn add-to-index [index dx dy]
  (->> (index->coordinates index)
       (mapv + [dy dx])
       (coordinates->index)))

(defn find-forbidden-region
  "Pixels close to the starting position of fly are considered forbidden"
  [pixels]
  (reduce (fn [forbidden-region index]
            (if (and (= (nth pixels index) (unchecked-int colours/white))
                     (< (second (index->coordinates index)) 80))
              (conj forbidden-region
                    index
                    (add-to-index index 0 1)
                    (add-to-index index 0 2)
                    (add-to-index index 1 0)
                    (add-to-index index 2 0))
              forbidden-region))
          #{}
          (range (count pixels))))

(defn find-distance-between-indices [i1 i2]
  (let [c1 (index->coordinates i1)
        c2 (index->coordinates i2)]
    (Math/sqrt (+ (* (- (first c1) (first c2)) (- (first c1) (first c2)))
                  (* (- (second c1) (second c2)) (- (second c1) (second c2)))))))

(defn find-next-proboscis-point-in-trajectory [pixels region-of-change forbidden-region previous-point-index]
  (->> (keep-indexed (fn [index pixel]
                       (when (and (contains? region-of-change index)
                                  (not (contains? forbidden-region index))
                                  (= pixel (unchecked-int colours/white))
                                  ;; Want to have lower tolerance, but won't work for some images
                                  (< (find-distance-between-indices index previous-point-index) 100))
                         index))
                     pixels)
       (apply max))
  ;(reduce (fn [best-index index]
  ;          (if (and (contains? region-of-change index)
  ;                   (= (nth pixels index) (unchecked-int colours/white))
  ;                   (<= (find-distance-between-indices index previous-point-index)
  ;                       (find-distance-between-indices best-index previous-point-index)))
  ;            (do (println (find-distance-between-indices index previous-point-index)) index)
  ;            best-index))
  ;        0
  ;        (range (count pixels)))
  )

(def counter (atom 0))
(def trajectory-points (atom []))

(defn main []
  ;; Show image
  ;(imagez/show fly)

  ;; Filter out fly pixels
  ;(let [new-image (imagez/new-image width height)
  ;      new-pixels (filter-pixels pixels)]
  ;  (imagez/set-pixels new-image new-pixels)
  ;  (imagez/show new-image)

  ;; Show difference between two images
  ;(let [old-image (imagez/load-image "assets/data/raw/scene00014.png")
  ;      new-image (imagez/load-image "assets/data/raw/scene00001.png")
  ;      new-pixels (show-difference (filter-pixels (imagez/get-pixels new-image)) (filter-pixels (imagez/get-pixels old-image)))]
  ;  (imagez/set-pixels new-image new-pixels)
  ;  (imagez/show new-image)
  ;  (imagez/save new-image "assets/output/newimg.png"))

  ;; Save image
  ;(let [old-image (imagez/load-image "assets/data/raw/scene00001.png")
  ;      new-image (imagez/load-image "assets/data/raw/scene00014.png")]
  ;  (diff-and-save new-image old-image "test"))

  ;; Filter fly pixels and save all images
  ;(let [images-pixels (->> (map (fn [image-name]
  ;                                (-> (imagez/load-image (str "assets/data/" image-name))
  ;                                    (imagez/get-pixels)
  ;                                    (filter-pixels)))
  ;                              image-names))
  ;      counter (atom 0)]
  ;  (for [image-pixels images-pixels]
  ;    (let [new-img (imagez/new-image width height)]
  ;      (swap! counter inc)
  ;      (imagez/set-pixels new-img image-pixels)
  ;      (imagez/save new-img (str "assets/output/" (deref counter) ".png")))
  ;    )
  ;  )

  ;; Take difference between many images and save them
  ;(for [i (drop 1 (range (count image-names)))]
  ;  (diff-and-save (imagez/load-image (str "assets/data/raw/" (nth image-names i)))
  ;                 (imagez/load-image (str "assets/data/raw/" (nth image-names (dec i))))
  ;                 i))

  ;; Try to track proboscis and save images with it marked (using region of change)
  ;(time (let [images-pixels (map (fn [image-name]
  ;                                 (-> (imagez/load-image (str "assets/data/raw/" image-name))
  ;                                     (imagez/get-pixels)
  ;                                     (filter-pixels)))
  ;                               image-names)
  ;            region-of-change (get-region-of-change images-pixels)]
  ;        (reset! counter 0)
  ;        (for [image-pixels images-pixels]
  ;          (let [lowest-white-pixel (find-lowest-white-pixel image-pixels region-of-change)
  ;                modified-pixels image-pixels
  ;                new-img (imagez/new-image width height)]
  ;            (swap! counter inc)
  ;            (aset modified-pixels lowest-white-pixel (unchecked-int colours/red))
  ;            (for [direction [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]]
  ;              (aset modified-pixels
  ;                    (as-> lowest-white-pixel $
  ;                          (index->coordinates $)
  ;                          (mapv + $ direction)
  ;                          (coordinates->index $))
  ;                    (unchecked-int colours/red)))
  ;            (imagez/set-pixels new-img modified-pixels)
  ;            (imagez/save new-img (str "assets/output/proboscis-" (deref counter) ".png")))
  ;          )))

  ;; Try to track proboscis using a start point and following it
  ;(time (let [starting-point (find-proboscis-starting-point)
  ;            images-pixels (->> (map (fn [image-name]
  ;                                      (-> (imagez/load-image (str "assets/data/modified/" image-name))
  ;                                          (imagez/get-pixels)
  ;                                          (filter-pixels)))
  ;                                    image-names))
  ;            region-of-change (get-region-of-change images-pixels)
  ;            forbidden-region (find-forbidden-region (first images-pixels))
  ;            counter (atom 0)
  ;            ;trajectory-points (atom [starting-point])
  ;            ]
  ;        (reset! trajectory-points [starting-point])
  ;        (for [image-pixels (drop 1 images-pixels)]
  ;          (let [next-point-index (find-next-proboscis-point-in-trajectory image-pixels region-of-change forbidden-region (last (deref trajectory-points)))
  ;                modified-pixels image-pixels
  ;                new-img (imagez/new-image width height)]
  ;            (swap! counter inc)
  ;            (swap! trajectory-points conj next-point-index)
  ;            (aset modified-pixels next-point-index (unchecked-int colours/red))
  ;            (for [direction [[1 0] [1 1] [0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1]]]
  ;              (aset modified-pixels
  ;                    (as-> next-point-index $
  ;                          (index->coordinates $)
  ;                          (mapv + $ direction)
  ;                          (coordinates->index $))
  ;                    (unchecked-int colours/red)))
  ;            (imagez/set-pixels new-img modified-pixels)
  ;            (imagez/save new-img (str "assets/output/proboscis-modified-" (deref counter) ".png")))
  ;          )
  ;        ))

  ;; Consider lowest white pixel (not counting XXX leftmost pixels) as the proboscis
  (println (let [images-pixels (map (fn [image-name]
                                      (-> (imagez/load-image (str "assets/data/" image-name))
                                          (imagez/get-pixels)
                                          (filter-pixels)))
                                    image-names)
                 counter (atom 0)
                 indices (reverse (range (* width height)))]
             (map index->coordinates (reduce (fn [trajectory-points image-pixels]
                                               (let [new-img (imagez/new-image width height)]
                                                 (swap! counter inc)
                                                 (imagez/set-pixels new-img image-pixels)
                                                 (imagez/save new-img (str "assets/output/" (deref counter) ".png"))
                                                 (conj trajectory-points (some (fn [index]
                                                                                 (when (and (= (nth image-pixels index) (unchecked-int colours/white))
                                                                                            (> (second (index->coordinates index)) 70))
                                                                                   index))
                                                                               indices))))
                                             []
                                             images-pixels))))

  )

(comment

  (map index->coordinates (deref trajectory-points))

  (find-distance-between-indices 116129 115007)

  (count image-names)

  (main)

  )
