(ns almanac.core
  (:require
    [almanac.constants :as c]
    [almanac.ellipse :as ellipse]
    [almanac.math :as math]
    [almanac.solar-system :as solar-system]
    [almanac.sunrise-equation :as sunrise-equation]
    [clj-time.coerce :as tc]
    [clj-time.core :as t]
    [clj-time.format :as tf]))

;; TODO
;; Precession of Earth's orbit
;; Interaction of Earth and moon
;; Precession of Earth's axial tilt?

(defrecord Config
  [lattitude
   longitude
   time-zone
   date
   circular-orbit
   coarse-sunrise-equation])

(defn- get-sun-inclination-fn [lattitude longitude circular-orbit]
  (fn [epoch]
    (let [date (tc/from-epoch (int epoch))
          {:keys [inclination-angle]} (solar-system/apparent-position-of-sun
                                        lattitude longitude date circular-orbit)]
      (+ inclination-angle c/SUN_HALF_WIDTH_OFFSET_DEGREES))))

(defn- calculate-sunrise-and-sunset
  [^Config conf]
  (let [{:keys [lattitude
                longitude
                date
                circular-orbit
                coarse-sunrise-equation]} conf
        sun-inclination-fn (get-sun-inclination-fn
                             lattitude longitude circular-orbit)
        coarse-solutions (sunrise-equation/calculate-approximate-sunrise-and-sunset
                           lattitude longitude date circular-orbit)]
    (if coarse-sunrise-equation
      coarse-solutions
      (->> coarse-solutions
           (map tc/to-epoch)
           (map #(math/newton sun-inclination-fn 1.0 0.01 20 %))
           (map int)
           (map tc/from-epoch)))))

(declare cli-spec
         parse-args)

(defn- time-of-day-str [dt]
  (let [s (str dt)
        regex #".+T(\d{2,2}):(\d{2,2}):(\d{2,2}).+"
        [_ h m s] (re-matches regex s)
        [h m s] (map #(Integer/parseInt %) [h m s])]
    (format "%d:%02d:%02d" h m s)))

(defn -main [& args]
  (let [opts (parse-args cli-spec args)
        [sunrise sunset] (->> (calculate-sunrise-and-sunset opts)
                              (map #(t/plus % (t/hours (:time-zone opts))))
                              (map time-of-day-str))]
    (println (format "Sunrise: %s\nSunset: %s" sunrise sunset))))

(defn parse-args [cli-spec args]
  (->> args
       (partition 2)
       (map (fn [[k v]]
              (let [{:keys [arg parse-fn]} (->> cli-spec
                                                (filter #(= k (:arg %)))
                                                first)
                    [_ k*] (re-matches #"\-\-(.+)" arg)
                    v* (if parse-fn
                         (parse-fn v)
                         v)]
                [(keyword k*) v*])))
       (into {})
       map->Config))

(def cli-spec
  [{:arg "--lattitude"
    :description "REQUIRED. Lattitude in degrees."
    :parse-fn #(Double/parseDouble %)}
   {:arg "--longitude"
    :description "REQUIRED. Longitude in degrees."
    :parse-fn #(Double/parseDouble %)}
   {:arg "--time-zone"
    :description "REQUIRED. Time zone, as a number of hours after UTC."
    :parse-fn #(Integer/parseInt %)}
   {:arg "--date"
    :description "REQUIRED. Date"
    :parse-fn #(tf/parse (:date tf/formatters) %)}
   {:arg "--coarse-sunrise-eq"
    :description (str "OPTIONAL. When true, don't account for the motion of "
                      "Earth around the Sun during the day.")
    :parse-fn #(Boolean/parseBoolean %)}
   {:arg "--coarse-sunrise-eq"
    :description (str "OPTIONAL. When true, model Earth's orbit as a circle "
                      "instead of an ellipse.")
    :parse-fn #(Boolean/parseBoolean %)}])
