(ns almanac.time
  (:require
    [almanac.constants :as c]
    [clj-time.coerce :as tc]
    [clj-time.core :as t]))

(defn- seconds-since-winter-solstice-2023
  "Returns the date as a number of seconds since winter solstice 2023."
  [dt]
  (double (- (tc/to-epoch dt)
             (tc/to-epoch c/WINTER_SOLSTICE_2023_UTC))))

;; The Earth rotates once relative to the background of stars during one
;; "sidereal" day. This is different from the common meaning of day: a "solar"
;; day, where the sun does one lap in the sky as seen from Earth. The difference
;; is because the Earth moves in its orbit around the Sun during one day.
(defn sidereal-days-since-winter-solstice-2023
  "Computes the number of sidereal days since winter solstice 2023."
  [dt]
  (/ (seconds-since-winter-solstice-2023 dt)
     c/SIDEREAL_DAY_IN_SECONDS))

(defn years-since-winter-solstice-2023
  "Computes the number of years since since winter solstice 2023."
  [dt]
  (/ (seconds-since-winter-solstice-2023 dt)
     c/ONE_YEAR_IN_SECONDS))

(defn format-time
  "Expresses a number of seconds as a time of day string."
  [total-seconds]
  (let [hours (int (quot total-seconds 3600))
        total-seconds (mod total-seconds 3600)
        minutes (int (quot total-seconds 60))
        seconds (int (mod total-seconds 60))]
    (format "%d:%02d:%02d" hours minutes seconds)))
