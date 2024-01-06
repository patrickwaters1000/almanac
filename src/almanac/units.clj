(ns almanac.units
  (:require
    [almanac.constants :as c]
    [clj-time.coerce :as tc]
    [clj-time.core :as t]))

(defn date-to-seconds
  "Returns the date as a number of seconds since January 1, 2024."
  [dt]
  (- (tc/to-epoch dt)
     (tc/to-epoch (t/date-time 2024))))

(defn date-to-days
  "Returns the date as a fractional number of days."
  [dt]
  (/ (date-to-seconds dt) c/ONE_DAY_IN_SECONDS))

(defn date-to-years
  "Returns the date as a fractional number of years."
  [dt]
  (/ (date-to-seconds dt) c/ONE_YEAR_IN_SECONDS))

(defn radians-to-degrees [r]
  (* r (/ 180.0 Math/PI)))

(defn degrees-to-radians [d]
  (* d (/ Math/PI 180.0)))
