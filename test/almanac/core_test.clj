(ns almanac.core-test
  (:require
    [almanac.core :as core]
    [almanac.testlib :refer [approx?]]
    [clj-time.core :as t]
    [clojure.test :refer [is deftest]]))

(def portland-lattitude 45.515)
(def portland-longitude -122.678)

(def portland-sunrise-and-sunset-data
  [[(t/date-time 2024 1 1) -8 "7:50" "16:37"]
   [(t/date-time 2024 2 1) -8 "7:32" "17:16"]
   [(t/date-time 2024 3 1) -8 "6:47" "17:58"]
   [(t/date-time 2024 4 1) -7 "6:49" "19:39"]
   [(t/date-time 2024 5 1) -7 "5:57" "20:18"]
   [(t/date-time 2024 6 1) -7 "5:25" "20:52"]
   [(t/date-time 2024 7 1) -7 "5:26" "21:02"]
   [(t/date-time 2024 8 1) -7 "5:55" "20:37"]
   [(t/date-time 2024 9 1) -7 "6:32" "19:47"]
   [(t/date-time 2024 10 1) -7 "7:09" "18:49"]
   [(t/date-time 2024 11 1) -7 "7:51" "17:56"]
   [(t/date-time 2024 12 1) -8 "7:31" "16:28"]])

(defn- hm-str->minutes [s]
  (let [regex #"(\d{1,2}):(\d{2,2})"
        [_ h-str m-str] (re-matches regex s)
        h (Integer/parseInt h-str)
        m (Integer/parseInt m-str)]
    (+ (* 60 h) m)))

(defn- minutes-since-midnight [dt]
  (let [s (str dt)
        regex #".+T(\d{2,2}):(\d{2,2}):(\d{2,2})\.\d{3,3}Z"
        [_ h-str m-str s-str] (re-matches regex s)
        h (Integer/parseInt h-str)
        m (Integer/parseInt m-str)
        s (Integer/parseInt s-str)]
    (+ (* 60.0 h) m (/ s 60.0))))

(deftest computing-minutes-since-midnight
  (let [got (minutes-since-midnight (t/date-time 2024 2 4 1 3 40))]
    (is (approx? 0.01 63.67 got))))

(deftest computing-time-of-day-str
  (is (= "1:03" (#'core/time-of-day-str (t/date-time 2024 2 4 1 3 4)))))

(defn compute-errors [& {:keys [coarse-sunrise-equation circular-orbit]}]
  (for [[date
         timezone
         sunrise-want-str
         sunset-want-str] portland-sunrise-and-sunset-data
        :let [config (core/map->Config
                       {:lattitude portland-lattitude
                        :longitude portland-longitude
                        :time-zone timezone
                        :date date
                        :coarse-sunrise-equation coarse-sunrise-equation
                        :circular-orbit circular-orbit})
              [sunrise-got
               sunset-got] (map #(t/plus % (t/hours timezone))
                                (#'core/calculate-sunrise-and-sunset config))
              [sunrise-got-minutes
               sunset-got-minutes] (->> [sunrise-got
                                         sunset-got]
                                        (map minutes-since-midnight))
              [sunrise-want
               sunset-want] (map hm-str->minutes [sunrise-want-str
                                                  sunset-want-str])
              sunrise-error (- sunrise-got-minutes sunrise-want)
              sunset-error (- sunset-got-minutes sunset-want)
              day-length-error (- sunset-error sunrise-error)]]
    {:date (subs (str date) 0 10)
     :sunrise-got (#'core/time-of-day-str sunrise-got)
     :sunrise-want sunrise-want-str
     :sunset-got (#'core/time-of-day-str sunset-got)
     :sunset-want sunset-want-str
     :sunrise-error sunrise-error
     :sunset-error sunset-error
     :day-length-error day-length-error}))

(defn mean-absolute-value [xs]
  (let [n (count xs)
        abs-err (->> xs
                     (map #(Math/abs %))
                     (reduce + 0.0))]
    (/ abs-err n)))

(defn compute-mae [& {:keys [coarse-sunrise-equation
                             circular-orbit]}]
  (let [errors (compute-errors :coarse-sunrise-equation coarse-sunrise-equation
                               :circular-orbit circular-orbit)]
    {:sunrise-error (mean-absolute-value (map :sunrise-error errors))
     :sunset-error (mean-absolute-value (map :sunset-error errors))
     :day-length-error (mean-absolute-value (map :day-length-error errors))}))

(comment
  (def errors (compute-errors :coarse-sunrise-equation false
                              :circular-orbit false ;;true
                              ))
  (def stats
    (vec (for [coarse-sunrise-equation [false true]
               circular-orbit [false true]]
           (assoc (compute-mae :coarse-sunrise-equation coarse-sunrise-equation
                               :circular-orbit circular-orbit)
             :coarse-sunrise-equation coarse-sunrise-equation
             :circular-orbit circular-orbit))))
  ;;
  )
