(ns sportsball.core
  (:require [org.httpkit.client :as http-client]
            [clojure.data.json  :as json]
            [java-time :as jt])
  (:gen-class))

;; https://github.com/toddrob99/MLB-StatsAPI/blob/master/statsapi/endpoints.py


(defn mlb-fetch
  "Retrieve data from MLB Stats API"
  [endpoint query]
  (let [base     "http://statsapi.mlb.com/api/v1"
        url      (str base endpoint)
        options  {:query-params query}
        response @(http-client/get url options)]
    (json/read-str (:body response)
                   :key-fn keyword)))

(defn iso8601z-to-dt [s]
  (let [ios8601-format-string "yyyy-MM-dd'T'HH:mm:ss'Z'"]
    (jt/zoned-date-time
     (jt/local-date-time iso8601-format-string s)
     "UTC")))

(defn dt-to-tzdt [dt tz]
  (jt/with-zone-same-instant dt tz))

(defn tzdt-to-clock-time [tzdt]
  (let [clock-time-fmt "h:mma"]
    (jt/format clock-time-fmt tzdt)))

(defn iso-to-tzclock [s tz]
  (-> s
      (iso8601z-to-dt)
      (dt-to-tzdt tz)
      (tzdt-to-clock-time)))

(defn schedule-to-game [schedule]
  (-> schedule :dates first :games first))

(defn remap-team [team]
  {:name   (-> team :team :name)
   :wins   (-> team :leagueRecord :wins)
   :losses (-> team :leagueRecord :losses)
   :score  (-> team :score)
   :winner (-> team :isWinner)})

(defn game-teams [game]
  (let [away (-> game :teams :away)
        home (-> game :teams :home)]
    {:away (remap-team away)
     :home (remap-team home)}))

(defn dispatch-game-status [game]
  (-> game :status :detailedState))

(defmulti report dispatch-game-status)

(defmethod report "In Progress" [game]
  (let [{:keys [away home]} (game-teams game)
        time                (:gameDate game)]
    (str "In Progress: "
         (:name away) " "
         (:score away) " runs @ "
         (:name home) " "
         (:score home) " runs.")))

(defmethod report "Scheduled" [game]
  (let [{:keys [away home]} (game-teams game)
        time                (:gameDate game)]
    (str "Scheduled: "
         (:name away) " ("
         (:wins away) ":"
         (:losses away) ") @ "
         (:name home) " ("
         (:wins home) ":"
         (:losses home) ") "
         " at "
         (iso-to-tzclock time "America/Chicago") " Central.")))

(defmethod report "Final" [game]
  (let [{:keys [away home]} (game-teams game)
        time                (:gameDate game)]
    (str "Final: "
         (if (:winner away)
           (.toUpperCase (:name away))
           (:name away)) " "
         (:score away) " runs @ "
         (if (:winner home)
           (.toUpperCase (:name home))
           (:name home)) " "
         (:score home) " runs.")))

(defn cards []
  (-> (mlb-fetch "/schedule/games" {:sportId "1"
                                    :teamId  "138"})
      (schedule-to-game)
      (report)))

(defn -main
  "Play with the MLB API"
  [& args]
  (println
   (mlb-fetch "/schedule/games" {:sportId "1"
                                 :teamId  "138"})))