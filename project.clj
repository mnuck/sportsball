(defproject sportsball "0.1.0-SNAPSHOT"
  :description "Sandbox to goof off with the sports API"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [http-kit "2.3.0"]
                 [org.clojure/data.json "0.2.6"]
                 [clojure.java-time "0.3.2"]]
  :main ^:skip-aot sportsball.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})