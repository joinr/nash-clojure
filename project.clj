(defproject nash-clojure "1.0.0-SNAPSHOT"
  :description "A Nash game analysis application"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main nash-clojure.core
  :dependencies [[org.clojure/clojure "1.10.1"],
                 [org.clojure/math.numeric-tower "0.0.4"],
                 [org.clojure/tools.cli "0.2.2"]]
  :profiles {:dev {:dependencies [[techascent/tech.viz "0.4.2"]
                                  [criterium "0.4.5"]]}})
