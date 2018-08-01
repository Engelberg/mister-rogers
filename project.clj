(defproject mister-rogers "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 ;; General-purpose tooling
                 [better-cond "2.0.1-SNAPSHOT"]
                 [medley "1.0.0"]
                 [com.rpl/specter "1.1.1"]
                 ;; Improved data structures
                 [org.clojure/data.int-map "0.2.4"]
                 ;; For simpler API namespacing
                 [potemkin "0.4.5"]
                 ;; For Java version
                 [org.jamesframework/james-core "1.2"]
                 [org.jamesframework/james-extensions "1.2"]
                 ;; For writing examples
                 [org.clojure/data.generators "0.1.2"]
                 [net.cgrand/xforms "0.18.2"]
                 [primitive-math "0.1.6"]
                 [prismatic/hiphip "0.2.1"]
                 [net.mikera/core.matrix "0.62.0"]
                 [net.mikera/vectorz-clj "0.47.0"]
                 ;; For reading data files for examples
                 [org.clojure/tools.reader "1.3.0"]
                 [semantic-csv "0.1.0"]
                 [clojure-csv/clojure-csv "2.0.1"]]
  :jvm-opts ^:replace ["-server"] 
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"])
  ;; :plugins [[lein-virgil "0.1.8"]])
  
  
                 

