(defproject nms-recipe-parser "0.1.0-SNAPSHOT"
  :description "NMS recipe database parser"
  :url "FIXME: add a correct url"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v20.html"}

  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/test.check "1.1.0"]
                 [com.grammarly/omniconf "0.4.2"]
                 [com.taoensso/timbre "5.1.0"]
                 [com.taoensso/tufte "2.2.0"]]

  :plugins [[lein-environ "1.1.0"]]

  :min-lein-version "2.5.0"

  :profiles {:dev {:dependencies [[nrepl "0.7.0"]
                                  [pjstadig/humane-test-output "0.10.0"]]

                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :env {:dev true}}

             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :env {:production true}
                       :aot :all
                       :omit-source true}})
