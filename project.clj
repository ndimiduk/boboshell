(defproject boboshell "1.0.0-SNAPSHOT"
  :description "FIXME: write"
  :repositories {"javax" "http://repository.jboss.com/maven2",
		 "jmx" "http://simile.mit.edu/maven"}
  :dev-dependencies [[swank-clojure "1.2.0-SNAPSHOT"]]
  :dependencies [[org.clojure/clojure "1.1.0"]
                 [org.clojure/clojure-contrib "1.1.0"]
		 [com.browseengine/bobo-browse "2.5.0-rc1"]
		 [org.apache.lucene/lucene-core "3.0.1"]
		 [fastutil/fastutil "5.0.9"]
		 [log4j "1.2.15"]
		 [org.springframework/spring "2.5.5"]]
  :namespaces [boboshell.core])

