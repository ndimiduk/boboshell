(ns boboshell.bobo
  (:use [boboshell.core]
	[clojure.contrib.def :only (defvar-)]
	[clojure.contrib.pprint :only (pprint)])
  (:import (java.io File)
	   (java.util Arrays)
	   (org.apache.lucene.analysis.standard StandardAnalyzer)
	   (org.apache.lucene.index IndexReader)
	   (org.apache.lucene.queryParser QueryParser)
	   (org.apache.lucene.store NIOFSDirectory)
	   (org.apache.lucene.util Version)
	   (com.browseengine.bobo.api BoboBrowser BoboIndexReader BrowseRequest
				      FacetSpec)
	   (com.browseengine.bobo.facets.impl SimpleFacetHandler)))

;;; package private fun

(defvar- *pprint* (ref false))
(defvar- *idx-path* (ref nil))
(defvar- *l-reader* (ref nil))
(defvar- *b-reader* (ref nil))

(defn relative-field-sizes
  "Retrieve a map of field names to (term count) size relative to overall index."
  []
  (let [counts (term-counts-by-field @*l-reader*)
	sum (total-term-count counts)]
    (-> (into {}
	      (map (fn [[field count]]
		     [field (float (/ count sum))])
		   counts)))))

(defn largest-field
  "Retrieve the name and relative size of the largest field in the index."
  []
  (reduce (fn [[k1 v1] [k2 v2]]
	    (if (> v1 v2) [k1 v1] [k2 v2]))
	  (relative-field-sizes)))

(defn- set-index
  [path facet-handlers]
  (dosync
   (ref-set *idx-path* (.getCanonicalPath (File. path)))
   (when-not (nil? @*b-reader*)
     (ref-set *b-reader* nil))
   (when-not (nil? @*l-reader*)
     (.close @*l-reader*))
   (ref-set *l-reader*
	    (IndexReader/open (NIOFSDirectory/open (File. @*idx-path*)) true))
   (let [facets (if-not (nil? facet-handlers)
		  facet-handlers
		  (map (fn [[k v]]
			 (SimpleFacetHandler. k))
		       (dissoc (relative-field-sizes) (largest-field))))]
     (ref-set *b-reader*
	      (BoboIndexReader/getInstance @*l-reader* (Arrays/asList (into-array facets)))))))

(defn pretty-print
  "Toggle between returning search result objects and pretty-printing search results."
  []
  (do
    (dosync
     (alter *pprint* not))
    @*pprint*))

(defn open-index
  "Prepare the environment for searching and browsing on a lucene persisted index."
  [path & facet-handlers]
  (do
    (set-index path facet-handlers)
    @*idx-path*))

(defn field-names
  "List index field names."
  []
  (keys (relative-field-sizes)))

(defn search
  #^{:arglists '([query] [query default-field] [ facet-spec-map] [query default-field facet-spec-map])
     :doc "Search against the opened index."}
  ([query]
     (search query (first (largest-field)) {}))
  ([query default-field-or-facet-map]
     (let [default-field (if (instance? String default-field-or-facet-map)
			   default-field-or-facet-map
			   (first (largest-field)))
	   facet-spec-map (if (map? default-field-or-facet-map)
			    default-field-or-facet-map
			    {})]
       (search query default-field facet-spec-map)))
  ([query default-field facet-spec-map]
     (do
       (when (nil? @*b-reader*)
	 (throw (IllegalStateException. "First open an index with (open-index path/to/idx)")))
       (let [pq (.parse (QueryParser. Version/LUCENE_29 default-field (StandardAnalyzer. Version/LUCENE_29))
			query)
	     request (doto (BrowseRequest.)
		       (.setCount 10)
		       (.setOffset 0)
		       (.setQuery pq)
		       (.setFacetSpecs facet-spec-map))
	     result (browse-result->map (.browse (BoboBrowser. @*b-reader*) request))]
	 (if @*pprint*
	   (pprint result)
	   result)))))

