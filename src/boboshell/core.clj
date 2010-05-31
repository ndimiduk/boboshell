(ns boboshell.core
  (:use [clojure.contrib.def :only (defvar-)])
  (:use [clojure.contrib.pprint :only (pprint)])
  (:import (java.io File)
	   (java.util Arrays)
	   (org.apache.lucene.analysis.standard StandardAnalyzer)
	   (org.apache.lucene.document Document)
	   (org.apache.lucene.index IndexReader)
	   (org.apache.lucene.queryParser QueryParser)
	   (org.apache.lucene.store NIOFSDirectory)
	   (org.apache.lucene.util Version)
	   (com.browseengine.bobo.api BoboBrowser BoboIndexReader BrowseFacet
				      BrowseHit BrowseRequest BrowseResult
				      FacetAccessible FacetSpec)
	   (com.browseengine.bobo.facets.impl SimpleFacetHandler)))

;;; package private fun

(defvar- *pprint* (ref false))
(defvar- *idx-path* (ref nil))
(defvar- *l-reader* (ref nil))
(defvar- *b-reader* (ref nil))

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

(defn- term-counts-by-field
  [#^IndexReader r]
  (loop [terms (.terms r)
	 more (.next terms)
	 acc {}]
    (if more
      (let [term (.term terms)
	    field (.field term)]
	(recur terms (.next terms) (if (nil? (get acc field))
				     (assoc acc field 1)
				     (assoc acc field (inc (get acc field))))))
      acc)))

(defn- total-term-count
  [counts]
  (reduce (fn [c [_ v]] (+ c v)) 0 counts))

;;; fns pulled from clobo

(defn- document->map
  "Convert a Lucene Document into a clojure map."
  [#^Document document]
  (if (nil? document)
    {}
    (-> (into {}
	      (for [f (.getFields document)]
		[(keyword (.name f)) (.stringValue f)]))
	(dissoc :_content))))

(defn- browsehit->map
  "Convert a BrowseHit into a clojure map."
  [#^BrowseHit hit]
  (into {}
	[[:docid (.getDocid hit)]
	 [:fields (into (document->map (.getStoredFields hit))
			(for [[name values] (.getFieldValues hit)]
			  (let [raw-value (first (seq values))]
			    (try
			      [name (Integer/parseInt raw-value)]
			      (catch NumberFormatException e
				(try
				  [name (Double/parseDouble raw-value)]
				  (catch NumberFormatException e
				    [name raw-value])))))))]
;			  [name (first (seq values))]))]
	 [:score (.getScore hit)]]))

(defn- facet-map->map
  "Convert a facet result map into a clojure map."
  [#^Map facet-map]
  (into []
	(for [[#^String name #^FacetAccessible facet-list] facet-map]
	  {"facet" name
	   "values" (into []
			  (for [#^BrowseFacet facet (.getFacets facet-list)]
			    {"value" (.getValue facet)
			     "count" (.getHitCount facet)}))})))

(defn- browse-result->map
  "Convert a BrowseResult into a clojure map."
  [#^IndexReader reader #^BrowseResult result]
  (into {}
	[[:num-hits (.getNumHits result)]
	 [:hits (apply vector
		       (for [hit (.getHits result)]
			 (browsehit->map hit)))]
	 [:facets (facet-map->map (.getFacetMap result))]]))

;;; public interface

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

(defn relative-field-sizes
  "Retrieve a map of field names to (term count) size relative to overall index."
  []
  (let [counts (term-counts-by-field @*l-reader*)
	sum (total-term-count counts)]
    (-> (into {}
	      (map (fn [[field count]]
		     [field (float (/ count sum))])
		   counts)))))

(defn field-names
  []
  (keys (relative-field-sizes)))

(defn largest-field
  "Retrieve the name and relative size of the largest field in the index."
  []
  (reduce (fn [[k1 v1] [k2 v2]]
	    (if (> v1 v2) [k1 v1] [k2 v2]))
	  (relative-field-sizes)))

(defn search
  "Search against the opened index."
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
	     result (browse-result->map @*l-reader* (.browse (BoboBrowser. @*b-reader*) request))]
	 (if @*pprint*
	   (pprint result)
	   result)))))

