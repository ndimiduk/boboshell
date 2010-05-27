(ns boboshell.core
  (:use [clojure.contrib.def :only (defvar-)])
  (:use [clojure.contrib.pprint :only (pprint)])
  (:import (java.io File)
	   (org.apache.lucene.analysis.standard StandardAnalyzer)
	   (org.apache.lucene.document Document)
	   (org.apache.lucene.index IndexReader)
	   (org.apache.lucene.queryParser QueryParser)
	   (org.apache.lucene.store NIOFSDirectory)
	   (org.apache.lucene.util Version)
	   (com.browseengine.bobo.api BoboBrowser BoboIndexReader BrowseFacet
				      BrowseHit BrowseRequest BrowseResult
				      FacetAccessible)))

;;; package private fun

(defvar- *pprint* (ref false))
(defvar- *idx-path* (ref nil))
(defvar- *l-reader* (ref nil))
(defvar- *b-reader* (ref nil))

(defn- set-index
  [path]
  (dosync
   (ref-set *idx-path* (.getAbsolutePath (File. path)))
   (when-not (nil? @*b-reader*)
     (ref-set *b-reader* nil))
   (when-not (nil? @*l-reader*)
     (.close @*l-reader*))
   (ref-set *l-reader*
	    (IndexReader/open (NIOFSDirectory/open (File. @*idx-path*)) true))
   (ref-set *b-reader*
	    (BoboIndexReader/getInstance @*l-reader*))))

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
  []
  (do
    (dosync
     (alter *pprint* not))
    @*pprint*))

(defn open-index
  [path]
  (do
    (set-index path)
    @*idx-path*))

(defn search
  [query default-field]
  (do
    (when (nil? @*b-reader*)
      (throw (IllegalStateException. "First open an index with (open-index path/to/idx)")))
    (let [pq (.parse (QueryParser. Version/LUCENE_29 default-field  (StandardAnalyzer. Version/LUCENE_29)) query)
	  request (doto (BrowseRequest.)
		    (.setCount 10)
		    (.setOffset 0)
		    (.setQuery pq))
	  result (browse-result->map @*l-reader* (.browse (BoboBrowser. @*b-reader*) request))]
      (if @*pprint*
	(pprint result)
	result))))
