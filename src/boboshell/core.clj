(ns boboshell.core
  (:import (org.apache.lucene.document Document)
	   (org.apache.lucene.index IndexReader)
	   (com.browseengine.bobo.api BrowseFacet BrowseHit BrowseResult
				      FacetAccessible)
	   (com.browseengine.bobo.facets.impl SimpleFacetHandler)))

(defn term-counts-by-field
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

(defn total-term-count
  [counts]
  (reduce (fn [c [_ v]] (+ c v)) 0 counts))

;;; fns pulled from clobo

(defn document->map
  "Convert a Lucene Document into a clojure map."
  [#^Document document]
  (if (nil? document)
    {}
    (-> (into {}
	      (for [f (.getFields document)]
		[(keyword (.name f)) (.stringValue f)]))
	(dissoc :_content))))

(defn browsehit->map
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

(defn facet-map->map
  "Convert a facet result map into a clojure map."
  [#^Map facet-map]
  (into []
	(for [[#^String name #^FacetAccessible facet-list] facet-map]
	  {"facet" name
	   "values" (into []
			  (for [#^BrowseFacet facet (.getFacets facet-list)]
			    {"value" (.getValue facet)
			     "count" (.getHitCount facet)}))})))

(defn browse-result->map
  "Convert a BrowseResult into a clojure map."
  [#^BrowseResult result]
  (into {}
	[[:num-hits (.getNumHits result)]
	 [:hits (apply vector
		       (for [hit (.getHits result)]
			 (browsehit->map hit)))]
	 [:facets (facet-map->map (.getFacetMap result))]]))
