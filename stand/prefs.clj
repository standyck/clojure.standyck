(ns
    #^{:author "Stan Dyck",
       :doc "A light-weight set of utilities for setting and accessing persistent, cross-platform  preference values.
  Sample usage:
    ;preference format
    (def test-data {\"path/to/node1\" {:a 3, :b true},
                   {\"path/to/node2\" {:value1 \"hello\", :value2 \"there\"}})

    ;load preferences
    (apply-prefs test-data)

    ;get a value
    (get-prefs-values \"path/to/node1\" :a) ;=> \"3\"

    ;[time passes...REPL is closed...come back tomorrow]
    (get-prefs-values \"path/to/node1\" :a) ;=> \"3\"

    ;get entire node of values
    (get-prefs-values \"path/to/node2\") ;=> {:value1 \"hello\", :value2 \"there\"} " }
  stand.prefs
  (:import (java.util.prefs Preferences)))

;;To use do something like (require '[stand.prefs :as p])

(def test-data {"path/to/node1" {:a 3, :b true},
                "path/to/node2" {:val1 "hello", :val2 "there"}})

(defn get-pref-node
  "Gets a Preference object for the specified path from the system node"
  [path]
  (let [root (Preferences/systemRoot)]
    (. root node path)))

(defn get-pref-values
  "Gets a value from a preference node. k should be a keyword."
  ([path]
     ;;return a map of all the prefs
     ;;at this node.
     (let [node (get-pref-node path)
           pref-keys (seq (. node keys))]
       (zipmap (map #(keyword %) pref-keys)
               (map #(get-pref-values path (keyword %)) pref-keys))))
  ([path k]
     ;;return the specific value at k or nil if is doesn't exist.
     (let [node (get-pref-node path)]
       (. node get (name k) nil))))


(defn set-pref-node
  "Assign a map of parameters to the specified preference node"
  [path pref-map]
  (let [node (get-pref-node path)
        nkeys (map #(name %) (keys pref-map))
        nvals (map #(str %) (vals pref-map))]
    (map #(. node put %1 %2) nkeys nvals)))


(defn apply-prefs
  "Assign a map of preference values to the specified node or nodes.
   The prefs-map should be of the form {\"node-path\" {:key value :key2 value2} \"node2\" {:key value}})"
  [prefs-map]
  (map #(set-pref-node %1 %2) (keys prefs-map) (vals prefs-map)))

(defn delete-prefs
  "Remove the preference value associated with the specified key at the specified node path."
  [path k]
  (let [node (get-pref-node path)]
    (. node remove (name k))))

