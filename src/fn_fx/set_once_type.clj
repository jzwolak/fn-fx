(ns fn-fx.set-once-type
  (:import (clojure.lang ILookup)))


(defprotocol ISetOnce
  (set-once! [this k v]))

(defmacro defquasitype
  "Creates a type using deftype where the fields can be assigned a value once and then become immutable.
  The fields can be accessed using keywords (e.g., :field-name, :field2, :some-field, :size, etc.)"
  [tp-name fields & protos]
  {:pre [(symbol? tp-name)
         (every? symbol? fields)]}
  (let [fields (mapv (fn [sym]
                       (with-meta sym
                                  (assoc (meta sym) :volatile-mutable true)))
                     fields)
        v-sym  (gensym "v")]
    `(deftype ~tp-name ~fields
       ISetOnce
       (set-once! [this# k# ~v-sym]
         (assert (not (k# this#)) (str k# " is already set, can only set values once"))
         (case k#
           ~@(mapcat
               (fn [x]
                 `[~(keyword (name x))
                   (~'set! ~x ~v-sym)])
               fields))
         this#)

       ILookup
       (~'valAt [this# k# default#]
         (case k#
           ~@(mapcat
               (fn [x]
                 `[~(keyword (name x))
                   ~x])
               fields)
           default#))
       (~'valAt [this# k#]
         (.valAt this# k# nil))
       ~@protos)))
