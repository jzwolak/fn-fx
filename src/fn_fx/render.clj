(ns fn-fx.render
  (:import (javafx.embed.swing JFXPanel)
           (javax.swing JFrame)
           (javafx.application Application)
           (javafx.scene Scene Node)
           (javafx.stage Stage)
           (javafx.util Builder)
           (java.lang.reflect Method Modifier)
           (javafx.collections ObservableList)
           (javafx.event EventHandler)
           (javafx.collections FXCollections)
           (java.util WeakHashMap)
           (org.reflections Reflections))
  (:require [fn-fx.util :as util]
            [fn-fx.diff :as diff]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)

(JFXPanel.)

(def constructors (atom {}))
(def types (atom {}))
(def types->kw (atom {}))

(def ^:dynamic *log* false)

(defn log [form]
  (when *log*
    (println form)))

(defn scan-all []
  (let [ref      (Reflections. "javafx" nil)
        builders (.getSubTypesOf ref Builder)]
    (vec (for [^Class builder builders
               :when (not (.startsWith (.getName builder)
                                       "javafx.fxml."))]
           (Class/forName (subs (.getName builder) 0
                                (- (count (.getName builder))
                                   (count "Builder"))))))))


(declare create-component)
(declare convert-observable-list)
(declare create-event-handler)

(def converter-code (atom (partition-all 2 [Integer/TYPE `int
                                            java.lang.Integer `int
                                            Double/TYPE `double
                                            String `str
                                            EventHandler `create-event-handler
                                            ObservableList `convert-observable-list])))

(defn import-components [components]
  (doseq [component components]
    (let [component    (if (instance? Class component)
                         (symbol (.getName ^Class component))
                         component)
          klass        (Class/forName (name component))
          builder-name (symbol (str (name component) "Builder"))
          kw-name      (->> (.lastIndexOf (name component) ".")
                            inc
                            (subs (name component))
                            util/camel->kabob
                            keyword)]
      (swap! converter-code conj [klass identity])
      (swap! constructors assoc kw-name (Class/forName (name builder-name)))
      (swap! types assoc kw-name klass)
      (swap! types->kw assoc klass kw-name))))

;; Import everything inside JavaFX that we are aware of
(import-components
  (scan-all))


(def default-properties
  {:scene
   {:root (fn [] (javafx.scene.layout.StackPane.))}})

(def synthetic-properties
  {:stage
   {:shown {:set (fn [^Stage stage property val]
                   (if val
                     (.show stage)
                     (.hide stage)))}}})


(def get-enum-converter
  (memoize (fn [^Class to-tp]
             (let [clauses (mapcat
                             (fn [o]
                               `[~(keyword (string/replace (.toLowerCase ^String (str o))
                                             #"\_" "-"))
                                 ~(symbol (.getName to-tp) (str o))])
                             (.getEnumConstants to-tp))
                   fn-name (symbol (str "-enum-converter-" (munge (.getName to-tp))))
                   form    `(fn ~fn-name [v#]
                              (case v#
                                ~@clauses
                                :else (assert false (str "Bad enum value " v#))))]
               (log form)
               (eval form)))))


(def get-converter
  (memoize (fn [^Class to-tp]
             (if (.isEnum to-tp)
               `(get-enum-converter ~to-tp)
               (first (keep (fn [[^Class klass converter]]
                              (when (.isAssignableFrom to-tp klass)
                                converter))
                            @converter-code))))))



(def get-builder
  (memoize
    (fn [nm]
      (let [ctor (@constructors nm)
            _    (assert ctor (str "No constructor for " (pr-str nm)))
            form `(fn [] (. ~(@constructors nm) create))]
        (log form)
        (eval form)))))


; this is never used. The only reference to it is inside a when-let and presumbably this statement is always false since
; the body of the when-let requires *id-map* to be defined as a WeakHashMap and never is defined.
(def ^:dynamic *id-map*)


(def get-builder-setter
  (memoize
    (fn [tp]
      (let [^Class ctor (@constructors tp)
            builder-sym (with-meta (gensym "builder")
                                   {:tag (symbol (.getName ctor))})
            val-sym     (gensym "val")
            clauses     (for [m (.getMethods ctor)
                              :when (Modifier/isPublic (.getModifiers ^Method m))
                              :when (not (Modifier/isStatic (.getModifiers ^Method m)))
                              :when (not (Modifier/isVolatile (.getModifiers ^Method m)))
                              :when (= (.getParameterCount ^Method m) 1)
                              :when (not (#{"applyTo"} (.getName ^Method m)))
                              :let [arg-type (aget (.getParameterTypes ^Method m) 0)]
                              :let [converter (get-converter arg-type)]
                              :when converter]
                          `[~(keyword (util/camel->kabob (.getName ^Method m)))
                            (. ~builder-sym
                               ~(symbol (.getName ^Method m))
                               ~(with-meta `(~converter ~val-sym)
                                           {:tag arg-type}))])
            form        `(fn [~builder-sym property# ~val-sym]
                           (case property#
                             ~@(apply concat clauses)
                             (println "Unknown builder property" property# "on" ~tp)
                             #_(assert false (str "Unknown property" {:property-name property#}))))]
        (log form)
        (eval form)))))

(def primitive-properties #{Integer Long Double Float String BigDecimal BigInteger
                            EventHandler Integer/TYPE Long/TYPE Double/TYPE Float/TYPE})

(def children-properties
  (memoize
    (fn [^Class tp]
      (set (for [m (.getMethods tp)
                 :when (.startsWith (.getName ^Method m) "get")
                 :let [arg-type (.getReturnType ^Method m)]
                 :let [converter (get-converter arg-type)]
                 :when (and (not (primitive-properties arg-type))
                         (not (.isEnum arg-type)))
                 :let [prop-name (let [mn (subs (.getName ^Method m) 3)]
                                   (str (.toLowerCase (subs mn 0 1)) (subs mn 1)))]
                 :when converter]
             (keyword prop-name))))))



(def get-setter
  (memoize
    (fn [tp]
      (assert tp)
      (let [^Class tp tp
            obj-sym   (with-meta (gensym "obj")
                                 {:tag (symbol (.getName tp))})
            val-sym   (gensym "val")
            clauses   (for [m (.getMethods tp)
                            :when (= (.getParameterCount ^Method m) 1)
                            :when (.startsWith (.getName ^Method m) "set")
                            :let [arg-type (aget (.getParameterTypes ^Method m) 0)]
                            :let [converter (get-converter arg-type)]
                            :let [prop-name (let [mn (subs (.getName ^Method m) 3)]
                                              (str (.toLowerCase (subs mn 0 1)) (subs mn 1)))]
                            :when converter]
                        `[~(keyword prop-name)
                          (. ~obj-sym
                             ~(symbol (.getName ^Method m))
                             ~(with-meta `(~converter ~val-sym)
                                         {:tag arg-type}))])

            form      `(fn [~obj-sym property# ~val-sym]
                         (case property#
                           ~@(apply concat clauses)
                           (let [syn# (get-in synthetic-properties [(@types->kw ~tp) property# :set])]
                             (if syn#
                               (syn# ~obj-sym property# ~val-sym)
                               (println "Unknown setter property" property# " on " ~tp)))))]

        (log form)
        (eval form)))))

(def get-static-setter
  (memoize
    (fn [tp]
      (assert tp)
      (let [^Class tp tp
            child-sym (gensym "child")
            val-sym   (gensym "val")
            clauses   (for [m (.getMethods tp)
                            :when (Modifier/isStatic (.getModifiers ^Method m))
                            :when (= (.getParameterCount ^Method m) 2)
                            :when (.startsWith (.getName ^Method m) "set")
                            :let [arg-type0 (aget (.getParameterTypes ^Method m) 0)
                                  arg-type1 (aget (.getParameterTypes ^Method m) 1)
                                  converter (get-converter arg-type1)
                                  prop-name (let [mn (subs (.getName ^Method m) 3)]
                                              (util/camel->kabob (str (.toLowerCase (subs mn 0 1)) (subs mn 1))))]
                            :when converter]
                        `[~prop-name
                          (~(symbol (.getName tp)
                                    (.getName ^Method m))
                            ~(with-meta child-sym
                                        {:tag arg-type0})
                            ~(with-meta `(~converter ~val-sym)
                                        {:tag arg-type1}))])

            form      `(fn [~child-sym property# ~val-sym]
                         (case (name property#)
                           ~@(apply concat clauses)
                           (println "Unknown static property" property# "on" ~tp)))]

        (log form)
        (eval form)))))

(def get-getter
  (memoize
    (fn [tp]
      (let [^Class tp tp
            obj-sym   (with-meta (gensym "obj")
                                 {:tag (symbol (.getName tp))})
            clauses   (for [m (.getMethods tp)
                            :when (= (.getParameterCount ^Method m) 0)
                            :when (.startsWith (.getName ^Method m) "get")
                            :when (not (contains? #{"getClass"} (.getName ^Method m)))
                            :let [prop-name (let [mn (subs (.getName ^Method m) 3)]
                                              (str (.toLowerCase (subs mn 0 1)) (subs mn 1)))]]
                        `[~(keyword prop-name)
                          (. ~obj-sym
                             ~(symbol (.getName ^Method m)))])
            form      `(fn [~obj-sym property#]
                         (case property#
                           ~@(apply concat clauses)
                           (println "Unknown property" property#)))]
        (log form)
        (eval form)))))


(def ^:dynamic *handler-fn*)

(defn create-event-handler [{:keys [include event-properties] :as template}]
  (let [handler-fn *handler-fn*]
    (reify EventHandler
      (handle [this event]
        (future
          (handler-fn template))))))


(def get-builder-build-fn
  (memoize
    (fn [tp]
      (let [^Class ctor (@constructors tp)
            builder-sym (with-meta (gensym "builder")
                                   {:tag (symbol (.getName ctor))})
            form        `(fn [~builder-sym]
                           (.build ~builder-sym))]
        (log form)
        (eval form)))))


(defn convert-observable-list [itms]
  (if (instance? ObservableList itms)
    itms
    (FXCollections/observableArrayList ^java.util.Collection (mapv create-component itms))))

(def ignore-properties #{:type :fn-fx/children :fn-fx/id})

(defn create-component [tp spec]
  {:pre [(keyword? tp)]}
  (let [builder-factory (get-builder tp)
        _       (assert builder-factory (str "Can't find constructor for" tp (pr-str spec)))
        builder (builder-factory)
        setter  (get-builder-setter tp)
        synths (synthetic-properties tp)]
    (reduce-kv
      (fn [_ k v]
        (when (and (not (ignore-properties k))
                   (not (contains? synths k))
                   (not (namespace k)))
          (setter builder k v)))
      nil
      spec)
    ;; Fill in default properties
    (reduce-kv
      (fn [_ k v]
        (when-not (k spec)
          (setter builder k (v))))
      nil
      (default-properties tp))
    (let [built        ((get-builder-build-fn tp) builder)
          built-setter (get-setter (@types tp))]
      (reduce-kv
        (fn [_ k v]
          (when (contains? synths k)
            (built-setter built k v)))
        nil
        spec)
      (reduce-kv
        (fn [_ k v]
          (when (and (not (ignore-properties k))
                     (namespace k))
            (let [tp (->> k namespace keyword (get @types))]
              (assert tp (str "No static setter found for " (namespace k)))
              ((get-static-setter tp) built (name k) v))))
        nil
        spec)
      (when-let [id (:fn-fx/id spec)]
        (.put ^WeakHashMap *id-map* id built))
      built)))

(defn ui
  "Creates a virtual user interface node. This does not correspond to an actual JavaFX component but is instead used
  to render a JavaFX component (or user defined component). You might refer to this as a user interface \"spec\"
  and it includes the values from the application state. Everything needed to render the UI is returned by this
  function. There is a field in the returned type instance to reference the JavaFX node. This field is set when
  a diff is run and points to the actual UI components rendered from this spec. Note: JavaFX may be the UI toolkit
  used, but this code is more generic and would work with other UI toolkits if a suitable IDom type is supplied to
  the diff function."
  [type & {:as props}]
  (let [props-col (children-properties (@types type))
        grouped (reduce-kv
                  (fn [acc k v]
                    (assoc-in acc [(contains? props-col k) k] v))
                  {}
                  props)]
    (diff/component type (grouped false) (grouped true))))


