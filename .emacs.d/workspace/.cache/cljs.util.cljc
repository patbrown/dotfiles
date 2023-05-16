;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cljs.util
  (:refer-clojure :exclude [boolean?])
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [clojure.edn :as edn])
  (:import [java.io File]
           [java.net URL URLDecoder]
           [java.security MessageDigest]))

;; next line is auto-generated by the build-script - Do not edit!
(def ^:dynamic *clojurescript-version* {:major 1, :minor 11, :qualifier 60})

(defn compilation-error [cause]
  (ex-info nil {:clojure.error/phase :compilation} cause))

(defn- main-src-directory []
  (some (fn [file]
          (when (= "main" (.getName file))
            file))
    (iterate (memfn getParentFile) (io/as-file (io/resource "cljs/util.cljc")))))

(defn- file-hash [file]
  (if (.isDirectory file)
    0
    (hash (slurp file))))

(def ^:private synthethetic-version-prefix "0.0.")

(def ^:private synthetic-clojurescript-version
  (delay (let [qualifier (fn [n]
                           (if (== n Integer/MIN_VALUE)
                             0
                             (Math/abs n)))]
           (str synthethetic-version-prefix
                (qualifier (reduce unchecked-add-int (map file-hash (file-seq (main-src-directory)))))))))

(defn ^String clojurescript-version
  "Returns clojurescript version as a printable string."
  []
  (if (bound? #'*clojurescript-version*)
    (str
     (:major *clojurescript-version*)
     "."
     (:minor *clojurescript-version*)
     (when-let [i (:incremental *clojurescript-version*)]
       (str "." i))
     (when-let [q (:qualifier *clojurescript-version*)]
       (str "." q))
     (when (:interim *clojurescript-version*)
       "-SNAPSHOT"))
    @synthetic-clojurescript-version))

(defn synthetic-version?
  "Returns true if clojurescript-version returns a synthetically-generated
   version."
  []
  (string/starts-with? (clojurescript-version) synthethetic-version-prefix))

(defn cljs-built-dep?
  "Returns true if ClojureScript itself is a built dep."
  []
  (not (synthetic-version?)))

(defn ^String compiled-by-version [f]
  (with-open [reader (io/reader f)]
    (let [match (some->> reader line-seq first
                         (re-matches #".*ClojureScript (\d+\.\d+\.\d+).*$"))]
      (or (and match (second match)) "0.0.0000"))))

(defn build-options [^File f]
  (with-open [reader (io/reader f)]
    (let [match (some->> reader line-seq first
                           (re-matches #".*ClojureScript \d+\.\d+\.\d+ (.*)$"))]
      (and match (edn/read-string (second match))))))

(defn munge-path [ss]
  (clojure.lang.Compiler/munge (str ss)))

(defn ns->relpath
  "Given a namespace as a symbol return the relative path. May optionally
  provide the file extension, defaults to :cljs."
  ([ns] (ns->relpath ns :cljs))
  ([ns ext]
   (ns->relpath ns ext \/))
  ([ns ext sep]
   (cond-> (string/replace (munge-path ns) \. sep)
     ext (str "." (name ext)))))

(defn ns->source
  "Given a namespace as a symbol return the corresponding resource if it exists."
  [ns]
  (or (io/resource (ns->relpath ns :cljs))
      (io/resource (ns->relpath ns :cljc))))

(defn path-seq
  [file-str]
  (->> File/separator
       java.util.regex.Pattern/quote
       re-pattern
       (string/split file-str)))

(defn to-path
  ([parts]
     (to-path parts File/separator))
  ([parts sep]
    (apply str (interpose sep parts))))

(defn split-paths
  [paths-str]
  (string/split paths-str (re-pattern File/pathSeparator)))

(declare ext)

(defn ^File to-target-file
  ([target-dir ns-info]
    (to-target-file target-dir ns-info "js"))
  ([target-dir {:keys [ns source-file] :as ns-info} ext]
    (let [src-ext (if source-file
                    (cljs.util/ext source-file)
                    "cljs")
          ns      (if (or (= src-ext "clj")
                          (and (= ns 'cljs.core) (= src-ext "cljc")))
                    (symbol (str ns "$macros"))
                    ns)
          relpath (string/split (munge-path (str ns)) #"\.")
          parents (cond-> (butlast relpath)
                    target-dir (conj target-dir))]
      (cond->> (io/file (str (last relpath) (str "." ext)))
        (seq parents)
        (io/file (to-path parents))))))

(defn mkdirs
  "Create all parent directories for the passed file."
  [^File f]
  (.mkdirs (.getParentFile (.getCanonicalFile f))))

(defn output-directory
  ([opts] (output-directory opts "out"))
  ([opts default]
   {:pre [(or (nil? opts) (map? opts))]}
   (or (:output-dir opts) default)))

(def windows?
  (.startsWith (.toLowerCase (System/getProperty "os.name")) "windows"))

(defn file? [f]
  (instance? File f))

(defn url? [f]
  (instance? URL f))

(defn ^String filename [^File f]
  (.getName f))

;; on Windows, URLs end up having forward slashes like
;; /C:/Users/... - Antonio
(defn ^String normalize-path [^String x]
  (-> (cond-> x
        windows? (string/replace #"^[\\/]" ""))
    (string/replace "\\" File/separator)
    (string/replace "/" File/separator)))

(defn ^String path [x]
  (cond
    (file? x) (.getAbsolutePath ^File x)
    (url? x) (if windows?
               (let [f (URLDecoder/decode (.getFile x))]
                 (normalize-path f))
               (.getPath ^URL x))
    (string? x) x
    :else (throw (Exception. (str "Expected file, url, or string. Got " (pr-str x))))))

(defn ^String ext
  "Given a file, url or string return the file extension."
  [x]
  (let [s (cond
            (file? x) (filename x)
            (url? x) (path x)
            (string? x) x
            :else (throw (Exception. (str "Expected file, url, or string. Got " (pr-str x)))))]
    (last (string/split s #"\."))))

(defn ^String get-name
  "Given a file or url return the last component of the path."
  [x]
  {:pre [(or (file? x) (url? x))]}
  (if (file? x)
    (filename x)
    (last (string/split (path x) #"[\\\/]"))))

(defn ^String relative-name
  "Given a file return a path relative to the working directory. Given a
   URL return the JAR relative path of the resource."
  [x]
  {:pre [(or (file? x) (url? x))]}
  (letfn [(strip-user-dir [s]
            (let [user-path (.toPath (io/file (System/getProperty "user.dir")))
                  base-count (.getNameCount user-path)
                  file-path (.toPath (io/file s))]
              (if (.startsWith file-path user-path)
                (str (.subpath file-path base-count (.getNameCount file-path)))
                s)))]
    (if (file? x)
      (strip-user-dir (.getAbsolutePath x))
      (let [f (URLDecoder/decode (.getFile x))]
        (if (string/includes? f ".jar!/")
          (last (string/split f #"\.jar!/"))
          (strip-user-dir f))))))

(defn last-modified [src]
  (cond
    (file? src) (.lastModified ^File src)
    (url? src)
    (let [conn (.openConnection ^URL src)]
      (try
        (.getLastModified conn)
        (finally
          (let [ins (.getInputStream conn)]
            (when ins
              (.close ins))))))
    :else
    (throw
      (IllegalArgumentException. (str "Cannot get last modified for " src)))))

(defn changed? [a b]
  (not (== (last-modified a) (last-modified b))))

(defn file-or-resource [s]
  (or (and (.exists (io/file s)) (io/file s))
      (io/resource s)))

(defn topo-sort
  ([x get-deps]
    (topo-sort x 0 (atom (sorted-map)) (memoize get-deps)))
  ([x depth state memo-get-deps]
    (let [deps (memo-get-deps x)]
      (swap! state update-in [depth] (fnil into #{}) deps)
      (doseq [dep deps]
        (topo-sort dep (inc depth) state memo-get-deps))
      (doseq [[<depth _] (subseq @state < depth)]
        (swap! state update-in [<depth] set/difference deps))
      (when (= depth 0)
        (distinct (apply concat (vals @state)))))))

(defn valid-js-id-start? [s]
  (re-find #"(?U)^[\p{Alpha}_$]" s))

(def debug-prn-mutex (Object.))

(defn debug-prn
  [& args]
  (binding [*out* *err*]
    (locking debug-prn-mutex
      (apply println args)
      (flush))))

(defmacro measure
  "Like cljs.core/time but toggleable and takes a message string."
  {:added "1.0"}
  ([msg expr] `(measure true ~msg ~expr))
  ([enable msg expr]
    `(if ~enable
       (let [start# (. System (nanoTime))
             ret# ~expr]
         (debug-prn (str ~msg ", elapsed time:") (/ (double (- (. System (nanoTime)) start#)) 1000000.0) "msecs")
         ret#)
       ~expr)))

(defmacro compile-if
  ([exp then] `(compile-if ~exp ~then nil))
  ([exp then else]
   (if (try (eval exp)
            (catch Throwable _ false))
     `(do ~then)
     `(do ~else))))

(defmacro compile-when
  [exp then]
  `(compile-if ~exp ~then nil))

(defn boolean? [x]
  (or (true? x) (false? x)))

(defn levenshtein-distance
  "The the minimum number of single-element edits needed to
  transform s in to t."
  [s t]
  (let [f (fn [f s t]
            (cond
              (empty? s) (count t)
              (empty? t) (count s)
              :else (let [cost (if (= (first s) (first t))
                                 0
                                 1)]
                      (min (inc (f f (rest s) t))
                        (inc (f f s (rest t)))
                        (+ cost (f f (rest s) (rest t)))))))
        g (memoize f)]
    (g g s t)))

(defn suggestion
  "Provides a best suggestion for an unknown, taken from knowns,
  minimizing the Levenshtein distance, returning nil if threshold
  cannot be satisfied."
  [threshold unknown knowns]
  (let [distance     (partial levenshtein-distance unknown)
        closest      (apply min-key distance knowns)
        closest-dist (distance closest)]
    (when (<= closest-dist threshold)
      closest)))

(defn unknown-opts
  "Takes a set of passed opt keys and known opt keys and for each
  unknown opt key returns a vector of the key and its (potentially
  nil) suggestion."
  [passed knowns]
  {:pre [(set? passed) (set? knowns)]}
  (for [unknown (set/difference passed knowns)]
    [unknown (some-> (suggestion 3 (str unknown) (map str knowns))
               (subs 1)
               keyword)]))

(defn distinct-by
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (let [v (f x)]
                         (if (contains? seen v)
                           (recur (rest s) seen)
                           (cons x (step (rest s) (conj seen v)))))))
                    xs seen)))]
     (step coll #{}))))

(def ^:private hex-digits (char-array "0123456789ABCDEF"))

(defn- bytes-to-hex-str
  "Convert an array of bytes into a hex encoded string."
  [^bytes bytes]
  (loop [index (int 0)
         buffer (StringBuilder. (int (* 2 (alength bytes))))]
    (if (== (alength bytes) index)
      (.toString buffer)
      (let [byte (aget bytes index)]
        (.append buffer (aget ^chars hex-digits (bit-and (bit-shift-right byte 4) 0xF)))
        (.append buffer (aget ^chars hex-digits (bit-and byte 0xF)))
        (recur (inc index) buffer)))))

(defn content-sha
  ([^String s]
   (content-sha s nil))
  ([^String s ^Long n]
   (let [digest (MessageDigest/getInstance "SHA-1")
         _ (.reset digest)
         _ (.update digest (.getBytes s "utf8"))
         sha (bytes-to-hex-str (.digest digest))]
     (if-not (nil? n)
       (apply str (take n sha))
       sha))))

(defn map-merge [a b]
  (if (and (map? a) (map? b))
    (loop [ks (seq (keys a)) ret a b' b]
      (if ks
        (let [k (first ks)]
          (if (contains? b' k)
            (recur
              (next ks)
              (assoc ret k (map-merge (get ret k) (get b' k)))
              (dissoc b' k))
            (recur (next ks) ret b')))
        (merge ret b')))
    a))

(defn conjunction-str [xs]
  (let [xs (vec xs)]
    (case (count xs)
      1 (first xs)
      2 (str (first xs) " and " (second xs))
      (str (string/join ", " (pop xs)) " and " (peek xs)))))

(defn module-file-seq
  "Return a seq of all files in `node_modules` ending in `.js` or `.json` that are
   not in an internally nested `node_modules` dir."
  ([] (module-file-seq (io/file "node_modules")))
  ([dir]
   (let [fseq (tree-seq
                (fn [^File f]
                  ;; ignore embedded node_modules, the user did not install
                  ;; these
                  (and (. f (isDirectory))
                       (not (boolean
                              (re-find #"node_modules[\\\/].*[\\\/]node_modules"
                                (.getPath f))))))
                (fn [^File d]
                  (seq (. d (listFiles))))
                dir)]
     (filter (fn [^File f]
               (let [path (.getPath f)]
                 (or (.endsWith path ".json")
                     (.endsWith path ".js"))))
       fseq))))