(ns gltf
  "Zero-dep portable CLJC. Restored from the legacy kami-engine/kami-gltf Rust
  crate (kotoba-lang/kami-engine, deleted in PR #82 \"Remove Rust workspace
  from kami-engine\") as part of the clj-wgsl migration (ADR-2607010930,
  com-junkawasaki/root).

  Purpose: minimal binary glTF 2.0 (.glb) *writer* — takes a `LoadedMesh`-shaped
  map (interleaved [pos3 norm3 uv2] × N float vertices + u32 indices) plus a
  flat RGBA base color, and emits a self-contained .glb byte sequence (JSON
  chunk + BIN chunk, 4-byte aligned, per the glTF 2.0 binary container spec).
  No external glTF/JSON dependency — the JSON chunk is hand-serialized here,
  matching the original Rust's `serde_json::json!` literal shape exactly.

  Despite the crate name suggesting a *loader*, the original `kami-gltf/src/lib.rs`
  contained only an *exporter* (`export_glb`) with no parsing/loading logic —
  this port is faithful to that (there was nothing else to port).

  Platform divergence (f32 <-> IEEE-754 le bytes, byte-array construction) is
  isolated behind `#?(:clj ... :cljs ...)` reader conditionals; all shape/JSON
  logic above that is pure, portable data + functions.

  GLB binary-container framing (12-byte header + JSON chunk + BIN chunk) is
  delegated to `kotoba-lang/glb` (ADR-0048 §5, `com-junkawasaki/root`) —
  extracted because the same container format was also independently
  hand-rolled (read-only) in `kotoba-lang/vrm`'s `vrm.glb`. `u32->le-bytes`
  / `le-bytes->u32` / `string->byte-seq` / `pad-len` / the GLB magic+version+
  chunk-type constants below are re-exported from `glb` for call-site
  backward compatibility (nothing here reimplements them anymore); this
  namespace still owns everything glTF-JSON-specific (`build-gltf-json`,
  `->json`, `compute-bounds`, mesh vertex/index byte encoding)."
  (:require [glb]))

;; ---------------------------------------------------------------------------
;; Little-endian byte encoding — re-exported from `glb` (see ns docstring)
;; ---------------------------------------------------------------------------

(def u32->le-bytes glb/u32->le-bytes)
(def le-bytes->u32 glb/le-bytes->u32)

(defn f32->le-bytes
  "f32 -> 4 little-endian byte ints (0-255) of the IEEE-754 single-precision
  bit pattern, matching Rust's `f32::to_le_bytes`."
  [x]
  #?(:clj
     (u32->le-bytes (Float/floatToIntBits (float x)))
     :cljs
     (let [buf (js/ArrayBuffer. 4)
           view (js/DataView. buf)]
       (.setFloat32 view 0 x true)
       [(.getUint8 view 0) (.getUint8 view 1) (.getUint8 view 2) (.getUint8 view 3)])))

(def string->byte-seq glb/string->byte-seq)

(defn bytes-seq->platform
  "Convert a seq of byte ints (0-255) to the platform's native byte buffer:
  a `byte[]` on the JVM, a `js/Uint8Array` in ClojureScript."
  [s]
  #?(:clj (byte-array (map unchecked-byte s))
     :cljs (js/Uint8Array. (clj->js (vec s)))))

(def pad-len glb/pad-len)

;; ---------------------------------------------------------------------------
;; Minimal JSON serialization (only what the glTF JSON chunk needs)
;; ---------------------------------------------------------------------------

(defn json-escape [^String s]
  (-> s
      (clojure.string/replace "\\" "\\\\")
      (clojure.string/replace "\"" "\\\"")))

(defn ->json
  "Serialize a plain CLJC value (nil/bool/number/string/keyword/map/sequential)
  to a JSON string. Map keys may be keywords or strings."
  [v]
  (cond
    (nil? v) "null"
    (true? v) "true"
    (false? v) "false"
    (keyword? v) (str "\"" (json-escape (name v)) "\"")
    (string? v) (str "\"" (json-escape v) "\"")
    (map? v) (str "{"
                   (clojure.string/join ","
                     (map (fn [[k val]]
                            (str "\"" (json-escape (if (keyword? k) (name k) (str k))) "\""
                                 ":" (->json val)))
                          v))
                   "}")
    (number? v) (str v)
    (sequential? v) (str "[" (clojure.string/join "," (map ->json v)) "]")
    :else (throw (ex-info "gltf/->json: unsupported value" {:value v}))))

;; ---------------------------------------------------------------------------
;; Mesh bounds
;; ---------------------------------------------------------------------------

(defn compute-bounds
  "Compute [min max] position bounds (each a 3-vector) over the first
  `vertex-count` interleaved [pos3 norm3 uv2] vertices (stride 8 floats)."
  [vertices vertex-count]
  (loop [i 0
         mn [##Inf ##Inf ##Inf]
         mx [##-Inf ##-Inf ##-Inf]]
    (if (>= i vertex-count)
      [mn mx]
      (let [base (* i 8)
            p [(nth vertices base) (nth vertices (+ base 1)) (nth vertices (+ base 2))]]
        (recur (inc i) (mapv min mn p) (mapv max mx p))))))

;; ---------------------------------------------------------------------------
;; glTF JSON chunk
;; ---------------------------------------------------------------------------

(defn build-gltf-json
  "Build the glTF 2.0 JSON chunk (as a plain CLJC map) describing a single
  scene/node/mesh/material, matching the original Rust's `serde_json::json!`
  literal 1:1."
  [{:keys [color vertex-count index-count vertex-byte-len index-byte-len
           total-buffer-len min-pos max-pos]}]
  {:asset {:version "2.0" :generator "kami-scad"}
   :scene 0
   :scenes [{:nodes [0]}]
   :nodes [{:mesh 0}]
   :meshes [{:primitives [{:attributes {:POSITION 0 :NORMAL 1 :TEXCOORD_0 2}
                            :indices 3
                            :material 0}]}]
   :materials [{:pbrMetallicRoughness {:baseColorFactor color
                                        :metallicFactor 0.0
                                        :roughnessFactor 0.5}}]
   :accessors [{:bufferView 0
                :componentType 5126 ;; FLOAT
                :count vertex-count
                :type "VEC3"
                :byteOffset 0
                :min min-pos
                :max max-pos}
               {:bufferView 0
                :componentType 5126
                :count vertex-count
                :type "VEC3"
                :byteOffset 12} ;; after position (3 floats)
               {:bufferView 0
                :componentType 5126
                :count vertex-count
                :type "VEC2"
                :byteOffset 24} ;; after position + normal (6 floats)
               {:bufferView 1
                :componentType 5125 ;; UNSIGNED_INT
                :count index-count
                :type "SCALAR"}]
   :bufferViews [{:buffer 0
                   :byteOffset 0
                   :byteLength vertex-byte-len
                   :byteStride 32 ;; 8 floats × 4 bytes
                   :target 34962} ;; ARRAY_BUFFER
                  {:buffer 0
                   :byteOffset vertex-byte-len
                   :byteLength index-byte-len
                   :target 34963}] ;; ELEMENT_ARRAY_BUFFER
   :buffers [{:byteLength total-buffer-len}]})

;; ---------------------------------------------------------------------------
;; GLB (binary container) assembly — delegated to kotoba-lang/glb (ADR-0048 §5)
;; ---------------------------------------------------------------------------

(def glb-magic glb/glb-magic) ;; "glTF"
(def glb-version glb/glb-version)
(def json-chunk-type glb/chunk-type-json) ;; "JSON"
(def bin-chunk-type glb/chunk-type-bin)  ;; "BIN\0"

(defn export-glb-byte-seq
  "Pure, portable core: `mesh` (map with :vertices :indices :vertex-count
  :index-count) + `color` (RGBA 4-vector) -> vector of byte ints (0-255)
  forming a complete binary glTF 2.0 (.glb) file. No platform-specific types
  involved — safe to unit test directly on any platform.

  glTF-JSON building (`build-gltf-json`) and mesh vertex/index byte encoding
  stay here; the actual GLB container framing (header + chunk assembly +
  padding) is delegated to `glb/write-glb-raw` (`kotoba-lang/glb`, the
  canonical GLB codec, ADR-0048 §5) — the JSON bytes handed to it are already
  fully serialized (via this namespace's own `->json`), so `glb` does no
  re-encoding, just pure chunk framing."
  [{:keys [vertices indices vertex-count index-count] :as mesh} color]
  (let [vertex-bytes (vec (mapcat f32->le-bytes vertices))
        index-bytes (vec (mapcat u32->le-bytes indices))
        vertex-byte-len (count vertex-bytes)
        index-byte-len (count index-bytes)
        total-buffer-len (+ vertex-byte-len index-byte-len)
        [min-pos max-pos] (compute-bounds vertices vertex-count)
        json-map (build-gltf-json {:color color
                                    :vertex-count vertex-count
                                    :index-count index-count
                                    :vertex-byte-len vertex-byte-len
                                    :index-byte-len index-byte-len
                                    :total-buffer-len total-buffer-len
                                    :min-pos min-pos
                                    :max-pos max-pos})
        json-bytes (string->byte-seq (->json json-map))
        bin-bytes (vec (concat vertex-bytes index-bytes))]
    (glb/write-glb-raw json-bytes bin-bytes)))

(defn export-glb
  "Export `mesh` + RGBA `color` to a binary glTF 2.0 (.glb) buffer in the
  platform's native byte type (`byte[]` on the JVM, `js/Uint8Array` in cljs).
  Equivalent to the original Rust `export_glb`."
  [mesh color]
  (bytes-seq->platform (export-glb-byte-seq mesh color)))
