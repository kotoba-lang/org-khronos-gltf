(ns gltf-test
  (:require [clojure.test :refer [deftest is testing]]
            [gltf]
            [glb]
            [mesher]))

(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? (the-ns 'gltf)))))

;; Ported 1:1 from kami-gltf/src/lib.rs `#[test] fn glb_header_valid`.
(deftest glb-header-valid
  (let [mesh {:vertices (vec (repeat 24 0.0)) ;; 3 vertices × 8 floats
              :indices [0 1 2]
              :vertex-count 3
              :index-count 3}
        glb (gltf/export-glb-byte-seq mesh [1.0 0.0 0.0 1.0])]
    ;; magic "glTF"
    (is (= (vec (take 4 glb)) (gltf/u32->le-bytes gltf/glb-magic)))
    ;; version 2
    (is (= (subvec (vec glb) 4 8) (gltf/u32->le-bytes 2)))
    ;; total length header field matches actual byte count
    (let [total-len (gltf/le-bytes->u32 (subvec (vec glb) 8 12))]
      (is (= total-len (count glb))))))

;; Additional coverage beyond the original single Rust test, exercising the
;; ported logic that the Rust file only implicitly relied on.

(deftest json-chunk-type-tag-correct
  (let [mesh {:vertices (vec (repeat 16 0.0)) ;; 2 vertices × 8 floats
              :indices [0 1]
              :vertex-count 2
              :index-count 2}
        glb (vec (gltf/export-glb-byte-seq mesh [0.0 1.0 0.0 1.0]))]
    ;; bytes 12..16 = JSON chunk byte length (u32 le); 16..20 = "JSON" tag ascii
    (is (= (subvec glb 16 20) [0x4A 0x53 0x4F 0x4E]))))

(deftest compute-bounds-basic
  (let [vertices [1.0 2.0 3.0 0.0 0.0 0.0 0.0 0.0
                  -1.0 5.0 0.0 0.0 0.0 0.0 0.0 0.0]
        [mn mx] (gltf/compute-bounds vertices 2)]
    (is (= mn [-1.0 2.0 0.0]))
    (is (= mx [1.0 5.0 3.0]))))

(deftest le-byte-roundtrip
  (is (= 42 (gltf/le-bytes->u32 (gltf/u32->le-bytes 42))))
  (is (= 0x46546C67 (gltf/le-bytes->u32 (gltf/u32->le-bytes 0x46546C67)))))

(deftest json-serializes-known-shapes
  (is (= "null" (gltf/->json nil)))
  (is (= "true" (gltf/->json true)))
  (is (= "\"VEC3\"" (gltf/->json "VEC3")))
  (is (= "[1,2,3]" (gltf/->json [1 2 3])))
  (is (= "{\"a\":1}" (gltf/->json {:a 1}))))

;; ---------------------------------------------------------------------------
;; ADR-0048 §5 migration regression — export-glb-byte-seq now delegates its
;; GLB container framing to kotoba-lang/glb (glb/write-glb-raw). This proves
;; the migration didn't silently change output: a real, non-trivial mesh's
;; export is byte-count-exact and, critically, is itself a *valid* GLB per
;; the independent `glb` codec's own parser (glb/parse-glb round-trips the
;; JSON chunk this namespace built and hands back the exact bin bytes).
;; ---------------------------------------------------------------------------

(defn- real-mesh []
  ;; 3 vertices, interleaved [pos3 norm3 uv2] (stride 8 floats), + 1 triangle.
  (let [n 3
        vertex (fn [i] (concat [(double i) (double (* i 2)) (double (- i))] ;; pos3
                                [0.0 1.0 0.0]                                ;; norm3
                                [(double i) 0.0]))                           ;; uv2
        vertices (vec (mapcat vertex (range n)))]
    {:vertices vertices
     :indices [0 1 2]
     :vertex-count n
     :index-count 3}))

(deftest export-glb-byte-seq-migration-regression
  (let [mesh (real-mesh)
        color [0.25 0.5 0.75 1.0]
        out (gltf/export-glb-byte-seq mesh color)]
    (testing "exact byte count for this fixture (pinned — catches accidental framing drift)"
      (is (= 1008 (count out))))
    (testing "output is a byte-identical, independently-parseable GLB via kotoba-lang/glb"
      (let [{:keys [json bin]} (glb/parse-glb out)
            vertex-bytes (vec (mapcat gltf/f32->le-bytes (:vertices mesh)))
            index-bytes (vec (mapcat gltf/u32->le-bytes (:indices mesh)))
            expected-bin (vec (concat vertex-bytes index-bytes))]
        (is (= (gltf/build-gltf-json {:color color
                                       :vertex-count (:vertex-count mesh)
                                       :index-count (:index-count mesh)
                                       :vertex-byte-len (count vertex-bytes)
                                       :index-byte-len (count index-bytes)
                                       :total-buffer-len (count expected-bin)
                                       :min-pos (first (gltf/compute-bounds (:vertices mesh) (:vertex-count mesh)))
                                       :max-pos (second (gltf/compute-bounds (:vertices mesh) (:vertex-count mesh)))})
               json))
        (is (= expected-bin (subvec (vec bin) 0 (count expected-bin))))))))

;; ---------------------------------------------------------------------------
;; Parser (ADR-0048 §3) — closes the "glTF loader: export-only stub" gap.
;; ---------------------------------------------------------------------------

(deftest decode-accessor-basic-vec3-and-scalar
  (testing "a hand-built minimal glTF JSON + one buffer decodes exactly"
    (let [;; 2 vertices' worth of interleaved [pos3 norm3 uv2] (stride 8 floats)
          floats [1.0 2.0 3.0  0.0 1.0 0.0  0.0 0.0
                  4.0 5.0 6.0  0.0 1.0 0.0  1.0 1.0]
          vertex-bytes (vec (mapcat gltf/f32->le-bytes floats))
          indices [0 1]
          index-bytes (vec (mapcat gltf/u32->le-bytes indices))
          buf (vec (concat vertex-bytes index-bytes))
          json {:accessors [{:bufferView 0 :componentType 5126 :count 2 :type "VEC3" :byteOffset 0}
                             {:bufferView 0 :componentType 5126 :count 2 :type "VEC3" :byteOffset 12}
                             {:bufferView 0 :componentType 5126 :count 2 :type "VEC2" :byteOffset 24}
                             {:bufferView 1 :componentType 5125 :count 2 :type "SCALAR"}]
                :bufferViews [{:buffer 0 :byteOffset 0 :byteLength (count vertex-bytes) :byteStride 32}
                              {:buffer 0 :byteOffset (count vertex-bytes) :byteLength (count index-bytes)}]
                :buffers [{:byteLength (count buf)}]}
          buffers [buf]]
      (is (= [[1.0 2.0 3.0] [4.0 5.0 6.0]] (gltf/decode-accessor json buffers 0)))
      (is (= [[0.0 1.0 0.0] [0.0 1.0 0.0]] (gltf/decode-accessor json buffers 1)))
      (is (= [[0.0 0.0] [1.0 1.0]] (gltf/decode-accessor json buffers 2)))
      (is (= [0 1] (gltf/decode-accessor json buffers 3))))))

(deftest decode-accessor-normalized-unsigned-byte
  (testing "normalized UNSIGNED_BYTE divides by 255"
    (let [buf [255 128 0 0]
          json {:accessors [{:bufferView 0 :componentType 5121 :count 1 :type "VEC4" :normalized true}]
                :bufferViews [{:buffer 0 :byteOffset 0 :byteLength 4}]
                :buffers [{:byteLength 4}]}]
      (let [[[a b c d]] (gltf/decode-accessor json [buf] 0)]
        (is (= 1.0 a))
        (is (< (Math/abs (- b (/ 128.0 255.0))) 1e-9))
        (is (= 0.0 c))
        (is (= 0.0 d))))))

(deftest base64-round-trip
  (let [bytes [0 1 2 3 4 5 250 251 252 253 254 255]]
    (is (= bytes (gltf/base64->byte-seq
                  #?(:clj (.encodeToString (java.util.Base64/getEncoder) (byte-array (map unchecked-byte bytes)))
                     :cljs (js/btoa (apply str (map #(js/String.fromCharCode %) bytes)))))))))

(deftest parse-gltf-json-only-map-with-data-uri-buffer
  (testing "JSON-only .gltf (already-parsed map) with a data: URI buffer decodes"
    (let [floats [1.0 2.0 3.0]
          bytes (vec (mapcat gltf/f32->le-bytes floats))
          b64 #?(:clj (.encodeToString (java.util.Base64/getEncoder) (byte-array (map unchecked-byte bytes)))
                 :cljs (js/btoa (apply str (map #(js/String.fromCharCode %) bytes))))
          json {:meshes [{:primitives [{:attributes {:POSITION 0}}]}]
                :accessors [{:bufferView 0 :componentType 5126 :count 1 :type "VEC3"}]
                :bufferViews [{:buffer 0 :byteOffset 0 :byteLength (count bytes)}]
                :buffers [{:byteLength (count bytes)
                           :uri (str "data:application/octet-stream;base64," b64)}]}
          parsed (gltf/parse-gltf json)]
      (is (= [[1.0 2.0 3.0]] (-> parsed :meshes first :primitives first :positions))))))

;; Real round-trip: build a real mesh via kotoba-lang/mesher's `sdf-to-mesh`
;; (a genuinely working sibling library, ADR-0048 §3 task) — its output
;; shape (`{:vertex-count :index-count :vertices :indices}`, interleaved
;; [pos3 norm3 uv2] floats) already matches `gltf`'s own LoadedMesh contract
;; verbatim, so it plugs straight into `export-glb-byte-seq` with no
;; adapter. Export -> parse -> reassemble interleaved vertices -> compare
;; against the source mesh (float32 round-trip tolerance, since export
;; truncates f64 -> f32).
(defn- approx= [a b] (< (Math/abs (double (- a b))) 1e-4))

(deftest round-trip-mesh-from-mesher
  (let [sample (fn [x y z] [(- (Math/sqrt (+ (* x x) (* y y) (* z z))) 0.9) [1.0 0.0 0.0 1.0]])
        mesh (mesher/sdf-to-mesh sample 10 1.4)
        color [0.25 0.6 0.15 1.0]
        glb-bytes (gltf/export-glb-byte-seq mesh color)
        parsed (gltf/parse-gltf glb-bytes)
        prim (-> parsed :meshes first :primitives first)
        {:keys [positions normals uvs indices]} prim
        src-vertices (:vertices mesh)]
    (testing "sanity: mesher actually produced a non-trivial mesh"
      (is (> (:vertex-count mesh) 20))
      (is (> (:index-count mesh) 20)))
    (testing "counts match"
      (is (= (:vertex-count mesh) (count positions) (count normals) (count uvs)))
      (is (= (:index-count mesh) (count indices))))
    (testing "indices recovered exactly (integers, no precision loss)"
      (is (= (:indices mesh) indices)))
    (testing "positions/normals/uvs recovered within f32 round-trip tolerance"
      (dotimes [i (:vertex-count mesh)]
        (let [base (* i 8)
              src-pos [(nth src-vertices base) (nth src-vertices (+ base 1)) (nth src-vertices (+ base 2))]
              src-norm [(nth src-vertices (+ base 3)) (nth src-vertices (+ base 4)) (nth src-vertices (+ base 5))]
              src-uv [(nth src-vertices (+ base 6)) (nth src-vertices (+ base 7))]]
          (is (every? true? (map approx= src-pos (nth positions i)))
              (str "vertex " i " position mismatch: " src-pos " vs " (nth positions i)))
          (is (every? true? (map approx= src-norm (nth normals i)))
              (str "vertex " i " normal mismatch: " src-norm " vs " (nth normals i)))
          (is (every? true? (map approx= src-uv (nth uvs i)))
              (str "vertex " i " uv mismatch: " src-uv " vs " (nth uvs i))))))))
