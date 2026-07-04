(ns gltf-test
  (:require [clojure.test :refer [deftest is testing]]
            [gltf]
            [glb]))

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
