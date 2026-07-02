(ns gltf-test
  (:require [clojure.test :refer [deftest is testing]]
            [gltf]))

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
