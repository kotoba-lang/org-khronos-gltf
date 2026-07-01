(ns gltf-test
  (:require [clojure.test :refer [deftest is testing]]
            [gltf]))
(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? gltf))))
