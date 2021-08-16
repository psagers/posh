(ns posh.lib.datom-matcher-test
  (:require [clojure.test :refer [is are deftest testing]]
            [posh.lib.datom-matcher :as dm]))

(deftest datom-match-pattern?-test
  (are [pattern datom]
    (is (true? (dm/datom-match-pattern? pattern datom)))

    [] [1 :some/kikka "kukka"]
    ['_] [1 :some/kikka "kukka"]
    [1] [1 :some/kikka "kukka"]
    [#{1}] [1 :some/kikka "kukka"]
    ['_ :some/kikka #{"kukka"}] [1 :some/kikka "kukka"]))
