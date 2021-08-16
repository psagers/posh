(ns posh.lib.datascript-test
  (:require [clojure.test :refer [is are deftest testing]]
            [datascript.core :as dt]
            [posh.clj.datascript :as d]))

(deftest test-simple-query
  (let [conn (dt/create-conn {:a {:db/unique :db.unique/identity}})
        _ (d/posh! conn)
        _ (d/transact! conn [{:a "foo"}])
        eid (->> (d/q '[:find ?e
                        :where [?e :a "foo"]] conn)
                 deref
                 ffirst)]
    (is (some? eid) "Entity should be returned from basic matching query")))

;; NOTE: Hardcoding in a lookup-ref in :where isn't supposed to work -- so only testing :in here
;;       https://docs.datomic.com/on-prem/identity.html#lookup-refs
(deftest test-lookup-ref-in-eid
  (testing "Lookups refs work within query :in as entity-id"
    (let [conn (dt/create-conn {:a {:db/unique :db.unique/identity}})
          _ (d/posh! conn)
          _ (d/transact! conn [{:a "foo"
                                :b "bar"}])
          b (->> (d/q '[:find ?b
                        :in $ ?lookup
                        :where
                        [?lookup :b ?b]]
                      conn [:a "foo"])
                 deref
                 first)]
      (is (= b ["bar"])))))

(deftest test-lookup-ref-in-value
  (testing "Lookups refs work within query value as reference value"
    (let [conn (dt/create-conn {:a {:db/unique :db.unique/identity}
                                :b {:db/valueType :db.type/ref}})
          _ (d/posh! conn)
          _ (d/transact! conn [{:a "foo"
                                :b {:a "foo2"}}])
          b (->> (d/q '[:find ?aval
                        :in $ ?lookup
                        :where
                        [?e :b ?lookup]
                        [?e :a ?aval]]
                      conn [:a "foo2"])
                 deref
                 first)]
      (is (= b ["foo"])))))

(deftest test-lookup-ref-transact
  (testing "Lookups refs work via db/transact"
    (let [conn (dt/create-conn {:a {:db/unique :db.unique/identity}})
          _ (d/posh! conn)
          _ (d/transact! conn [{:a "foo"
                                :b "bar"}])
          ent-a (->> (d/q '[:find ?e
                            :where [?e :a "foo"]] conn)
                     deref
                     ffirst
                     (dt/entity (dt/db conn))
                     dt/touch)
          _ (d/transact! conn [[:db/add [:a "foo"] :b "zim"]])
          ent-b (->> (d/q '[:find ?e
                            :where [?e :a "foo"]] conn)
                     deref
                     ffirst
                     (dt/entity (dt/db conn))
                     dt/touch)]
      (is (= (:b ent-a) "bar"))
      (is (= (:b ent-b) "zim") "lookup ref overwrote previous value")
      (is (= (:db/id ent-a) (:db/id ent-b)) "lookup ref modified same entity as original tx"))))

(deftest test-tuple-value-in
  (testing "Query for tuple values works via :in"
    (let [conn (dt/create-conn)
          _ (d/posh! conn)
          _ (d/transact! conn [{:a ["foo" "bar"]
                                :b 42}])
          ent (->> (d/q '[:find ?e
                          :in $ ?v
                          :where [?e :a ?v]] conn ["foo" "bar"])
                   deref
                   ffirst
                   (dt/entity (dt/db conn))
                   dt/touch)]
      (is (= ["foo" "bar"] (:a ent)))
      (is (= 42 (:b ent))))))

(deftest test-tuple-value-where
  (testing "Query for tuple values works in :where clause"
    (let [conn (dt/create-conn)
          _ (d/posh! conn)
          _ (d/transact! conn [{:a ["foo" "bar"]
                                :b 42}])
          ent (->> (d/q '[:find ?e
                          :where [?e :a ["foo" "bar"]]] conn)
                   deref
                   ffirst
                   (dt/entity (dt/db conn))
                   dt/touch)]
      (is (= ["foo" "bar"] (:a ent)))
      (is (= 42 (:b ent))))))

(deftest test-basic-pull-reaction
  (testing "Basic pull returns entity reaction which updates on entity's transact"
    (let [conn (dt/create-conn)
          _ (d/posh! conn)
          _ (d/transact! conn [{:a "foo" :b 42}])
          eid (->> (d/q '[:find ?e
                          :where [?e :a "foo"]] conn)
                   deref
                   ffirst)
          entity-reaction (d/pull conn '[*] eid)]
      (is (= (select-keys @entity-reaction [:a :b])
             {:a "foo" :b 42}) "entity-reaction derefs to first transacted value")
      (d/transact! conn [{:a "baz" :b 42}])
      (is (= (select-keys @entity-reaction [:a :b])
             {:a "foo" :b 42}) "entity-reaction contains correct value after unrelated tx")
      (d/transact! conn [(assoc @entity-reaction :a "bar" :b 43)])
      (is (= (select-keys @entity-reaction [:a :b])
             {:a "bar" :b 43}) "entity-reaction derefs to later transacted value"))))

(deftest test-pull-many
  (testing "pull-many returns entity reaction which updates on any entity's transact"
    (let [conn (dt/create-conn)
          _ (d/posh! conn)
          ents [{:a "foo" :b 42}
                {:a "bar" :b 52}
                {:a "baz" :b 62}]
          _ (d/transact! conn ents)
          eids (->> (d/q '[:find ?e
                           :where [?e :a _]] conn)
                    deref
                    (reduce into [])
                    reverse)
          ;;dtlg-raw (dt/pull-many (dt/db conn) '[*] eids)
          entity-reaction (d/pull-many conn '[*] eids)]
      (is (= ents (map #(select-keys % [:a :b]) @entity-reaction))
          "Entities in reaction should match input entities against input sequence of eids")
      (let [updated-ents (vec (map #(update % :b inc) @entity-reaction))]
        (d/transact! conn updated-ents)
        (is (= updated-ents @entity-reaction)
            "Entities in reaction should updated after transact")))))

(defn analyze [schema data pattern id]
  (let [conn (dt/create-conn schema)]
    (d/posh! conn)
    (d/transact! conn data)
    (posh.lib.pull-analyze/pull-analyze
     d/dcfg
     [:datoms :patterns]
     {:db @conn :schema schema :db-id :posh}
     pattern id)))

(deftest pull-reverse-one-component-test
  (let [->schema (fn [isComponent]
                   {:parent/id {:db/unique :db.unique/identity}
                    :parent/child {:db/valueType :db.type/ref
                                   :db/isComponent isComponent
                                   :db/cardinality :db.cardinality/one}
                    :child/id {:db/unique :db.unique/identity}})
        data [{:parent/id "grandma"
               :parent/score 16
               :name "margit"
               :parent/child {:parent/id "dad"
                              :name "make"
                              :parent/child {:child/id "kid"
                                             :name "eeva"}}}]]

    (testing "pulling reverse relationships"
      (let [conn (dt/create-conn (->schema true))
            _ (d/posh! conn)
            _ (d/transact! conn data)
            grandma-reaction (d/pull conn [:parent/score] [:parent/id "grandma"])
            dad-reaction (d/pull conn [{:parent/_child [:parent/score]}] [:parent/id "dad"])
            child-reaction (d/pull conn [{:parent/_child [{:parent/_child [:parent/score]}]}] [:child/id "kid"])]

        (testing "all see same score"
          (is (= (-> @grandma-reaction :parent/score)
                 (-> @dad-reaction :parent/_child :parent/score)
                 (-> @child-reaction :parent/_child :parent/_child :parent/score))))

        (testing "all get updated on score change"
          (d/transact! conn [[:db/add [:parent/id "grandma"] :parent/score 42]])
          (is (= (-> @grandma-reaction :parent/score)
                 (-> @dad-reaction :parent/_child :parent/score)
                 (-> @child-reaction :parent/_child :parent/_child :parent/score))))))

    (testing "analysis"
      (are [pattern id expected]
        (let [with-isComponent (analyze (->schema true) data pattern id)
              without-isComponent (analyze (->schema false) data pattern id)]

          (testing "both ways give same resuls"
            (is (= with-isComponent without-isComponent)))

          (testing "results are correct"
            (is (= expected with-isComponent))))

        ;; direct
        [:parent/score :name]
        [:parent/id "grandma"]
        '{:datoms {:posh [[1 :parent/score 16]
                          [1 :name "margit"]]},
          :patterns {:posh [[#{1} #{:name :parent/score} _]
                            [_ :parent/id "grandma"]]}}

        ;; child -> child
        [:name :parent/score {:parent/child [:name {:parent/child [:name]}]}]
        [:parent/id "grandma"]
        '{:datoms {:posh ([1 :name "margit"]
                          [1 :parent/score 16]
                          [1 :parent/child 2]
                          [2 :name "make"]
                          [2 :parent/child 3]
                          [3 :name "eeva"])},
          :patterns {:posh ([#{1 2} :parent/child _]
                            [#{3 2} #{:name} _]
                            [#{1} #{:name :parent/score} _]
                            [_ :parent/id "grandma"])}}

        ;; _child -> _child
        [:name {:parent/_child [:name {:parent/_child [:name :parent/score]}]}]
        [:child/id "kid"]
        '{:datoms {:posh ([3 :name "eeva"]
                          [2 :parent/child 3]
                          [2 :name "make"]
                          [1 :parent/child 2]
                          [1 :name "margit"]
                          [1 :parent/score 16])},
          :patterns {:posh ([_ :parent/child 2]
                            [_ :parent/child 3]
                            [#{1} #{:name :parent/score} _]
                            [#{3 2} #{:name} _]
                            [_ :child/id "kid"])}}))))

(deftest pull-reverse-many-test
  (let [schema {:parent/id {:db/unique :db.unique/identity}
                :parent/child {:db/valueType :db.type/ref
                               :db/isComponent false
                               :db/cardinality :db.cardinality/many}
                :child/id {:db/unique :db.unique/identity}}
        data [{:parent/id "grandma"
               :parent/score 16
               :name "margit"
               :parent/child [{:parent/id "mom"
                               :name "irmeli"
                               :parent/child [{:child/id "kid"
                                               :name "eeva"}]}
                              {:parent/id "dad"
                               :name "make"
                               :parent/child [{:child/id "kid"
                                               :name "eeva"}]}]}]]

    (testing "analysis"
      (are [pattern id expected]
        (let [ana (analyze schema data pattern id)]
          (is (= expected ana)))

        ;; child -> child -> _child -> _child
        [:name {:parent/child [:name {:parent/child [:name {:parent/_child [{:parent/_child [:parent/score]}]}]}]}]
        [:parent/id "grandma"]
        '{:datoms {:posh ([1 :name "margit"]
                          [1 :parent/child 2]
                          [1 :parent/child 4]
                          [2 :name "irmeli"]
                          [2 :parent/child 3]
                          [3 :name "eeva"]
                          [1 :parent/score 16]
                          [4 :parent/child 3]
                          [4 :name "make"])},
          :patterns {:posh ([_ :parent/child 3]
                            [_ :parent/child 2]
                            [_ :parent/child 4]
                            [#{1 4 2} :parent/child _]
                            [#{1} #{:parent/score} _]
                            [#{1 4 3 2} #{:name} _]
                            [_ :parent/id "grandma"])}}))))
