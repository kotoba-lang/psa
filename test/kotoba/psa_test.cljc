(ns kotoba.psa-test
  (:require [clojure.test :refer [deftest is testing]]
            [kotoba.psa :as psa]))

(def ^:private day 86400000)
(def ^:private t0 1767225600000) ;; 2026-01-01T00:00:00Z

(defn- d [n] (+ t0 (* n day)))

(defn- ts [worker date project role hours]
  {:ts/worker worker :ts/date date :ts/project project :ts/role role :ts/hours hours})

(def ^:private cards
  [(psa/rate-card "alpha" :engineer 15000 :cost 9000)
   (psa/rate-card "alpha" :designer 12000 :cost 8000)
   (psa/rate-card "beta" nil 10000)])          ;; project-wide, no cost declared

;; ---------------------------------------------------------------------------
;; Rate lookup
;; ---------------------------------------------------------------------------

(deftest exact-role-beats-the-project-wide-fallback
  (is (= 15000 (:rate/billable (psa/rate-for cards "alpha" :engineer))))
  (is (= 10000 (:rate/billable (psa/rate-for cards "beta" :engineer))))
  (testing "a project-wide card covers a role it never named"
    (is (nil? (:rate/role (psa/rate-for cards "beta" :designer))))))

(deftest an-unknown-project-has-no-rate-not-a-zero-one
  (is (nil? (psa/rate-for cards "gamma" :engineer))))

;; ---------------------------------------------------------------------------
;; Invariant 1 — no rate, no line
;; ---------------------------------------------------------------------------

(deftest unpriced-time-is-reported-not-billed-at-zero
  (let [p (psa/price-entry cards (ts "w-1" "2026-01-01" "gamma" :engineer 8))]
    (is (:priced/unpriced? p))
    (testing "nil is the absence of a price; zero would be a price"
      (is (nil? (:priced/billable p)))
      (is (not= 0 (:priced/billable p))))))

(deftest an-invoice-excludes-unpriced-time-and-says-so
  (let [entries [(ts "w-1" "2026-01-01" "alpha" :engineer 8)
                 (ts "w-1" "2026-01-02" "alpha" :architect 8)] ;; no such card... falls to none
        inv (psa/invoice "inv-1" "alpha" entries cards)]
    (testing "alpha has no project-wide fallback, so :architect is unpriced"
      (is (= 1 (count (:invoice/unpriced inv))))
      (is (= 120000 (:invoice/total inv))))
    (is (= "1 entries had no rate card and were not billed" (psa/describe-gap inv)))))

(deftest billable?-lets-a-caller-check-before-drafting
  (is (psa/billable? cards (ts "w-1" "2026-01-01" "alpha" :engineer 8)))
  (is (not (psa/billable? cards (ts "w-1" "2026-01-01" "alpha" :architect 8)))))

;; ---------------------------------------------------------------------------
;; Invariant 2 — margin needs both sides
;; ---------------------------------------------------------------------------

(deftest margin-is-computed-when-every-entry-is-costed
  (let [priced (map #(psa/price-entry cards %)
                    [(ts "w-1" "2026-01-01" "alpha" :engineer 10)
                     (ts "w-2" "2026-01-01" "alpha" :designer 10)])
        m (psa/margin priced)]
    (is (= 270000 (:margin/revenue m)))        ;; 150000 + 120000
    (is (= 170000 (:margin/cost m)))           ;;  90000 +  80000
    (is (= 100000 (:margin/amount m)))
    (is (< 0.37 (:margin/ratio m) 0.371))))

(deftest margin-is-unknown-when-any-cost-rate-is-missing
  (testing "a bill rate with no cost rate would read as pure profit"
    (let [priced (map #(psa/price-entry cards %)
                      [(ts "w-1" "2026-01-01" "beta" :engineer 10)])
          m (psa/margin priced)]
      (is (= 100000 (:margin/revenue m)))
      (is (= :unknown (:margin/cost m)))
      (is (= :unknown (:margin/amount m)))
      (is (= :unknown (:margin/ratio m)))
      (is (= 1 (:margin/uncosted-count m))))))

(deftest one-uncosted-entry-poisons-the-whole-margin
  (testing "a partially costed set produces a number that is really missing data"
    (let [priced (map #(psa/price-entry cards %)
                      [(ts "w-1" "2026-01-01" "alpha" :engineer 10)
                       (ts "w-1" "2026-01-02" "beta" :engineer 10)])
          m (psa/margin priced)]
      (is (= 250000 (:margin/revenue m)))
      (is (= :unknown (:margin/amount m)))
      (is (= 1 (:margin/uncosted-count m))))))

(deftest margin-of-nothing-is-unknown-not-zero
  (is (= :unknown (:margin/amount (psa/margin [])))))

;; ---------------------------------------------------------------------------
;; Invariant 3 — utilization needs a denominator
;; ---------------------------------------------------------------------------

(def ^:private date->ms {"2026-01-01" (d 0) "2026-01-02" (d 1) "2026-01-05" (d 4)})

(deftest utilization-against-declared-capacity
  (let [entries [(ts "w-1" "2026-01-01" "alpha" :engineer 8)
                 (ts "w-1" "2026-01-02" "alpha" :engineer 8)]
        caps [(psa/capacity "w-1" (d 0) (d 5) 40)]
        u (psa/utilization entries caps "w-1" [(d 0) (d 5)] date->ms)]
    (is (= 16.0 (:utilization/billable-hours u)))
    (is (= 0.4 (:utilization/ratio u)))))

(deftest utilization-is-unknown-without-capacity
  (let [entries [(ts "w-1" "2026-01-01" "alpha" :engineer 8)]
        u (psa/utilization entries [] "w-1" [(d 0) (d 5)] date->ms)]
    (is (= 8.0 (:utilization/billable-hours u)))
    (testing "no denominator, so no ratio — not 1.0"
      (is (= :unknown (:utilization/ratio u))))))

(deftest utilization-ignores-entries-outside-the-window
  (let [entries [(ts "w-1" "2026-01-01" "alpha" :engineer 8)
                 (ts "w-1" "2026-01-05" "alpha" :engineer 8)]
        caps [(psa/capacity "w-1" (d 0) (d 2) 16)]
        u (psa/utilization entries caps "w-1" [(d 0) (d 2)] date->ms)]
    (is (= 8.0 (:utilization/billable-hours u)))))

;; ---------------------------------------------------------------------------
;; Staffing
;; ---------------------------------------------------------------------------

(deftest an-assignment-inside-the-window-counts-in-full
  (let [a (psa/assignment "a-1" "w-1" "alpha" :engineer (d 0) (d 5) 40)]
    (is (= 40.0 (psa/hours-in-window a [(d 0) (d 5)])))))

(deftest a-partial-overlap-is-prorated
  (let [a (psa/assignment "a-1" "w-1" "alpha" :engineer (d 0) (d 10) 80)]
    (is (= 40.0 (psa/hours-in-window a [(d 0) (d 5)])))
    (is (= 16.0 (psa/hours-in-window a [(d 8) (d 10)])))))

(deftest a-disjoint-assignment-contributes-nothing
  (let [a (psa/assignment "a-1" "w-1" "alpha" :engineer (d 0) (d 5) 40)]
    (is (zero? (psa/hours-in-window a [(d 6) (d 10)])))))

(deftest over-allocation-is-detected-against-declared-capacity
  (let [as [(psa/assignment "a-1" "w-1" "alpha" :engineer (d 0) (d 5) 30)
            (psa/assignment "a-2" "w-1" "beta" :engineer (d 0) (d 5) 20)]
        caps [(psa/capacity "w-1" (d 0) (d 5) 40)]
        al (psa/allocation as caps "w-1" [(d 0) (d 5)])]
    (is (= 50.0 (:allocation/committed-hours al)))
    (is (:allocation/over? al))
    (is (= ["a-1" "a-2"] (:allocation/assignments al)))))

(deftest an-undeclared-capacity-is-neither-infinite-nor-a-violation
  (let [as [(psa/assignment "a-1" "w-1" "alpha" :engineer (d 0) (d 5) 200)]
        al (psa/allocation as [] "w-1" [(d 0) (d 5)])]
    (is (= 200.0 (:allocation/committed-hours al)))
    (is (not (:allocation/capacity-known? al)))
    (testing "not reported as over-allocated, because nothing said what over means"
      (is (not (:allocation/over? al))))))

(deftest allocation-only-counts-the-named-person
  (let [as [(psa/assignment "a-1" "w-1" "alpha" :engineer (d 0) (d 5) 40)
            (psa/assignment "a-2" "w-2" "alpha" :engineer (d 0) (d 5) 40)]
        al (psa/allocation as [] "w-1" [(d 0) (d 5)])]
    (is (= 40.0 (:allocation/committed-hours al)))))

;; ---------------------------------------------------------------------------
;; Invoicing
;; ---------------------------------------------------------------------------

(deftest an-invoice-groups-lines-by-role
  (let [entries [(ts "w-1" "2026-01-01" "alpha" :engineer 8)
                 (ts "w-2" "2026-01-01" "alpha" :engineer 8)
                 (ts "w-3" "2026-01-01" "alpha" :designer 4)]
        inv (psa/invoice "inv-1" "alpha" entries cards)]
    (is (= 2 (count (:invoice/lines inv))))
    (testing "two rates make two checkable lines, not one blended figure"
      (is (= [:designer :engineer] (mapv :line/role (:invoice/lines inv))))
      (is (= [48000 240000] (mapv :line/amount (:invoice/lines inv)))))
    (is (= 288000 (:invoice/total inv)))))

(deftest an-invoice-only-covers-its-own-project
  (let [entries [(ts "w-1" "2026-01-01" "alpha" :engineer 8)
                 (ts "w-1" "2026-01-02" "beta" :engineer 8)]
        inv (psa/invoice "inv-1" "alpha" entries cards)]
    (is (= 120000 (:invoice/total inv)))
    (is (= 1 (count (:invoice/entries inv))))))

(deftest already-billed-time-is-excluded-and-reported
  (let [entries [(ts "w-1" "2026-01-01" "alpha" :engineer 8)
                 (ts "w-1" "2026-01-02" "alpha" :engineer 8)]
        first-inv (psa/invoice "inv-1" "alpha" entries cards)
        billed (set (:invoice/entries first-inv))
        second-inv (psa/invoice "inv-2" "alpha" entries cards billed)]
    (is (= 240000 (:invoice/total first-inv)))
    (testing "re-running the same timesheet bills nothing a second time"
      (is (zero? (:invoice/total second-inv)))
      (is (= 2 (count (:invoice/excluded-already-billed second-inv))))
      (is (= "2 entries were already billed and were excluded"
             (psa/describe-gap second-inv))))))

(deftest entry-key-is-content-derived-so-a-reimport-cannot-double-bill
  (testing "the same day's work read from two systems collides on purpose"
    (is (= (psa/entry-key (ts "w-1" "2026-01-01" "alpha" :engineer 8))
           (psa/entry-key (assoc (ts "w-1" "2026-01-01" "alpha" :engineer 8)
                                 :ts/observed-ms 28800000))))))

(deftest describe-gap-is-empty-when-nothing-was-left-out
  (let [inv (psa/invoice "inv-1" "alpha" [(ts "w-1" "2026-01-01" "alpha" :engineer 8)] cards)]
    (testing "no reassuring 'no problems' line — absence of text is the signal"
      (is (= "" (psa/describe-gap inv))))))

;; ---------------------------------------------------------------------------
;; Composition with the sibling libraries' shape
;; ---------------------------------------------------------------------------

(deftest entries-emitted-by-kotoba-activity-price-unchanged
  (testing "the :ts/* shape kotoba.activity emits, with a role attached"
    (let [from-activity {:ts/worker "w-1" :ts/date "2026-01-01"
                         :ts/project "alpha" :ts/hours 1.5 :ts/observed-ms 6000000}
          p (psa/price-entry cards (assoc from-activity :ts/role :engineer))]
      (is (not (:priced/unpriced? p)))
      (is (= 22500.0 (:priced/billable p))))))

(deftest hours-and-ms-round-trip
  (is (= 1.5 (psa/ms->hours (psa/hours->ms 1.5)))))
