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

;; ---------------------------------------------------------------------------
;; Invariant 4 — no rate, no conversion
;; ---------------------------------------------------------------------------

(def ^:private rates
  [(psa/fx-rate "EUR" "USD" 1.08 "2026-01-01")
   (psa/fx-rate "JPY" "USD" 0.0064 "2026-01-01")])

(deftest same-currency-converts-at-one-without-a-rate-lookup
  (let [m (psa/convert [] 1000 "USD" "USD")]
    (is (= 1000 (:money/amount m)))
    (is (= 1 (:money/rate m)))))

(deftest a-conversion-carries-the-rate-and-the-date-it-came-from
  (let [m (psa/convert rates 1000 "EUR" "USD")]
    (is (= 1080.0 (:money/amount m)))
    (is (= 1.08 (:money/rate m)))
    (testing "an invoice total that cannot say which rate it used is uncheckable"
      (is (= "2026-01-01" (:money/as-of m))))))

(deftest rates-are-directional-and-not-inverted-automatically
  (testing "a buy rate is not a sell rate; inferring one invents a spread"
    (is (:money/unconvertible? (psa/convert rates 1000 "USD" "EUR")))))

(deftest an-unpriced-currency-is-neither-converted-at-one-nor-dropped
  (let [t (psa/total-in rates [{:amount 1000 :currency "USD"}
                               {:amount 1000 :currency "EUR"}
                               {:amount 1000 :currency "GBP"}]
                        "USD")]
    (is (= 2080.0 (:total/amount t)))
    (testing "the GBP amount is absent from the total AND named"
      (is (= 1 (count (:total/unconvertible t))))
      (is (= "GBP" (:currency (first (:total/unconvertible t))))))
    (testing "and the total says it is incomplete rather than looking finished"
      (is (not (:total/complete? t))))))

(deftest a-fully-convertible-total-says-so-and-lists-its-rates
  (let [t (psa/total-in rates [{:amount 1000 :currency "USD"}
                               {:amount 1000 :currency "EUR"}]
                        "USD")]
    (is (:total/complete? t))
    (is (= 1 (count (:total/rates-used t))))))

;; ---------------------------------------------------------------------------
;; Expenses
;; ---------------------------------------------------------------------------

(deftest a-billable-expense-carries-its-markup
  (let [e (psa/expense "e-1" "alpha" 10000 "USD" :billable? true :markup 0.1)]
    (is (= 11000.0 (psa/expense-billable-amount e)))))

(deftest a-non-billable-expense-bills-nothing-but-still-costs
  (let [e (psa/expense "e-1" "alpha" 10000 "USD" :billable? false)
        m (psa/project-margin [] [e] [])]
    (is (zero? (psa/expense-billable-amount e)))
    (testing "absorbing a cost decides who pays, not whether to count it"
      (is (= 10000 (:margin/expense-cost m))))))

(deftest markup-applies-only-to-billable-expenses
  (let [e (psa/expense "e-1" "alpha" 10000 "USD" :billable? false :markup 0.5)]
    (is (zero? (psa/expense-billable-amount e)))))

;; ---------------------------------------------------------------------------
;; Subcontractors
;; ---------------------------------------------------------------------------

(deftest a-subcontractor-carries-both-rates-explicitly
  (let [s (psa/subcontract "s-1" "alpha" "vendor-a" :engineer 10 8000 14000 "USD")
        m (psa/subcontract-margin s)]
    (is (= 140000 (:sub/billable m)))
    (is (= 80000 (:sub/cost m)))
    (is (= 60000 (:sub/amount m)))))

(deftest project-margin-separates-where-the-money-went
  (let [priced (map #(psa/price-entry cards %)
                    [(ts "w-1" "2026-01-01" "alpha" :engineer 10)])
        expenses [(psa/expense "e-1" "alpha" 20000 "USD" :billable? true :markup 0.1)]
        subs [(psa/subcontract "s-1" "alpha" "vendor-a" :engineer 10 8000 14000 "USD")]
        m (psa/project-margin priced expenses subs)]
    (is (= 312000.0 (:margin/revenue m)))     ;; 150000 labour + 22000 expense + 140000 sub
    (is (= 190000 (:margin/cost m)))          ;;  90000 labour + 20000 expense +  80000 sub
    (is (= 122000.0 (:margin/amount m)))
    (testing "each source is visible, not just the net"
      (is (= 22000.0 (:margin/expense-revenue m)))
      (is (= 140000 (:margin/subcontract-revenue m))))))

(deftest invariant-2-survives-the-extra-cost-sources
  (testing "one uncosted labour entry still poisons the whole project margin"
    (let [priced (map #(psa/price-entry cards %)
                      [(ts "w-1" "2026-01-01" "beta" :engineer 10)])   ;; no cost rate
          m (psa/project-margin priced
                                [(psa/expense "e-1" "beta" 100 "USD" :billable? true)]
                                [(psa/subcontract "s-1" "beta" "v" :engineer 1 1 2 "USD")])]
      (is (= :unknown (:margin/amount m)))
      (is (= :unknown (:margin/ratio m)))
      (testing "and the expense and subcontract figures are still reported"
        (is (= 100 (:margin/expense-revenue m)))))))

;; ---------------------------------------------------------------------------
;; Revenue recognition
;; ---------------------------------------------------------------------------

(deftest as-delivered-recognises-what-was-billed
  (let [c (psa/contract "alpha" :as-delivered)
        priced (map #(psa/price-entry cards %)
                    [(ts "w-1" "2026-01-01" "alpha" :engineer 10)])]
    (is (= 150000 (:revenue/amount (psa/recognize c priced false))))))

(deftest percent-complete-needs-a-budget-and-a-fee
  (let [priced (map #(psa/price-entry cards %)
                    [(ts "w-1" "2026-01-01" "alpha" :engineer 40)])]
    (testing "with both, progress is measured"
      (let [c (psa/contract "alpha" :percent-complete :fee 1000000 :budget-hours 100)
            r (psa/recognize c priced false)]
        (is (= 0.4 (:revenue/progress r)))
        (is (= 400000.0 (:revenue/amount r)))))
    (testing "without a budget, 'about 80% done' is an estimate this refuses to supply"
      (let [c (psa/contract "alpha" :percent-complete :fee 1000000)
            r (psa/recognize c priced false)]
        (is (= :unknown (:revenue/amount r)))
        (is (= :no-budget-hours (:revenue/reason r)))))
    (testing "without a fee either"
      (let [c (psa/contract "alpha" :percent-complete :budget-hours 100)
            r (psa/recognize c priced false)]
        (is (= :unknown (:revenue/amount r)))
        (is (= :no-fee (:revenue/reason r)))))))

(deftest progress-is-capped-at-one
  (testing "an over-budget project delivered its scope, not 130% of the contract"
    (let [c (psa/contract "alpha" :percent-complete :fee 1000000 :budget-hours 100)
          priced (map #(psa/price-entry cards %)
                      [(ts "w-1" "2026-01-01" "alpha" :engineer 130)])
          r (psa/recognize c priced false)]
      (is (= 1.0 (:revenue/progress r)))
      (is (= 1000000.0 (:revenue/amount r)))
      (testing "the overrun shows up in margin, which is where it belongs —
                130h billed against a fee that recognises only 100h of it"
        (is (= 1950000 (:margin/revenue (psa/margin priced))))))))

(deftest on-completion-recognises-nothing-until-it-is-done
  (let [c (psa/contract "alpha" :on-completion :fee 500000)
        priced (map #(psa/price-entry cards %)
                    [(ts "w-1" "2026-01-01" "alpha" :engineer 90)])]
    (is (zero? (:revenue/amount (psa/recognize c priced false))))
    (is (= 500000 (:revenue/amount (psa/recognize c priced true))))))

(deftest an-unknown-recognition-method-does-not-construct
  (is (nil? (psa/contract "alpha" :vibes-based))))
