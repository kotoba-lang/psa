# kotoba-psa

**Professional services in pure Clojure** — staffing, rates, margin,
utilization, invoices. A [kotoba-lang](https://github.com/kotoba-lang) capability
library for the PSA layer (the Kantata / BigTime / Certinia / NetSuite OpenAir /
Deltek Vantagepoint category).

It sits between two siblings and depends on neither:

| library | question |
|---|---|
| [`activity`](https://github.com/kotoba-lang/activity) | how much time was **observed**, and what for |
| **`psa`** | what that time is **worth**, who is committed to what, what may be invoiced |
| [`labor`](https://github.com/kotoba-lang/labor) | what the worker is **paid** |

All three read and emit the same `:ts/*` timesheet shape, so they compose without
any of them depending on the others.

## Maturity

| | |
|---|---|
| Role | capability |
| Dependencies | none |
| Tests | 25 tests, 56 assertions, all green |
| Runtime | `.cljc`, JVM + ClojureScript |
| Actor | `cloud-itonami/tehai` (手配) |

## The three invariants

Each has a test that fails if it stops holding. Every one is a place where PSA
tools conventionally produce a confident number instead of admitting a gap.

**1. No rate, no line.** Time whose `[project role]` has no rate card is reported
as unpriced. It is never billed at zero and never at a "default rate".

```clojure
(psa/price-entry cards (ts "w-1" "2026-01-01" "gamma" :engineer 8))
;; => {:priced/unpriced? true :priced/billable nil ...}
;;                             ^ nil is the absence of a price; 0 would be a price
```

**2. Margin needs both sides.** With a bill rate but no cost rate, margin is
`:unknown` — not equal to revenue. Fake profit is the classic PSA lie and it
comes from exactly this substitution. One uncosted entry is enough to poison
the whole figure, and the count travels with it.

```clojure
(psa/margin priced)
;; => {:margin/revenue 100000 :margin/cost :unknown :margin/amount :unknown
;;     :margin/ratio :unknown :margin/uncosted-count 1}
```

**3. Utilization needs a denominator.** With no declared capacity, utilization is
`:unknown`, not 1.0. A utilization figure is the number people are managed by,
so its denominator does not get invented.

## Contract

```clojure
(require '[kotoba.psa :as psa])

;; Rates. :cost may be omitted — margin then says :unknown rather than
;; pretending the work was free. A role-less card is the project-wide fallback.
(psa/rate-card "alpha" :engineer 15000 :cost 9000)
(psa/rate-card "beta" nil 10000)
(psa/rate-for cards "alpha" :engineer)

;; Staffing. Assignments prorate across a query window, assuming committed
;; hours are spread evenly — a real assumption, stated rather than buried.
(psa/assignment "a-1" "w-1" "alpha" :engineer from to 40)
(psa/capacity "w-1" from to 40)
(psa/allocation assignments capacities "w-1" [from to])
;; => {:allocation/committed-hours 50.0 :allocation/capacity-hours 40
;;     :allocation/capacity-known? true :allocation/over? true ...}
```

An **undeclared** capacity is not an infinite one, and it is not a violation
either: `:allocation/over?` stays false and `:allocation/capacity-known?` says
which case you are in. That is the deliberate difference from invariant 1 —
absent capacity is *unknown*, absent price is a *refusal to guess*.

```clojure
;; Value
(psa/price-entry cards entry)
(psa/margin priced)
(psa/utilization entries capacities "w-1" [from to] date->ms)

;; Invoicing
(psa/invoice "inv-1" "alpha" entries cards billed-key-set)
;; => {:invoice/lines [{:line/role :designer :line/hours 4.0 :line/rate 12000
;;                      :line/amount 48000} ...]
;;     :invoice/total 288000
;;     :invoice/entries [...]           ; the keys this invoice consumed
;;     :invoice/unpriced [...]          ; excluded, no rate card
;;     :invoice/excluded-already-billed [...]}

(psa/describe-gap inv)   ;; "" when nothing was left out
```

Lines are grouped by role, so a project billed at two rates shows two lines
rather than one blended figure nobody can check.

`entry-key` is content-derived — `[worker date project role]` — rather than a
generated id, so the same day's work read from two systems collides on purpose
and a re-imported timesheet cannot be billed twice.

`describe-gap` returns the empty string when nothing was excluded, so a caller
can append it unconditionally without printing a reassuring "no problems" line
that is really "nothing was checked".

## Composing

```clojure
(require '[kotoba.activity :as a] '[kotoba.labor :as labor])

;; observed -> priced
(psa/price-entry cards (assoc (first (a/->timesheet-entries "w-1" date-of att))
                              :ts/role :engineer))

;; observed -> paid
(labor/wages-for contract (a/->timesheet-entries "w-1" date-of att))
```

## Test

```bash
clojure -M:test
clojure -M:lint
```

## License

Apache-2.0.
