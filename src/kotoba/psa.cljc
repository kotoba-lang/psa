(ns kotoba.psa
  "Professional services: staffing, rates, margin, utilization, invoices —
  pure data contracts.

  A kotoba-lang capability library for the PSA layer the fleet had only a
  legal-vertical slice of (the Kantata / BigTime / Certinia / NetSuite
  OpenAir / Deltek Vantagepoint category). It sits between two siblings
  and depends on neither:

    kotoba.activity — how much time was OBSERVED and what it was for
    kotoba.psa      — what that time is WORTH, who is committed to what,
                      and what may be invoiced   ← here
    kotoba.labor    — what the worker is PAID

  Timesheet entries are read in the `:ts/*` shape all three share
  (`:ts/worker :ts/date :ts/hours :ts/project`), so the libraries compose
  without any of them depending on the others.

  Amounts are plain numbers in the smallest unit of the account currency
  (e.g. cents), per hour for rates — the same convention as
  `kotoba.labor`, no BigDecimal assumption.

  Three invariants, each with a test that fails if it stops holding.
  Every one of them is a place where PSA tools conventionally produce a
  confident number instead of admitting a gap:

    1. No rate, no line. Time whose [project role] has no rate card is
       reported as unpriced. It is never billed at zero and never at a
       'default rate'.
    2. Margin needs both sides. With a bill rate but no cost rate the
       margin is :unknown, not equal to revenue. Fake profit is the
       classic PSA lie and it comes from exactly this substitution.
    3. Utilization needs a denominator. With no declared capacity,
       utilization is :unknown, not 1.0.

  Portable (.cljc) across JVM / ClojureScript / SCI / GraalVM."
  (:require [clojure.string :as str]))

(def ^:private ms-per-hour 3600000)

;; ---------------------------------------------------------------------------
;; Rate card
;; ---------------------------------------------------------------------------

(defn rate-card
  "Construct a rate card entry: what an hour of `role` on `project` is
  billed at, and what it costs. `cost` may be omitted — margin then
  reports :unknown rather than pretending the work was free."
  [project role billable & {:keys [cost currency]}]
  {:rate/project  project
   :rate/role     role
   :rate/billable billable
   :rate/cost     cost
   :rate/currency (or currency "USD")})

(defn rate-for
  "Look up the rate card for [project role]. A role-less card
  (`:rate/role nil`) is the project-wide fallback; an exact role match
  wins over it. Returns nil when neither exists — callers must treat
  that as unpriced, not as free."
  [cards project role]
  (let [in-project (filter #(= project (:rate/project %)) cards)]
    (or (first (filter #(= role (:rate/role %)) in-project))
        (first (filter #(nil? (:rate/role %)) in-project)))))

;; ---------------------------------------------------------------------------
;; Staffing — assignments against capacity
;; ---------------------------------------------------------------------------

(defn assignment
  "Commit `hours` of `person` to `project` over `[from to]` (epoch ms)."
  [id person project role from to hours]
  {:assign/id      id
   :assign/person  person
   :assign/project project
   :assign/role    role
   :assign/from    from
   :assign/to      to
   :assign/hours   hours})

(defn capacity
  "Declare that `person` has `hours` available over `[from to]`."
  [person from to hours]
  {:capacity/person person
   :capacity/from   from
   :capacity/to     to
   :capacity/hours  hours})

(defn- overlap-ms [[a-from a-to] [b-from b-to]]
  (max 0 (- (min a-to b-to) (max a-from b-from))))

(defn hours-in-window
  "The part of an assignment that falls inside `[from to]`, prorated by
  overlap. This assumes the committed hours are spread evenly across the
  assignment's span — a real assumption, stated here rather than buried:
  an assignment front-loaded into its first week reads as under-allocated
  in that week and over-allocated later."
  [a [from to]]
  (let [span (- (:assign/to a) (:assign/from a))]
    (if (pos? span)
      (* (:assign/hours a) (/ (double (overlap-ms [(:assign/from a) (:assign/to a)] [from to])) span))
      0.0)))

(defn allocation
  "How `person` is committed over `[from to]`: total prorated hours, the
  declared capacity, and whether the commitments exceed it.

  `:allocation/capacity-hours` is nil when no capacity covering the
  window was declared, and `:allocation/over?` is then false — an
  undeclared capacity is not an infinite one, but neither is it a
  violation to report. `:allocation/capacity-known?` says which case
  you are in."
  [assignments capacities person [from to]]
  (let [mine (filter #(= person (:assign/person %)) assignments)
        committed (reduce + 0.0 (map #(hours-in-window % [from to]) mine))
        cap (first (filter #(and (= person (:capacity/person %))
                                 (<= (:capacity/from %) from)
                                 (>= (:capacity/to %) to))
                           capacities))]
    {:allocation/person          person
     :allocation/window          [from to]
     :allocation/committed-hours committed
     :allocation/capacity-hours  (:capacity/hours cap)
     :allocation/capacity-known? (some? cap)
     :allocation/over?           (boolean (and cap (> committed (:capacity/hours cap))))
     :allocation/assignments     (mapv :assign/id mine)}))

;; ---------------------------------------------------------------------------
;; Value, cost, margin
;; ---------------------------------------------------------------------------

(defn- entry-role [entry] (:ts/role entry))

(defn price-entry
  "Price one `:ts/*` timesheet entry against the rate cards. Returns
  `{:priced/entry :priced/rate :priced/billable :priced/cost :priced/unpriced? }`.
  With no card, `:priced/unpriced?` is true and both amounts are nil —
  not zero. Zero is a price; nil is the absence of one, and the
  difference is the whole point."
  [cards entry]
  (let [card (rate-for cards (:ts/project entry) (entry-role entry))
        h (:ts/hours entry)]
    {:priced/entry     entry
     :priced/rate      card
     :priced/billable  (when card (* (:rate/billable card) h))
     :priced/cost      (when (and card (:rate/cost card)) (* (:rate/cost card) h))
     :priced/unpriced? (nil? card)}))

(defn margin
  "Revenue, cost and margin across priced entries.

  `:margin/amount` is `:unknown` unless EVERY contributing entry has both
  a bill rate and a cost rate. A partially costed set produces a margin
  that looks like profit and is really missing data, so this refuses to
  produce one and names how many entries were missing a cost."
  [priced]
  (let [billable (filter :priced/billable priced)
        revenue (reduce + 0 (map :priced/billable billable))
        missing-cost (remove :priced/cost billable)
        ;; keep, not map: a nil cost is exactly the case this function
        ;; exists to refuse, and summing it would throw before we got the
        ;; chance to say :unknown.
        cost (reduce + 0 (keep :priced/cost billable))
        complete? (and (seq billable) (empty? missing-cost))]
    {:margin/revenue        revenue
     :margin/cost           (if complete? cost :unknown)
     :margin/amount         (if complete? (- revenue cost) :unknown)
     :margin/ratio          (if (and complete? (pos? revenue))
                              (double (/ (- revenue cost) revenue))
                              :unknown)
     :margin/unpriced-count (count (filter :priced/unpriced? priced))
     :margin/uncosted-count (count missing-cost)}))

(defn utilization
  "Billable hours over declared capacity for `person` in `[from to]`.

  `:utilization/ratio` is `:unknown` when no capacity covering the window
  was declared. A utilization figure with an invented denominator is the
  number people are managed by, so it does not get invented here."
  [entries capacities person [from to] date->ms]
  (let [mine (filter #(= person (:ts/worker %)) entries)
        in-window (filter #(let [ms (date->ms (:ts/date %))]
                             (and ms (<= from ms) (< ms to)))
                          mine)
        billable (reduce + 0.0 (map :ts/hours in-window))
        cap (first (filter #(and (= person (:capacity/person %))
                                 (<= (:capacity/from %) from)
                                 (>= (:capacity/to %) to))
                           capacities))]
    {:utilization/person         person
     :utilization/window         [from to]
     :utilization/billable-hours billable
     :utilization/capacity-hours (:capacity/hours cap)
     :utilization/ratio          (if (and cap (pos? (:capacity/hours cap)))
                                   (double (/ billable (:capacity/hours cap)))
                                   :unknown)}))

;; ---------------------------------------------------------------------------
;; Invoicing
;; ---------------------------------------------------------------------------

(defn entry-key
  "Identity of a timesheet entry for double-billing purposes. Deliberately
  content-derived rather than a generated id: two systems that emit the
  same day's work for the same worker, project and role produce the same
  key, so re-importing a timesheet cannot bill it twice."
  [e]
  [(:ts/worker e) (:ts/date e) (:ts/project e) (:ts/role e)])

(defn invoice
  "Draft an invoice for `project` from `:ts/*` entries.

  `billed` is the set of `entry-key`s already on a committed invoice;
  those are excluded and reported, never silently re-billed. Entries with
  no rate card are excluded and reported too — the invoice total covers
  exactly what could be priced, and `:invoice/unpriced` says what could
  not.

  Lines are grouped by role, so a project billed at two rates shows two
  lines rather than one blended figure nobody can check."
  ([id project entries cards] (invoice id project entries cards #{}))
  ([id project entries cards billed]
   (let [mine (filter #(= project (:ts/project %)) entries)
         {dupes true fresh false} (group-by #(contains? billed (entry-key %)) mine)
         priced (map #(price-entry cards %) fresh)
         {unpriced true billable false} (group-by :priced/unpriced? priced)
         lines (->> billable
                    (group-by #(get-in % [:priced/rate :rate/role]))
                    (map (fn [[role items]]
                           {:line/role   role
                            :line/hours  (reduce + 0.0 (map (comp :ts/hours :priced/entry) items))
                            :line/rate   (get-in (first items) [:priced/rate :rate/billable])
                            :line/amount (reduce + 0 (map :priced/billable items))}))
                    (sort-by #(str (:line/role %)))
                    vec)]
     {:invoice/id       id
      :invoice/project  project
      :invoice/lines    lines
      :invoice/total    (reduce + 0 (map :line/amount lines))
      :invoice/currency (or (:rate/currency (first cards)) "USD")
      :invoice/entries  (mapv entry-key (map :priced/entry billable))
      :invoice/unpriced (mapv (comp entry-key :priced/entry) unpriced)
      :invoice/excluded-already-billed (mapv entry-key dupes)})))

(defn billable?
  "Would `entry` produce a line on an invoice? Useful as a pre-check so a
  caller can surface unpriced work before drafting rather than after."
  [cards entry]
  (some? (rate-for cards (:ts/project entry) (entry-role entry))))

(defn describe-gap
  "One-line human summary of what an invoice draft could not bill. Empty
  string when nothing was left out — so a caller can append it
  unconditionally without printing a reassuring 'no problems' line that
  is really 'nothing was checked'."
  [inv]
  (let [u (count (:invoice/unpriced inv))
        d (count (:invoice/excluded-already-billed inv))]
    (str/join "; "
              (cond-> []
                (pos? u) (conj (str u " entries had no rate card and were not billed"))
                (pos? d) (conj (str d " entries were already billed and were excluded"))))))

(defn hours->ms [h] (* h ms-per-hour))
(defn ms->hours [ms] (/ (double ms) ms-per-hour))
