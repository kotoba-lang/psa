# CLAUDE.md — kotoba-lang/psa

Professional services: staffing, rates, margin, utilization, invoices, expenses,
subcontractors, revenue recognition, currency. Zero dependencies.

## Four invariants. Do not weaken them.

Each sits where PSA tools conventionally produce a confident number instead of
admitting a gap.

1. **No rate, no line.** Unpriced time is reported, never billed at zero and
   never at a default. `nil` is the absence of a price; `0` is a price.
2. **Margin needs both sides.** One entry missing a cost rate and the whole
   figure is `:unknown`. Substituting revenue produces something that looks like
   profit and is really missing data.
3. **Utilization needs a denominator.** No declared capacity, no ratio — not 1.0.
4. **No rate, no conversion.** An amount whose FX rate is unknown is excluded
   from the total and named; `:total/complete?` says which you have. Rates are
   directional and are never inverted automatically.

## The deliberate asymmetry

Assigning against **undeclared capacity is allowed** (`:allocation/over?` stays
false, `:capacity-known?` says why). Billing against an **undeclared rate is
held**. Absent capacity is *unknown*; absent price is a *refusal to guess*. If
you find yourself making these consistent, you are removing the distinction the
library exists to draw.

## Conventions

- Amounts are plain numbers in the smallest currency unit. No BigDecimal.
- `entry-key` is content-derived (`[worker date project role]`), so the same
  day's work from two systems collides on purpose and cannot be billed twice.
- `recognize` returns `:unknown` rather than an estimate when the method's
  inputs are absent. Progress caps at 1.0 — the overrun belongs in margin.
- Keep this repo dependency-free; it composes with its siblings by shape only.

## Test

    clojure -M:test && clojure -M:lint
