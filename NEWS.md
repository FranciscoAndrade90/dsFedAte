# dsRctFed 0.0.0.9000

## New features

* `localAteDS()` now computes and returns delta-method variance quantities for
  binary outcomes when `compute_variance = TRUE`. Each site returns three
  additional elements: `g1_raw_k` and `g0_raw_k` (unnormalised gradient sums
  over all observations, divided by global *n* on the client), and `s_k_delta`
  (within-site scatter of δᵢ = μ̂₁(xᵢ) − μ̂₀(xᵢ) around its local mean τ̂ₖ).
  The per-arm Fisher information matrices are **not** returned by the server;
  the client obtains H_w⁻¹ directly from `ds.glm(viewVarCov = TRUE)$VarCovMatrix`
  (equal to H_w⁻¹ for binomial models, where dispersion = 1). This minimises
  per-site disclosure. The `heterogeneous` parameter has no effect for binary
  outcomes.

* `localAteDS()` gains a `heterogeneous` argument (default `TRUE`).  When
  `TRUE` and `compute_variance = TRUE`, the return list includes an additional
  element `n_k_X_bar`: a numeric vector of length `d` containing the column
  sums of the covariate design matrix (`n_k * X̄_k`).  The client uses this to
  assemble the global covariate mean `X̄` for the leverage-corrected variance
  estimator (Term I, eq. 17 of the companion variance paper).  Set to `FALSE`
  to suppress this extra disclosure when heterogeneity is known to be absent.
  Has no effect when `compute_variance = FALSE`.

---

## Breaking changes

* `localAteDS()` has a new signature. The old arguments `tau_str` and
  `gamma_str` (coefficient differences) are replaced by `alpha0_str`,
  `beta0_str`, `alpha1_str`, and `beta1_str` (individual arm intercepts and
  slopes). A new argument `outcome_name` is required so that residuals can be
  computed server-side.

## New features

* New server-side assign function `addStudyIdDS()` supports the adjusted GD
  estimator (Definition 9 of Khellaf, Bellet & Josse 2025). It appends K-1
  explicit integer dummy columns (`study_id2`, …, `study_idK`) to the
  server-side data frame. Plain integers are used rather than a factor because
  `model.frame()` inside `ds.glm()` drops empty factor levels to a single
  level on each server (every server holds one study), which triggers R's
  "contrasts can be applied only to factors with ≥ 2 levels" error.
  The function is declared under `AssignMethods` in `inst/DATASHIELD` so that
  `DSI::datashield.assign()` can invoke it.

* `localAteDS()` now uses `model.matrix()` instead of `as.matrix()` to build
  the covariate design matrix, correctly handling any column type via R's
  formula interface.

* `localAteDS()` gains a `compute_variance` argument (default `TRUE`). When
  `TRUE`, two additional scalars are returned beyond the point-estimate
  quantities `n_k`, `n_k_1`, and `tau_k`:
  - `s_k_scalar`: within-site projected scatter scalar, used for the
    covariate-mean uncertainty term via the parallel-axis decomposition.
  - `rss_k`: residual sum of squares across both arms, used for
    sigma-hat-squared with pooled df n - 2(d+1).
  Setting `compute_variance = FALSE` limits disclosure to the three scalars
  strictly needed for the point estimate.

## Earlier changes

* Added `dsBase::checkPermissivePrivacyControlLevel()` call to `localAteDS()`.
* Added disclosure risk check via `dsBase::listDisclosureSettingsDS()`.
* Added explicit numeric parsing validation for all string arguments.
* Declared `cli` and `dsBase` under `Imports` in `DESCRIPTION`.
* Added unit tests for `localAteDS()`, `.checkDisclosureRisk()`, and parsing
  helpers.
