# dsRctFed 0.0.0.9000

## Breaking changes

* `localAteDS()` has a new signature. The old arguments `tau_str` and
  `gamma_str` (coefficient differences) are replaced by `alpha0_str`,
  `beta0_str`, `alpha1_str`, and `beta1_str` (individual arm intercepts and
  slopes). A new argument `outcome_name` is required so that residuals can be
  computed server-side.

## New features

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
