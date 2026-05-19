# dsRctFed 0.0.0.9000

## Breaking changes

* `localAteDS()` has a new signature. The old arguments `tau_str` and
  `gamma_str` (coefficient differences) are replaced by `alpha0_str`,
  `beta0_str`, `alpha1_str`, and `beta1_str` (individual arm intercepts and
  slopes). A new argument `outcome_name` is required so that residuals can be
  computed server-side.

## New features

* `localAteDS()` now returns six values instead of two: `n_k`, `n_k_1`,
  `tau_k`, `x_bar_k`, `s_k_scalar`, and `rss_k`. The four new quantities
  support federated variance estimation without any additional communication
  round:
  - `n_k_1`: number of treated observations (needed for p-hat).
  - `x_bar_k`: local covariate means (needed for the global mean X-bar).
  - `s_k_scalar`: within-site projected scatter scalar (needed for the
    covariate-mean uncertainty term via the parallel-axis decomposition).
  - `rss_k`: residual sum of squares across both arms (needed for
    sigma-hat-squared with pooled df n - 2(d+1)).

## Earlier changes

* Added `dsBase::checkPermissivePrivacyControlLevel()` call to `localAteDS()`.
* Added disclosure risk check via `dsBase::listDisclosureSettingsDS()`.
* Added explicit numeric parsing validation for all string arguments.
* Declared `cli` and `dsBase` under `Imports` in `DESCRIPTION`.
* Added unit tests for `localAteDS()`, `.checkDisclosureRisk()`, and parsing
  helpers.
