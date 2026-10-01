# dsRctFed 0.1.2

## Changes

* The getting-started vignette has been removed from this package; the
  analyst-facing walkthrough is maintained in `dsRctFedClient`, where end users
  look for it. `dsRctFed` now has no soft dependency on `dsRctFedClient`,
  `DSI`, `DSLite`, `dsBaseClient`, `knitr`, `rmarkdown`, `MASS`, or `devtools`.

# dsRctFed 0.1.1

## Changes

* `localAteDS()` now returns arm-specific residual sums of squares `rss_k_1`
  and `rss_k_0` instead of the pooled `rss_k`, allowing the client to estimate
  arm-specific noise variances \eqn{\sigma_1^2} and \eqn{\sigma_0^2}.

# dsRctFed 0.1.0

Initial release.

## New features

* `localAteDS()` aggregate function computes the local ATE estimate and,
  optionally, the variance statistics (residual sum of squares and within-server
  projected scatter) needed for the delta-method variance. Set
  `compute_variance = FALSE` to limit disclosure to the three scalars strictly
  needed for the point estimate (`n_k`, `n_k_1`, `tau_k`).

* `localAteDS()` `heterogeneous` argument (default `TRUE`) additionally returns
  covariate column sums, allowing the client to assemble the global covariate
  mean for the leverage-corrected variance estimator.

* `addStudyIdDS()` assign function appends K-1 integer study-dummy columns to a
  server-side data frame, supporting the adjusted generalised difference estimator
  when study membership directly shifts the outcome.

* All functions enforce DataSHIELD disclosure controls: privacy control level is
  checked via `dsBase::checkPermissivePrivacyControlLevel()` and minimum subset
  size is validated against `nfilter.subset` from
  `dsBase::listDisclosureSettingsDS()`.
