## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Companion client-side package

`dsFedAte` is a 'DataSHIELD' server-side package. Its companion
analyst-side package, `dsFedAteClient`, will be submitted to CRAN
shortly after this one. The two packages must be submitted sequentially
because `dsFedAteClient` lists `dsFedAte` under `Suggests`; submitting
them together would produce a "Suggests or Enhances not in mainstream
repositories" NOTE on the client. `dsFedAte` itself has no dependency
on `dsFedAteClient`; the analyst-facing `federated_rct` vignette lives
in `dsFedAteClient`, where the end user naturally looks for it.

## Test environments

* local Ubuntu, R 4.6.0
* win-builder (devel and release)

## Reverse dependencies

None (new package).
