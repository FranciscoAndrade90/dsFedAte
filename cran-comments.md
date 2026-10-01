## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

## Companion client-side package

`dsRctFed` is a 'DataSHIELD' server-side package. Its companion analyst-side
package, `dsRctFedClient`, will be submitted to CRAN shortly after this one.
The two packages are developed in tandem but `dsRctFed` has no runtime
dependency on `dsRctFedClient`; the analyst-facing getting-started vignette
lives in `dsRctFedClient`, where the end user naturally looks for it.

## Test environments

* local Ubuntu, R 4.6.0
* win-builder (devel and release)

## Reverse dependencies

None (new package).
