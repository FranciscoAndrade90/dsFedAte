
# dsRctFed

<!-- badges: start -->
<!-- badges: end -->

Server-side 'DataSHIELD' package for federated estimation of the average
treatment effect (ATE) from a randomised controlled trial (RCT) distributed
across multiple data nodes. `dsRctFed` is installed on the server side (Opal
or Armadillo); the analyst-side companion package is `dsRctFedClient`.

## Installation

```r
install.packages("dsRctFed")
```

## Example

`dsRctFed` is called indirectly: the analyst runs `dsRctFedClient::ds.EstimateAte()`
on the client, which dispatches the server-side aggregate function `localAteDS()`
on each data node. A full worked example is provided in the `getting_started`
vignette:

```r
vignette("getting_started", package = "dsRctFed")
```
