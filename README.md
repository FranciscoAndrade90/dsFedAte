
# dsFedAte

<!-- badges: start -->
<!-- badges: end -->

Server-side 'DataSHIELD' package for federated estimation of the average
treatment effect (ATE) from a randomised controlled trial (RCT) distributed
across multiple data nodes. `dsFedAte` is installed on the server side (Opal
or Armadillo); the analyst-side companion package is `dsFedAteClient`.

## Installation

```r
install.packages("dsFedAte")
```

## Example

`dsFedAte` is called indirectly: the analyst runs `dsFedAteClient::ds.EstimateAte()`
on the client, which dispatches the server-side aggregate function `localAteDS()`
on each data node. A full worked example is provided in the `getting_started`
vignette of the client package:

```r
vignette("getting_started", package = "dsFedAteClient")
```
