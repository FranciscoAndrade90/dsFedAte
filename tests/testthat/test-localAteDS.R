library(testthat)

options(
  datashield.privacyControlLevel = "banana",
  nfilter.tab                    = 3,
  nfilter.subset                 = 3,
  nfilter.glm                    = 0.33,
  nfilter.string                 = 80,
  nfilter.stringShort            = 20,
  nfilter.kNN                    = 3,
  nfilter.levels.density         = 0.33,
  nfilter.levels.max             = 40,
  nfilter.noise                  = 0.25,
  nfilter.privacy.old            = 5
)

make_test_data <- function(n = 20) {
  set.seed(42)
  data.frame(
    X1 = rnorm(n),
    X2 = rnorm(n),
    W  = rbinom(n, 1, 0.5)
  )
}

# Happy path ------------------------------------------------------------------

test_that("localAteDS returns n_k and tau_k for valid input", {
  options(datashield.privacyControlLevel = "banana")
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "X1,X2", "W", "0.5", "0.1,0.2")
  expect_type(result, "list")
  expect_named(result, c("n_k", "tau_k"))
  expect_equal(result$n_k, nrow(test_df))
  expect_type(result$tau_k, "double")
  expect_false(is.na(result$tau_k))
})

test_that("localAteDS handles a single covariate", {
  options(datashield.privacyControlLevel = "banana")
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "X1", "W", "-0.3", "0.5")
  expect_false(is.na(result$tau_k))
})

# Sad path: privacy control level ---------------------------------------------

test_that("localAteDS is blocked under non-permissive privacy level", {
  options(datashield.privacyControlLevel = "non-permissive")
  test_df <- make_test_data()
  expect_error(
    localAteDS("test_df", "X1,X2", "W", "0.5", "0.1,0.2"),
    "BLOCKED"
  )
  options(datashield.privacyControlLevel = "banana")
})

# Sad path: disclosure check --------------------------------------------------

test_that("localAteDS errors when dataset is below nfilter.subset", {
  options(datashield.privacyControlLevel = "banana", nfilter.subset = 10)
  test_df <- make_test_data(n = 5)
  expect_error(
    localAteDS("test_df", "X1,X2", "W", "0.5", "0.1,0.2"),
    "too few rows"
  )
  options(nfilter.subset = 3)
})

test_that(".checkDisclosureRisk aborts when nrow < nfilter.subset", {
  options(nfilter.subset = 5)
  small_df <- data.frame(X1 = 1:3, X2 = 1:3, W = c(0L, 1L, 0L))
  expect_error(.checkDisclosureRisk(small_df), "too few rows")
  options(nfilter.subset = 3)
})

# Sad path: numeric parsing ---------------------------------------------------

test_that(".parse_numeric_scalar aborts on non-numeric input", {
  expect_error(.parse_numeric_scalar("abc", "tau_str"), "could not be parsed")
})

test_that(".parse_numeric_vector aborts when any element is non-numeric", {
  expect_error(.parse_numeric_vector("0.1,abc,0.3", "gamma_str"), "could not be fully parsed")
})
