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

# True DGP: alpha0=1, alpha1=1.5, beta0=(0.3,-0.2), beta1=(0.3,-0.2)
ALPHA0 <- 1.0;   ALPHA1 <- 1.5
BETA0  <- c(0.3, -0.2);  BETA1 <- c(0.3, -0.2)

make_test_data <- function(n = 30, seed = 42) {
  set.seed(seed)
  df    <- data.frame(X1 = rnorm(n), X2 = rnorm(n), W = rbinom(n, 1, 0.5))
  df$Y  <- ALPHA0 + (ALPHA1 - ALPHA0) * df$W +
            BETA0[1] * df$X1 + BETA0[2] * df$X2 + rnorm(n, sd = 0.5)
  df
}

# localAteDS uses eval(parse(), envir = parent.frame()) — the test data must be
# visible in the calling frame. Assigning globally is the standard pattern for
# direct server-function unit tests (same approach used in setupDSLite.R).
assign("test_df", make_test_data(), envir = .GlobalEnv)

# Happy path: return structure -------------------------------------------------

test_that("localAteDS returns all five expected elements", {
  options(datashield.privacyControlLevel = "banana")
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  expect_type(result, "list")
  expect_named(result, c("n_k", "n_k_1", "tau_k", "s_k_scalar", "rss_k"))
})

test_that("localAteDS n_k equals nrow of the data frame", {
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  expect_equal(result$n_k, nrow(test_df))
})

test_that("localAteDS n_k_1 is between 0 and n_k", {
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  expect_gte(result$n_k_1, 0L)
  expect_lte(result$n_k_1, result$n_k)
})

test_that("localAteDS tau_k is finite", {
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  expect_false(is.na(result$tau_k))
  expect_false(is.infinite(result$tau_k))
})

test_that("localAteDS tau_k matches manual computation", {
  test_df  <- make_test_data()
  result   <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  x_bar    <- colMeans(test_df[, c("X1", "X2")])
  tau_expected <- (ALPHA1 - ALPHA0) + sum(x_bar * (BETA1 - BETA0))
  expect_equal(result$tau_k, tau_expected)
})

test_that("localAteDS rss_k is non-negative", {
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  expect_gte(result$rss_k, 0)
})

test_that("localAteDS rss_k matches manual residual computation", {
  test_df  <- make_test_data()
  result   <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  idx_1    <- test_df$W == 1L
  pred_1   <- ALPHA1 + as.matrix(test_df[idx_1,  c("X1","X2")]) %*% BETA1
  pred_0   <- ALPHA0 + as.matrix(test_df[!idx_1, c("X1","X2")]) %*% BETA0
  rss_exp  <- sum((test_df$Y[idx_1]  - pred_1)^2) +
              sum((test_df$Y[!idx_1] - pred_0)^2)
  expect_equal(result$rss_k, as.numeric(rss_exp))
})

test_that("localAteDS s_k_scalar is non-negative", {
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  expect_gte(result$s_k_scalar, 0)
})

test_that("localAteDS s_k_scalar is zero when gamma is zero", {
  assign("test_df", make_test_data(), envir = .GlobalEnv)
  # BETA1 == BETA0, so gamma = 0 and every projection equals zero
  result  <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","))
  expect_equal(result$s_k_scalar, 0, tolerance = 1e-10)
})

test_that("localAteDS s_k_scalar is positive for nonzero gamma", {
  assign("test_df", make_test_data(), envir = .GlobalEnv)
  result  <- localAteDS(
    "test_df", "Y", "X1,X2", "W",
    "1.0", "0.3,-0.2",
    "1.5", "0.6, 0.1"   # different beta1 → nonzero gamma
  )
  expect_gt(result$s_k_scalar, 0)
})

test_that("localAteDS handles a single covariate", {
  assign("test_df", make_test_data(), envir = .GlobalEnv)
  result  <- localAteDS("test_df", "Y", "X1", "W", "1.0", "0.3", "1.5", "0.3")
  expect_named(result, c("n_k", "n_k_1", "tau_k", "s_k_scalar", "rss_k"))
  expect_false(is.na(result$tau_k))
})

# compute_variance = FALSE -------------------------------------------------

test_that("localAteDS with compute_variance=FALSE returns only three elements", {
  test_df <- make_test_data()
  result  <- localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ","),
             compute_variance = FALSE)
  expect_named(result, c("n_k", "n_k_1", "tau_k"))
})

test_that("localAteDS compute_variance=FALSE tau_k matches full call", {
  test_df  <- make_test_data()
  full     <- localAteDS("test_df", "Y", "X1,X2", "W",
              as.character(ALPHA0), paste(BETA0, collapse = ","),
              as.character(ALPHA1), paste(BETA1, collapse = ","))
  ate_only <- localAteDS("test_df", "Y", "X1,X2", "W",
              as.character(ALPHA0), paste(BETA0, collapse = ","),
              as.character(ALPHA1), paste(BETA1, collapse = ","),
              compute_variance = FALSE)
  expect_equal(ate_only$tau_k, full$tau_k)
  expect_equal(ate_only$n_k,   full$n_k)
  expect_equal(ate_only$n_k_1, full$n_k_1)
})

# Sad path: privacy control level ----------------------------------------------

test_that("localAteDS is blocked under non-permissive privacy level", {
  options(datashield.privacyControlLevel = "non-permissive")
  assign("test_df", make_test_data(), envir = .GlobalEnv)
  expect_error(localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ",")), "BLOCKED")
  options(datashield.privacyControlLevel = "banana")
})

# Sad path: disclosure check ---------------------------------------------------

test_that("localAteDS errors when dataset is below nfilter.subset", {
  options(nfilter.subset = 10)
  assign("test_df", make_test_data(n = 5), envir = .GlobalEnv)
  expect_error(localAteDS("test_df", "Y", "X1,X2", "W",
             as.character(ALPHA0), paste(BETA0, collapse = ","),
             as.character(ALPHA1), paste(BETA1, collapse = ",")), "too few rows")
  options(nfilter.subset = 3)
})

test_that(".checkDisclosureRisk aborts when nrow < nfilter.subset", {
  options(nfilter.subset = 5)
  small_df <- data.frame(X1 = 1:3, X2 = 1:3, W = c(0L, 1L, 0L))
  expect_error(.checkDisclosureRisk(small_df), "too few rows")
  options(nfilter.subset = 3)
})

# Sad path: numeric parsing ----------------------------------------------------

test_that(".parse_numeric_scalar aborts on non-numeric input", {
  expect_error(.parse_numeric_scalar("abc", "alpha0_str"), "could not be parsed")
})

test_that(".parse_numeric_vector aborts when any element is non-numeric", {
  expect_error(.parse_numeric_vector("0.1,abc,0.3", "beta0_str"), "could not be fully parsed")
})
