# Tests for calculate_aal

testthat::test_that("calculate_aal integrates a simple two-point curve correctly", {
  # rp = 10, 100; loss = 0.1, 1.0
  # Boundary 1 added (RP=1 not in input): (rp=1, aep=1, loss=0)
  # Boundary 2 added (flat cap beyond max RP): (aep=0, loss=1.0)
  # Points in decreasing-AEP order:
  #   rp=1,   aep=1,    loss=0
  #   rp=10,  aep=0.1,  loss=0.1
  #   rp=100, aep=0.01, loss=1.0
  #   rp=Inf, aep=0,    loss=1.0
  # Segment areas (trapezoidal):
  #   seg1 = (0+0.1)/2   * (1-0.1)    = 0.05  * 0.9  = 0.045
  #   seg2 = (0.1+1.0)/2 * (0.1-0.01) = 0.55  * 0.09 = 0.0495
  #   seg3 = (1.0+1.0)/2 * (0.01-0)   = 1.0   * 0.01 = 0.01
  # AAL = 0.045 + 0.0495 + 0.01 = 0.1045
  result <- calculate_aal(rp_vector = c(10, 100), loss_vector = c(0.1, 1.0))
  testthat::expect_equal(result, 0.1045, tolerance = 1e-8)
})

testthat::test_that("calculate_aal is order-independent (sorts internally)", {
  result_sorted <- calculate_aal(c(10, 100), c(0.1, 1.0))
  result_unsorted <- calculate_aal(c(100, 10), c(1.0, 0.1))
  testthat::expect_equal(result_unsorted, result_sorted, tolerance = 1e-8)
})

testthat::test_that("calculate_aal handles a single return period as a flat curve", {
  # rp=1 only, loss=0.2: boundary 1 not added (RP=1 already supplied),
  # boundary 2 caps flat at 0.2 all the way to AEP=0 -> AAL = 0.2
  result <- calculate_aal(rp_vector = 1, loss_vector = 0.2)
  testthat::expect_equal(result, 0.2, tolerance = 1e-8)
})

testthat::test_that("calculate_aal returns 0 when all supplied losses are 0", {
  result <- calculate_aal(rp_vector = c(5, 10, 25, 50, 100), loss_vector = rep(0, 5))
  testthat::expect_equal(result, 0, tolerance = 1e-8)
})

testthat::test_that("calculate_aal works with realistic multi-RP flood data (loss is monotonically increasing in RP)", {
  # A realistic-shaped damage-fraction curve, increasing with return period
  rp <- c(5, 10, 25, 50, 100)
  loss <- c(0.01, 0.03, 0.08, 0.15, 0.25)
  result <- calculate_aal(rp, loss)

  # AAL must be strictly positive and strictly less than the largest single loss
  # (since it's a probability-weighted average, not the worst case)
  testthat::expect_gt(result, 0)
  testthat::expect_lt(result, max(loss))
})

testthat::test_that("calculate_aal is reused identically for dollar and fractional units", {
  # Same shape curve, once as a fraction (0-1), once scaled to dollars -- the
  # function itself is unit-agnostic, so results should scale linearly.
  rp <- c(5, 10, 25, 50, 100)
  loss_fraction <- c(0.01, 0.03, 0.08, 0.15, 0.25)
  loss_dollars <- loss_fraction * 1000

  aal_fraction <- calculate_aal(rp, loss_fraction)
  aal_dollars <- calculate_aal(rp, loss_dollars)

  testthat::expect_equal(aal_dollars, aal_fraction * 1000, tolerance = 1e-8)
})

testthat::test_that("calculate_aal drops NA (return_period, loss) pairs", {
  result_with_na <- calculate_aal(
    rp_vector = c(10, NA, 100),
    loss_vector = c(0.1, 0.5, 1.0)
  )
  result_without_na <- calculate_aal(rp_vector = c(10, 100), loss_vector = c(0.1, 1.0))
  testthat::expect_equal(result_with_na, result_without_na, tolerance = 1e-8)
})

testthat::test_that("calculate_aal errors on mismatched vector lengths", {
  testthat::expect_error(
    calculate_aal(rp_vector = c(10, 100), loss_vector = c(0.1, 0.5, 1.0)),
    "same length"
  )
})

testthat::test_that("calculate_aal errors on return periods below 1 (including 0 and negative)", {
  testthat::expect_error(
    calculate_aal(rp_vector = c(0, 100), loss_vector = c(0.1, 1.0)),
    ">= 1"
  )
  testthat::expect_error(
    calculate_aal(rp_vector = c(-5, 100), loss_vector = c(0.1, 1.0)),
    ">= 1"
  )
  # RP strictly between 0 and 1 would silently skip the RP=1/AEP=1 boundary
  # injection if left unvalidated -- must be rejected, not silently accepted.
  testthat::expect_error(
    calculate_aal(rp_vector = c(0.5, 100), loss_vector = c(0.2, 1.0)),
    ">= 1"
  )
})

testthat::test_that("calculate_aal errors on duplicate return periods", {
  testthat::expect_error(
    calculate_aal(rp_vector = c(10, 10, 100), loss_vector = c(0.1, 0.2, 1.0)),
    "duplicate"
  )
})

testthat::test_that("calculate_aal errors on empty input", {
  testthat::expect_error(
    calculate_aal(rp_vector = numeric(0), loss_vector = numeric(0)),
    "must not be empty"
  )
})

testthat::test_that("calculate_aal drops a non-numeric-parseable entry instead of crashing", {
  # A value that isn't NA but also isn't numeric-parseable (e.g. a stray string in
  # what should be a numeric column) must be treated like the other NA-pair-dropping
  # cases -- caught by coercion and filtered out -- not slip through as an uncaught
  # NA that later crashes an `if (any(...))` check.
  result_with_bad_entry <- calculate_aal(
    rp_vector = c("10", "abc", "100"),
    loss_vector = c(0.1, 0.5, 1.0)
  )
  result_without_bad_entry <- calculate_aal(rp_vector = c(10, 100), loss_vector = c(0.1, 1.0))
  testthat::expect_equal(result_with_bad_entry, result_without_bad_entry, tolerance = 1e-8)
})

testthat::test_that("calculate_aal accepts numeric-looking character input", {
  result_character <- calculate_aal(rp_vector = c("10", "100"), loss_vector = c("0.1", "1.0"))
  result_numeric <- calculate_aal(rp_vector = c(10, 100), loss_vector = c(0.1, 1.0))
  testthat::expect_equal(result_character, result_numeric, tolerance = 1e-8)
})
