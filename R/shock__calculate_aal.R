#' Calculate Average Annual Loss (AAL) via trapezoidal integration over exceedance probability
#'
#' @title Calculate Average Annual Loss (AAL)
#' @description Integrates a loss-per-return-period curve into a single Average Annual Loss
#'   (AAL) figure using the standard trapezoidal-rule method: convert each return period (RP)
#'   to an annual exceedance probability (AEP = 1/RP), then integrate loss against AEP. Two
#'   boundary assumptions close the curve at both ends (see Details) so the integral is
#'   well-defined even though the input only covers a finite set of return periods.
#'
#'   Unit-agnostic by design: works identically whether `loss_vector` is expressed in dollars
#'   (e.g. a building's physical-damage AAL, `damage_factor * cost_factor` per return period)
#'   or as a loss fraction/rate (e.g. a business-disruption or agricultural physical-damage AAL
#'   rate) -- the same function is reused for every AAL channel, it is never re-derived.
#' @details
#'   Two boundary conditions are assumed, and are the only assumptions this function makes:
#'   \itemize{
#'     \item Below the smallest available return period (i.e. at RP = 1, AEP = 1 -- a
#'       near-certain, annual event): loss is assumed to be 0. This point is only injected when
#'       `rp_vector` does not already include RP = 1; if it does, that supplied value is used
#'       as-is instead.
#'     \item Above the largest available return period (i.e. as AEP -> 0, rarer than any
#'       supplied RP): loss is capped flat at the value observed for the largest RP (flat
#'       extrapolation), rather than decaying to zero. This mirrors how the underlying
#'       depth-damage/business-disruption curves already cap out flat at their maximum depth
#'       bin in `damage_and_cost_factors.csv`.
#'   }
#'   Integration is the trapezoidal rule across every consecutive pair of points (including the
#'   two boundary segments above), taken in order of decreasing AEP (i.e. increasing return
#'   period): `AAL = sum_i (L_i + L_{i+1}) / 2 * (AEP_i - AEP_{i+1})`.
#' @param rp_vector Numeric vector of return periods (years), each >= 1, no duplicates. May be
#'   supplied in any order.
#' @param loss_vector Numeric vector of losses corresponding to each return period, same length
#'   and order as `rp_vector`. In whatever units the caller wants the AAL expressed in.
#' @return Single numeric value: the Average Annual Loss, in the same units as `loss_vector`.
#' @examples
#' \dontrun{
#' # Loss (as a fraction of revenue) at return periods 5, 10, 25, 50, 100 years
#' calculate_aal(
#'   rp_vector = c(5, 10, 25, 50, 100),
#'   loss_vector = c(0.01, 0.03, 0.08, 0.15, 0.25)
#' )
#' }
#' @export
calculate_aal <- function(rp_vector, loss_vector) {
  if (length(rp_vector) == 0 || length(loss_vector) == 0) {
    stop("calculate_aal: rp_vector and loss_vector must not be empty")
  }
  if (length(rp_vector) != length(loss_vector)) {
    stop("calculate_aal: rp_vector and loss_vector must be the same length")
  }

  # Coerce first, then drop pairs with a missing return period or loss (both are
  # required to place a point) -- coercing before filtering ensures a value that
  # fails to parse as numeric (e.g. a stray non-numeric string) is caught here
  # rather than silently becoming an uncaught NA later.
  rp_all <- suppressWarnings(as.numeric(rp_vector))
  loss_all <- suppressWarnings(as.numeric(loss_vector))
  keep <- !is.na(rp_all) & !is.na(loss_all)
  rp <- rp_all[keep]
  loss <- loss_all[keep]

  if (length(rp) == 0) {
    stop("calculate_aal: no non-NA (return_period, loss) pairs supplied")
  }
  if (any(rp < 1)) {
    stop("calculate_aal: return periods must be >= 1 (RP = 1 is the AEP = 1 boundary)")
  }
  if (anyDuplicated(rp) > 0) {
    stop("calculate_aal: rp_vector must not contain duplicate return periods")
  }

  # Sort by increasing return period (equivalently, decreasing AEP)
  ord <- order(rp)
  rp <- rp[ord]
  loss <- loss[ord]
  aep <- 1 / rp

  # Boundary 1: at RP = 1 (AEP = 1), assume loss = 0 -- unless the caller already
  # supplied a value at RP = 1, in which case that measured value is used instead.
  if (rp[1] > 1) {
    rp <- c(1, rp)
    aep <- c(1, aep)
    loss <- c(0, loss)
  }

  # Boundary 2: above the largest available RP (as AEP -> 0), cap the loss flat at
  # the value observed for the largest RP (flat extrapolation, not decay to zero).
  aep <- c(aep, 0)
  loss <- c(loss, loss[length(loss)])

  # Trapezoidal integration over strictly decreasing AEP
  n <- length(aep)
  segment_aal <- (loss[-n] + loss[-1]) / 2 * (aep[-n] - aep[-1])

  sum(segment_aal)
}
