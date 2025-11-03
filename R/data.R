#' Yarra River water quality data with period labels
#' 
#' Dataset used to compare the water quality at Yarra River from the 1990s to recent years.
#' Observations are labeled as "1990s" (year <= 1999) or "recent" (year >= 2015); only these
#' two periods are kept for direct comparison.
#' 
#' @format A data frame with 888 observations and 15 variables:
#' \describe{
#'   \item{parameter}{Character. Parameter name (pH, Water Temperature, Turbidity, EC).}
#'   \item{period}{Factor. "1990s" or "recent".}
#'   \item{value}{Numeric. Measured value (units depend on parameter).}
#'   \item{hour}{Integer. Hour of day (0–23).}
#'   \item{date, year, month, weekday, time, site_id, name}{Context fields if present.}
#' }
#' @details Created in \code{data-raw/prepare_yarra_data.R} from the raw files.
#' @source Victorian Water Measurement Information System (WMIS): \url{https://data.water.vic.gov.au/}
#' @examples
#' data(yarra_wq_period)
#' head(yarra_wq_period)
"yarra_wq_period"

#' Hourly medians (with IQR) by parameter and period
#' 
#' Summary for plotting: within each parameter–period–hour cell,
#' the median and IQR (25th/75th percentiles) of \code{value}.
#' 
#' @format A data frame with 68 observations and 6 variables:
#' \describe{
#'   \item{parameter}{Character. Parameter name.}
#'   \item{period}{Factor. "1990s" or "recent".}
#'   \item{hour}{Integer. 0–23.}
#'   \item{median}{Numeric. Median of \code{value}.}
#'   \item{q1}{Numeric. 25th percentile.}
#'   \item{q3}{Numeric. 75th percentile.}
#' }
#' @details Derived from \code{yarra_wq_period} in \code{data-raw/prepare_yarra_data.R}.
#' @examples
#' data(wq_hourly_median)
#' head(wq_hourly_median)
"wq_hourly_median"