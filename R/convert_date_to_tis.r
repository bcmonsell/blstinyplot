#' Convert ts dates to tis format
#'
#' Convert dates used for monthly (or quarterly) \code{ts} series 
#' to \code{tis} \insertCite{tis2021}{blstinyplot} formats.
#'
#' Version 1.0, 2026-04-01
#' 
#' @param this_date numeric scalar or vector; \code{ts} date to be converted
#' @param this_freq numeric scalar; frequency of \code{ts} time series. 
#'        Default is 12 (monthly).
#' @param is_start logical scalar; is date assumed to be the beginning of the month? 
#'        Default is TRUE; if FALSE, date is assumed to be at the 
#'        end of the month.
#' @param return_tis logical scalar; If true, return as \code{tis} object; 
#'        otherwise return as integer. 
#'        Default is FALSE.
#' @return A \code{tis} index value that is the equivalent of the 
#'         \code{ts} date.
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' this_month <- as.numeric(substr(Sys.Date(),1,4)) +
#'     (as.numeric(substr(Sys.Date(),6,7)) - 1) / 12
#' end_this_month_tis <- 
#'     convert_date_to_tis(this_month, this_freq = 12,
#'                         is_start = FALSE, return_tis = TRUE)
#' start_this_month_tis <- 
#'     convert_date_to_tis(this_month, this_freq = 12,
#'                         is_start = TRUE, return_tis = TRUE)
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
convert_date_to_tis <- 
    function(this_date, 
		this_freq = 12, 
		is_start = TRUE, 
		return_tis = FALSE) {
   # Author: Brian C. Monsell (OEUS) Version 1.0, 2026-04-01

   # Set up year, period of date entered
   if (length(this_date) > 1) {
      this_year <- this_date[1]
      this_period <- this_date[2]
   } else {
      this_year <- this_date %/% 1
      this_period <- (this_date %% 1 * this_freq) + 1
   }

   # if series is quarterly, set month of first observation in the quarter.
   if (this_freq == 4) {
      this_period <- (this_period - 1) * 3 + 1
   }

   # Set up label for period with leading zeros if necessary
   this_period_label <- gsub(" ", "0", format(this_period, width = 2))

   # return daily tis index for either the start or the end of the month or quarter
   if (is_start) {
      this_daily_tis <- 
	      tis::ti(as.integer(paste0(this_year, this_period_label, "01")), tif = "daily")
   } else {
      if (this_freq == 12) {
         this_daily_tis <- 
		     tis::ti(tis::lastDayOf(tis::ti(as.integer(paste0(this_year, this_period_label, "01")), tif = "monthly")), tif = "daily")
      } else {
         this_daily_tis <- 
		     tis::ti(tis::lastDayOf(tis::ti(as.integer(paste0(this_year, this_period_label, "01")), tif = "quarterly")), tif = "daily")
      }
   }

   if (return_tis) {
      return(this_daily_tis)
   } else {
      return(as.integer(as.character(this_daily_tis)))
   }
}

