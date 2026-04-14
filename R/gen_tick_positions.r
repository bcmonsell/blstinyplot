#' Generate tick mark positions 
#' 
#' Generate tick mark position for x or y axis for base R plots
#'
#' Version 1.2, 2026-04-06 
#'
#' @param this_series numeric or date vector, the values used for the x-axis.
#'        This is a required entry.
#' @param tick_interval integer scalar; the value separating the tick marks.
#'        Default is \code{2}.
#' @param is_date logical scalar; indicates if \code{this_series} is a vector
#'        of \code{Date} objects. 
#'        Default is \code{FALSE}, \code{this_series} is numeric.
#' @param time_interval character scalar; the time unites used to construct
#'        the x-axis if \code{this_series} is a \code{Date} object vector.
#'        Default is \code{"years"}.
#' @param this_base numeric scalar; number used to integer divide \code{this_series}
#'        for numeric vectors. 
#'        Default is \code{1}.
#' @return Generates values for major tick marks for the X and Y axis of plots.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_time <- time(AirPassengers)
#' air_Date <- 
#'    as.Date(paste(as.numeric(floor(time(AirPassengers))), 
#'            as.numeric(cycle(AirPassengers)), 
#'            "1", sep = "-"))
#' 
#' air_x_axis_time <- 
#'    gen_tick_positions(air_time, tick_interval = 3)
#' air_x_axis_Date <- 
#'    gen_tick_positions(air_Date, tick_interval = 3, is_date = TRUE)
#' air_y_axis      <- 
#'    gen_tick_positions(AirPassengers, tick_interval = 200, 
#'                       this_base = 100)
#' @export
gen_tick_positions <- 
   function(this_series = NULL, 
            tick_interval = 2, 
            is_date = FALSE, 
            time_interval = "years",
            this_base = 1) {
#   Author: Brian C. Monsell (OEUS) Version 1.2, 2026-04-06 
			
	if (is.null(this_series)) {
        stop("Argument this_series must be specified.")
	}
  
	if (is_date) {
		if (!lubridate::is.Date(this_series)) {
			stop("Argument this_series must be a Date object.")
		}
		first_date <- min(this_series)
		last_date  <- max(this_series) + lubridate::years(1)

		this_main_ticks <- 
		  seq(first_date, last_date, 
		      by = paste(tick_interval, time_interval, sep = " "))
	
	} else {
		this_max <- max(this_series)
		this_min <- min(this_series)
		if (!is.null(this_base)) {
			this_min <- (this_min %/% this_base) * this_base
			this_max <- ((this_max %/% this_base) + 1) * this_base
		}
		this_main_ticks <- 
		  seq(this_min, this_max, by = tick_interval)
	}
	
	return(this_main_ticks)

}