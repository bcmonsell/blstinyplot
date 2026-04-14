#' Get NBER recession dates
#'
#' Add shading for US NBER recession dates \insertCite{nber}{blstinyplot}. 
#' Recession dates are taken from the \code{nberDates} function of the 
#' \code{tis} package \insertCite{tis2021}{blstinyplot}.
#'
#' Version 1.0, 2026-04-01
#'
#' @param start_recess numeric scalar; Starting date for plot. 
#'        Default is first recession starting date.
#' @param end_recess numeric scalar; Ending date for plot. 
#'        Default is last recession ending date.
#' @param add_recess_start numeric scalar; Starting date for an 
#'        additional recession period at the end of the series. 
#'        Default is not to add recession dates.
#' @param this_freq numeric scalar; frequency of \code{ts} time series. 
#'        Default is \code{12} (monthly).
#' @return Starting and ending dates for NBER recessions within a span of data
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' plot_limits <- c(1949, 1961)
#' thisRec <-
#'    get_recession_dates(start_recess = plot_limits[1], 
#'                        end_recess = plot_limits[2])
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
get_recession_dates <- 
    function(start_recess = NULL,
        end_recess = NULL, 
		add_recess_start = NULL,
        this_freq = 12) {
    # Author: Brian C. Monsell (OEUS) Version 1.0, 2026-04-01

    if (is.null(start_recess)) {
        stop("must specify the start_recess argument")
    }
	
    if (is.null(end_recess)) {
        stop("must specify the end_recess argument")
    }

    # if dates are length two, convert them to a scalar
    if (length(start_recess) > 1) {
        start_recess <- start_recess[1] + (start_recess[2] - 1) * this_freq
    }
    if (length(end_recess) > 1) {
        end_recess <- end_recess[1] + (end_recess[2] - 1) * this_freq
    }

    # read recession dates into data frame
    recessions_df = tis::nberDates()
    
    # Check if there are dates that need to be added to the end of the file.
    if (!is.null(add_recess_start)) {
        # set end to current date
        end_this_month <- 
		    as.numeric(substr(Sys.Date(),1,4)) + 
		    (as.numeric(substr(Sys.Date(),6,7)) - 1) / 12

        # convert \code{ts} style dates into \code{tis} format
        add_recess <- 
		    c(convert_date_to_tis(add_recess_start, this_freq = this_freq),
              convert_date_to_tis(end_this_month, this_freq = this_freq, 
			                      is_start = FALSE))

        # add dates to the end of the recessions matrix
        recessions_df <- rbind(recessions_df, add_recess)
    }

    # set up dimensions of recession data frame, initialize vector of recession dates in ts format
    recess_dim <- dim(recessions_df)
    recess_vec <- array(0, dim = recess_dim)

    # convert recession dates to ts format
    for (i in 1:recess_dim[1]) {
        for (j in 1:recess_dim[2]) {
            this_date <- recessions_df[i, j]
            recess_vec[i, j] <-
                as.numeric(substr(this_date, 1, 4)) + 
                (as.numeric(substr(this_date, 5, 6)) - 1)/12
        }
    }


    # set starting and ending dates
    if (is.null(start_recess)) {
        this_start <- recess_vec[1, 1]
    } else {
        this_start <- start_recess
    }

    if (is.null(end_recess)) {
        this_end <- recess_vec[recess_dim[1], 2]
    } else {
        this_end <- end_recess
    }

    # generate filters for which dates are before the starting date, 
	# after the ending date
    this_filter_1 <- !(recess_vec[, 1] < this_start)
    this_filter_2 <- !(recess_vec[, 2] > this_end)

    # generate filter for recessions that are fully contained in 
	# \code{this_start} to this_end
    this_filter <- this_filter_1 & this_filter_2

    # generate index vector
    indx <- seq(1, recess_dim[1])

    # Find last date where the start of the recession is before the start of the plot period.
    indx1 <- length(indx[!this_filter_1])

    # Check if ending date is before this_start.  If so, set to TRUE
    if (this_filter_2[indx1]) {
        if (recess_vec[indx1, 2] > this_start) {
            this_filter[indx1] <- this_filter_2[indx1]
        }
    }

    # Find first date where the end of the recession is after the end of the plot period.
    indx2 <- indx[!this_filter_2][1]

    # Check if starting date is before this_end.  If so, set to TRUE
    if (!is.na(indx2)) {
        if (this_filter_1[indx2]) {
            if (recess_vec[indx2, 1] < this_end) {
                this_filter[indx2] <- this_filter_1[indx2]
            }
        }
    }

    # Use this_filter to collect recession dates in plotting period
    finalRec <- matrix(recess_vec[cbind(this_filter, this_filter)], ncol = recess_dim[2])

    # Reset stating and ending dates of recession to this_start and this_end if not in the plotting
    # period
    if (finalRec[1, 1] < this_start) {
        finalRec[1, 1] <- this_start
    }
    if (finalRec[nrow(finalRec), 2] > this_end) {
        finalRec[nrow(finalRec), 2] <- this_end
    }

    return(finalRec)
}
