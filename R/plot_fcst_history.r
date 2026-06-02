#' Generate forecast history plot
#'
#' Generate forecast history plot, which compares the sum of squared forecast errors 
#' for two models. 
#'
#' Version 1.3, 2026-05-18
#'
#' @param seas_mdl1 A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{blstinyplot} for the
#'        first model. 
#'        This is a required entry.
#' @param seas_mdl2 A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package for the second model fit to the same series as
#'        for \code{seas_mdl1}. 
#'        This is a required entry.
#' @param start_hist integer scalar; starting date for the history analysis. 
#'        Could be an array of length 2; will be converted to a scalar.
#'        This is a required entry.
#' @param main_title Character string; main title of plot.  
#'        Default is  \code{'Differences in the Sum of Squared Forecast Errors'}.
#' @param name_mdl1 Character string; Description of first model for use in the subtitle. 
#'        Default is \code{'Model 1'}.
#' @param name_mdl2 Character string; Description of second model for use in the subtitle. 
#'        Default is \code{'Model 2'}.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_frame Logical scalar; indicates the plot will have a frame. 
#'        Default is the plot frame will be produced. 
#' @param this_col Character array of length 2; color used for each forecast lag. 
#'        Default is colors from the \code{rcartocolor} palette \code{"Bold"} 
#'        \insertCite{rcartocolor}{blstinyplot}; this palette is extracted using the
#'        \code{paletteer} package \insertCite{paletteer}{blstinyplot}.
#' @param this_plot_cex Numeric scalar; scaling for the plot itself. 
#'        Default is \code{0.8}.
#' @param this_lab_cex Numeric scalar; scaling for plot labels. 
#'        Default is the value of \code{this_plot_cex}.
#' @param this_main_cex Numeric scalar; scaling for main plot title. 
#'        Default is the value of \code{this_plot_cex + 0.1}.
#' @param this_sub_cex Numeric scalar; scaling for plot subtitle. 
#'        Default is the value of \code{this_plot_cex}.
#' @param this_axis_cex Numeric scalar; scaling for plot axis. 
#'        Default is the value of \code{this_plot_cex}.
#' @param this_mar Numeric vector; set margins for the plot. 
#'        Default is \code{c(5.1, 2.1, 5.1, 0.5)}.
#' @param main_title_line Integer scalar; position of main title of plot.  
#'        Default is \code{2.25}.
#' @param sub_title_line Integer scalar; position of main title of plot.  
#'        Default is \code{1}.
#' @param display_color_strip Logical scalar; indicates if the display color 
#'        will be stripped of trailing numbers. 
#'        Default is \code{TRUE}.
#' @param x_axis_label Character string; x-axis label for plot, if specified.
#' @param y_axis_label Character string; y-axis label for plot, if specified.
#' @param x_axis_main_ticks Numeric vector; X-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param y_axis_main_ticks Numeric vector; Y-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param this_reset Logical scalar; if TRUE, the values of \code{par} are reset. 
#'        Default is \code{TRUE}.
#' @return Generate forecast history plot. Can be more than one series. If series not specified, print out error message and return NULL.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas_mdl <- 
#'     seasonal::seas(AirPassengers, x11='', 
#'         slidingspans = '', transform.function = 'log',
#'         arima.model = '(0 1 1)(0 1 1)', 
#'         regression.aictest = NULL, outlier = NULL,
#'         forecast.maxlead = 36, 
#'         check.print = c( 'pacf', 'pacfplot' ),
#'         history.fstep = c(1, 12), history.estimates = 'fcst', 
#'         history.save = 'fcsterrors')
#' air_seas_mdl2 <- 
#'     seasonal::seas(AirPassengers, x11='', 
#'         slidingspans = '', transform.function = 'log',
#'         arima.model = '(0 1 1)(0 1 1)', 
#'         regression.variables = c("td"),
#'         forecast.maxlead = 36, 
#'         check.print = c( 'pacf', 'pacfplot' ),
#'         history.fstep = c(1, 12), history.estimates = 'fcst', 
#'         history.save = 'fcsterrors')
#' plot_fcst_history(air_seas_mdl, air_seas_mdl2, 
#'      start_hist = 1957.0,
#'      main_title = 'Differences in the SS Fcst Error for Airline Passengers',
#'      name_mdl1 = 'Airline model', 
#'      name_mdl2 = 'Airline model + regressors')
#' @import graphics
#' @import stats
#' @import utils
#' @importFrom Rdpack reprompt
#' @export
plot_fcst_history <- 
	function(seas_mdl1 = NULL, 
	    seas_mdl2 = NULL, 
		start_hist = NULL, 
		main_title = "Differences in the Sum of Squared Forecast Errors", 
		name_mdl1 = "Model 1", 
		name_mdl2 = "Model 2", 
		do_grid = FALSE, 
		do_frame = TRUE,
		this_col = NULL,
		this_plot_cex = 0.8, 
		this_lab_cex = NULL, 
		this_main_cex = NULL,
		this_sub_cex = NULL, 
		this_axis_cex = NULL, 
		this_mar = c(5.1, 2.1, 5.1, 0.5),
		main_title_line = 2.5, 
		sub_title_line = 1, 
		display_color_strip = TRUE, 
		x_axis_label = NULL, 
		y_axis_label = NULL, 
		x_axis_main_ticks = NULL,
		y_axis_main_ticks = NULL,
		this_reset = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 1.3, 2026-05-18
    
    # check if starting date of history analysis is specified if no starting date, return NULL
    if (is.null(start_hist)) {
        cat("must specify starting date for history analysis")
        return(NULL)
    }
    
    # check if seas objects for both models are specified if not, return NULL
    if (is.null(seas_mdl1)) {
        cat("must specify seas objects for both models")
        return(NULL)
    } else {
        if (is.null(seas_mdl2)) {
            cat("must specify seas objects for both models")
            return(NULL)
        }
    }

    # check if seas objects for both models are actually seas objects if not, return NULL
    if (!inherits(seas_mdl1, "seas")) {
        cat("must specify seas objects for both models")
        return(NULL)
    } else {
        if (!inherits(seas_mdl2, "seas")) {
            cat("must specify seas objects for both models")
            return(NULL)
        }
    }
	
    if (this_reset) { 
	    old_par <- par(no.readonly = TRUE) 
    }
    
    # set cex values if not set by user
    if (is.null(this_lab_cex)) {
        this_lab_cex <- this_plot_cex
    }
    if (is.null(this_axis_cex)) {
        this_axis_cex <- this_plot_cex
    }
    if (is.null(this_main_cex)) {
        this_main_cex <- this_plot_cex + 0.1
    }
    if (is.null(this_sub_cex)) {
        this_sub_cex <- this_plot_cex
    }

    par(mar = this_mar, cex = this_plot_cex, cex.axis = this_axis_cex, cex.lab = this_lab_cex)
    
    # extract udg information fr\026om seas objects
    udg_mdl1 <- seasonal::udg(seas_mdl1)
    udg_mdl2 <- seasonal::udg(seas_mdl2)
    
    # extract fstep information from udg lists
    fstep_mdl1 <- udg_mdl1$rvfcstlag
    fstep_mdl2 <- udg_mdl2$rvfcstlag
    
    # extract squared forecast errors from seas objects
    fcst_error_mdl1 <- seasonal::series(seas_mdl1, "history.fcsterrors")
    fcst_error_mdl2 <- seasonal::series(seas_mdl2, "history.fcsterrors")
    
    if (is.matrix(fcst_error_mdl1)) {
        # check if the number of forecast leads being tested are the same for the two models if number of
        # leads unequal, return NULL
        if (ncol(fcst_error_mdl1) != ncol(fcst_error_mdl2)) {
            cat("number of forecast lags tested must be equal")
            return(NULL)
        }
        
        # check if the forecast leads being tested are the same for the two models if forecast leads
        # tested not the same, return NULL
        nstep <- ncol(fcst_error_mdl1)
        this_nrows <- nrow(fcst_error_mdl1)
    } else {
        if (is.matrix(fcst_error_mdl1)) {
            cat("number of forecast lags tested must be equal")
            return(NULL)
        }
        nstep <- 1
        this_nrows <- length(fcst_error_mdl1)
    }
    nsum <- sum(fstep_mdl1 == fstep_mdl2)
    if (nstep > nsum) {
        cat("forecast lags tested for the two models need to be the same")
        return(NULL)
    }
	
	if (is.null(this_col)) {
		this_col <- 
			c(paletteer::paletteer_d("rcartocolor::Bold")[1:nstep], "lightgrey")
	}
    
    # set number of rows, frequency for time series tested
    this_freq <- udg_mdl1$freq
    
    # initialize list for difference in squared forecast error, column names
    fcst_err_list <- list()
    this_key <- paste0("lead ", fstep_mdl1)
    
    # generate x axis label
	if (is.null(x_axis_label)) {
		x_axis_label <- 
			stringr::str_to_title(paste0(blstinyplot::display_color(this_col[1], display_color_strip), 
				" = ", this_key[1]))
		if (nstep > 1) {
			for (j in 2:nstep) {
				x_axis_label <- 
					stringr::str_to_title(paste0(x_axis_label, ", ", 
						display_color(this_col[j], display_color_strip), 
						" = ", this_key[j]))
			}
		} 
	}
    
    # convert start with 2 elements to a scalar
    if (length(start_hist) == 2) {
        this_start_hist <- start_hist[1] + ((start_hist[2] - 1)/this_freq)
    } else {
        this_start_hist <- start_hist
    }
    
    # generate cumulative difference in squared forecast error, store as a time series object in the
    # list
    if (nstep > 1) {
        for (i in 1:nstep) {
            row_filter <- seq(fstep_mdl1[i], this_nrows)
            diff_fcst_error <- fcst_error_mdl1[row_filter, i] - fcst_error_mdl2[row_filter, i]
            this_start <- this_start_hist + ((fstep_mdl1[i] - 1)/this_freq)
            fcst_err_list[[this_key[i]]] <- ts(diff_fcst_error, start = this_start, 
                                               frequency = this_freq)
        }
    } else {
        diff_fcst_error <- fcst_error_mdl1 - fcst_error_mdl2
        this_start <- this_start_hist + ((fstep_mdl1 - 1)/this_freq)
        fcst_err_list[[this_key]] <- ts(diff_fcst_error, start = this_start, 
                                        frequency = this_freq)
    }
       
    # generate y-axis limit for main plot
    diff_range <- range(unlist(fcst_err_list), 0)
    
    # generate data frame with forecast errors
    fcst_error_df <- data.frame(
      date  = time(fcst_err_list[[1]]),
      fcst1 = as.double(fcst_err_list[[1]]),
	  zero = rep(0.0, length(fcst_err_list[[1]]))
    )
    
    if (nstep == 2) { 
		  numNA <- fstep_mdl1[2] - fstep_mdl1[1]
		  fcst_error_df$fcst2 <- 
			  c(rep(NA, numNA), as.double(fcst_err_list[[2]]))
	  }
    
    if (nstep == 3) { 
      numNA <- fstep_mdl1[3] - fstep_mdl1[1]
      fcst_error_df$fcst3 <- 
        c(rep(NA, numNA), as.double(fcst_err_list[[3]]))
    }
    
    if (nstep == 4) { 
      numNA <- fstep_mdl1[4] - fstep_mdl1[1]
      fcst_error_df$fcst4 <- 
        c(rep(NA, numNA), as.double(fcst_err_list[[4]]))
    }
	
    tinyplot::plt(fcst1 ~ date, data = fcst_error_df,
                  type = "n",
                  ylab = "",
                  xlab = "",
                  ylim = diff_range,
                  grid = do_grid,
                  frame = TRUE,
                  xaxt = "n",
                  yaxt = "n") 

    tinyplot::plt(fcst1 ~ date, data = fcst_error_df,
                  type = "l",
	              lty = 1, 
                  col = this_col[1],
                  lwd = 1,
                  add = TRUE) 
    
    if (nstep == 2) { 
		tinyplot::plt(fcst2 ~ date, data = fcst_error_df,
                      type = "l",
	                  lty = 1, 
                      col = this_col[2],
                      lwd = 1,
                      add = TRUE) 
	  }
    
    if (nstep == 3) { 
		tinyplot::plt(fcst3 ~ date, data = fcst_error_df,
                      type = "l",
	                  lty = 1, 
                      col = this_col[3],
                      lwd = 1,
                      add = TRUE) 
   }
    
    if (nstep == 4) { 
		tinyplot::plt(fcst4 ~ date, data = fcst_error_df,
                      type = "l",
	                  lty = 1, 
                      col = this_col[4],
                      lwd = 1,
                      add = TRUE) 
    }

	tinyplot::plt(zero ~ date, data = fcst_error_df,
                  type = "l",
	              lty = 1, 
                  col = this_col[nstep + 1],
                  lwd = 1,
                  add = TRUE) 
	
    if (is.null(x_axis_main_ticks)) {
		axis(1, cex.axis = this_axis_cex)
    } else {
		axis(1, at = x_axis_main_ticks, 
			 cex.axis = this_axis_cex)
    }
    if (!is.null(x_axis_label)) {
		mtext(x_axis_label, 1, 2.5, cex = this_lab_cex) 
    }

	# Generate Y-axis
	if (is.null(y_axis_main_ticks)) {
		axis(2, cex.axis = this_axis_cex)
	} else {
		axis(2, at = y_axis_main_ticks, 
			 cex.axis = this_axis_cex)
	}
		
    if (!is.null(y_axis_label)) {
		  mtext(y_axis_label, 2, 2, cex = this_lab_cex)
    }
		
    # generate main title and sub title
    if (!is.null(main_title)) {
        mtext(main_title, 3, main_title_line, cex = this_main_cex)
    }
    if (!is.null(name_mdl1)) {
        if (!is.null(name_mdl2)) {
            sub_title <- paste0(name_mdl1, " vs ", name_mdl2)
            if (is.null(main_title)) {
                mtext(sub_title, 1, sub_title_line + 1, 
                      cex = this_sub_cex)
            } else {
                mtext(sub_title, 3, sub_title_line, 
                      cex = this_sub_cex)
            }
        }
    }
    
    # reset graphics settings
    if (this_reset) { reset_par(old_par) }
}
