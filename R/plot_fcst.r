#' Forecast plot
#'
#' Generates regARIMA forecasts with confidence bounds
#'
#' Version 1.5, 2026-05-15
#'
#' @param seas_obj A seas object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{blstinyplot}.
#'        This is a required entry.
#' @param main_title Character string; main title of plot.  
#'        Default is  \code{'ARIMA Residuals'}.
#' @param do_sub Logical scalar; indicates if subtitle is generated. 
#'        Default is to generate the subtitle. 
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param do_frame Logical scalar; indicates the plot will have a frame. 
#'        Default is the plot frame will be produced. 
#' @param length_ori Integer scalar; number of years of the original series to show 
#'        with forecasts. Default is 2 years. 
#' @param this_col Array of character strings; color used for original series, 
#'        forecast, and forecast bounds. 
#'        Default is \code{c("darkgrey", "blue", "darkgreen")}. 
#' @param this_plot_cex Numeric scalar; scaling for the plot itself. 
#'        Default is \code{0.8}.
#' @param this_lab_cex Numeric scalar; scaling for plot labels. 
#'        Default is the value of \code{this_plot_cex}.
#' @param this_main_cex Numeric scalar; scaling for main plot title. 
#'        Default is the value of \code{1.0}.
#' @param this_sub_cex Numeric scalar; scaling for plot subtitle. 
#'        Default is the value of \code{0.7}.
#' @param this_axis_cex Numeric scalar; scaling for plot axis. 
#'        Default is the value of \code{this_plot_cex}.
#' @param x_axis_label Character string; x-axis label for plot, if specified.
#' @param y_axis_label Character string; y-axis label for plot, if specified.
#' @param x_axis_main_ticks Numeric vector; X-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param y_axis_main_ticks Numeric vector; Y-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param main_title_line Integer scalar; position of main title of plot.  
#'        Default is \code{2.25}.
#' @param sub_title_line Integer scalar; position of main title of plot.  
#'        Default is \code{0.75}.
#' @param this_reset Logical scalar; if TRUE, the values of \code{par} are reset. 
#'        Default is \code{TRUE}.
#' @return Generates a plot of the regARIMA forecasts with confidence bounds.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, 
#'                   arima.model = '(0 1 1)(0 1 1)', 
#'                   forecast.maxlead = 60, 
#'                   forecast.save = "fct", 
#'                   series.save = "a1")
#' plot_fcst(air_seas, 
#'           main_title = 'Forecasts for Airline Passengers', 
#'           do_grid = TRUE)
#' @import graphics
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
plot_fcst <- 
    function(seas_obj = NULL, 
	         main_title = "ARIMA forecasts", 
			 do_sub = TRUE, 
			 do_grid = FALSE, 
			 do_frame = TRUE, 
			 length_ori = 2, 
             this_col = c("darkgrey", "blue", "darkgreen"), 
			 this_plot_cex = 0.8, 
             this_lab_cex = NULL, 
			 this_main_cex = 1.0, 
			 this_sub_cex = 0.7,
             this_axis_cex = NULL, 
			 x_axis_label = "Time",
			 y_axis_label = NULL,
			 x_axis_main_ticks = NULL,
			 y_axis_main_ticks = NULL,
			 main_title_line = 2.25,
             sub_title_line = 0.75,			 
			 this_reset = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 1.5, 2026-05-15
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
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
        this_main_cex <- this_plot_cex
    }
    if (is.null(this_sub_cex)) {
        this_sub_cex <- this_plot_cex
    }
    
    if (is.null(main_title)) {
        do_sub <- FALSE
    }

    par(cex = this_plot_cex, cex.axis = this_axis_cex, cex.lab = this_lab_cex,
        cex.main = this_main_cex)
        
    # extract forecasts, original series
    fcst <- seasonal::series(seas_obj, "fct")
    a1 <- seasonal::series(seas_obj, "a1")
    
    # get date for end of series
    end_a1 <- end(a1)
    
    # get series to be plotted without forecasts
    srs <- window(a1, start = c(end_a1[1] - length_ori, 1))
    # get series to be pottted with forecasts
    ext <- ts(c(srs, fcst[, 1]), start = start(srs), frequency = frequency(srs))
	
	len_srs  <- length(srs)
	len_fcst <- length(fcst[, 1])
	
	fcst_df <- 
		data.frame(date = time(ext),
			ext = as.double(ext),
			ori = c(as.double(srs), rep(NA, len_fcst)),
			fcst = c(rep(NA, len_srs), as.double(fcst[, 1])),
			lowerci  = c(rep(NA, len_srs), as.double(fcst[, 2])),
			upperci = c(rep(NA, len_srs), as.double(fcst[, 3]))) 
 
	this_y_range <- range(ext, as.double(fcst[, 2]), as.double(fcst[,3]))
	
    tinyplot::plt(ext ~ date, data = fcst_df,
                  type = "n",
                  ylab = "",
                  xlab = "",
                  ylim = this_y_range,
                  grid = do_grid,
                  frame = do_frame,
                  xaxt = "n",
                  yaxt = "n") 
					  
    tinyplot::plt(ori ~ date, data = fcst_df,
                  type = "l",
	              lty = 1, 
                  col = this_col[1],
                  lwd = 1,
                  add = TRUE) 

    tinyplot::plt(fcst ~ date, data = fcst_df,
                  type = "l",
	              lty = 1, 
                  col = this_col[2],
                  lwd = 1,
                  add = TRUE) 
					  
    tinyplot::plt(lowerci ~ date, data = fcst_df,
                  type = "l",
	              lty = 2, 
                  col = this_col[3],
                  lwd = 1,
                  add = TRUE) 
					  
    tinyplot::plt(upperci ~ date, data = fcst_df,
                  type = "l",
	              lty = 2, 
                  col = this_col[3],
                  lwd = 1,
                  add = TRUE) 
					  
    if (is.null(x_axis_main_ticks)) {
		axis(1, cex.axis = this_axis_cex)
    } else {
		axis(1, at = x_axis_main_ticks, 
			 cex.axis = this_axis_cex)
    }
    if (!is.null(x_axis_label)) {
		mtext(x_axis_label, 1, 2, cex = this_lab_cex) 
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
		
	# add main title, if specified.
	if (!is.null(main_title)) {
		mtext(main_title, 3, main_title_line, cex = this_main_cex)
	}
	
	# add y-axis label, if specified.
	if (!is.null(y_axis_label)) {
		mtext(y_axis_label, 2, 2.5)
	}

    # generate subtitile
    if (do_sub) {
        aape <- tryCatch(seasonal::udg(seas_obj, "aape.0"), error = function(e) {
            NULL
        })
        if (!is.null(aape)) {
            mtext(paste("AAPE Last 3 Years = ", format(aape, digits = 4, nsmall = 2), sep = ""), 3, 
                		sub_title_line = 1, cex = this_sub_cex)
        }
    }
    
    if (this_reset) { reset_par(old_par) }
}
