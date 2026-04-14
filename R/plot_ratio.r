#' Ratio plot
#'
#' Generates a high-definition plot around a reference line other than zero.
#'
#' Version 1.5, 2026-04-02
#'
#' @param ratio_series Time series of ratios/factors for which you want to 
#'        generate a high definition plot.
#'        This is a required entry.
#' @param ratio_range Range of values you wish the plot to be plotted over. 
#'        Default is range of the series.
#' @param main_title Title for the plot. 
#'        Default is character string \code{'Ratio Plot'}.
#' @param main_title_line Integer scalar; position of main title of plot.  
#'        Default is \code{2}.
#' @param main_title_cex Numeric scalar; scaling for main title of plot. 
#'        Default is \code{this_plot_cex + 0.25}.
#' @param ratio_mean Assumed mean value for the ratio.  
#'        Default is \code{1.0}
#' @param ratio_color Color used for lines in ratio plot.  
#'        Default is \code{'black'}.
#' @param ratio_mean_color Color used for mean line in ratio plot.  
#'        Default is \code{'black'}.
#' @param x_axis_main_ticks Numeric vector; X-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param y_axis_main_ticks Numeric vector; Y-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param x_axis_label Character string; x-axis label for plot.
#'        Default is \code{'Time'}.
#' @param y_axis_label Character string; y-axis label for plot, if specified.
#' @param this_plot_cex Numeric scalar; scaling for the plot itself.   
#'        Default is \code{0.8}.
#' @param this_lab_cex Numeric scalar; scaling for plot labels.   
#'        Default is the value of \code{this_plot_cex}.
#' @param this_axis_cex Numeric scalar; scaling for plot axis.   
#'        Default is the value of \code{this_plot_cex}.
#' @param this_mar Numeric vector; set margins for the plot.   
#'        Default is \code{c(4,4,4,0.5)}.
#' @param this_reset Logical scalar; if TRUE, the values of \code{par} are reset.   
#'        Default is \code{TRUE}.
#' @return Generates a high definition plot of rations centered on one,   
#'         by default.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' air_seas <-   
#'     seasonal::seas(AirPassengers,  
#'         transform.function= 'log', 
#'         arima.model = '(0 1 1)(0 1 1)',   
#'         seats.save = 's10')
#' air_sf <- seasonal::series(air_seas, 's10')
#' plot_ratio(air_sf,  
#'      main_title = 'SEATS seasonal for Airline Passenger',  
#'      ratio_color = 'darkblue')
#' @import graphics
#' @import stats
#' @export
plot_ratio <- 
    function(ratio_series = NULL, 
	         ratio_range = range(ratio_series), 
	         main_title = "Ratio Plot",  
	         main_title_line = 2,  
	         main_title_cex = NULL, 
	         ratio_mean = 1.0,  
	         ratio_color = "black",  
	         ratio_mean_color = "black",
	         x_axis_main_ticks = NULL,
	         y_axis_main_ticks = NULL,
			 x_axis_label = "Time",
			 y_axis_label = NULL,
	         this_plot_cex = 0.8,  
	         this_lab_cex = NULL, 
	         this_axis_cex = NULL,  
	         this_mar = c(4,4,4,0.5),  
	         this_reset = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 1.5, 2026-04-02

    # check if \code{ratio_series} is specified
    if (is.null(ratio_series)) {
        stop("Argument ratio_series must be specified.")
    }
	
	# if \code{this_reset} is TRUE, set \code{old_par}
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
    if (is.null(main_title_cex)) {
        main_title_cex <- this_plot_cex + 0.25
    }

   # reset par based on user input.
    par(mar = this_mar, cex = this_plot_cex, cex.axis = this_axis_cex,
        cex.lab = this_lab_cex)
   
    # generate length of the ratio series, time index
    ratio_length <- length(ratio_series)
    ratio_time <- as.double(time(ratio_series))
	
	# create a matrix with the mean and value input by user
	ratio_matrix <- cbind(ratio_mean, as.double(ratio_series))
	
	# generate max and min of \code{ratio_matrix} 
	y_min <- apply(ratio_matrix, 1, min)
	y_max <- apply(ratio_matrix, 1, max)

    # plot ratio without title, x and y axis
	tinyplot::plt(
        xmin = ratio_time, ymin = y_min, 
        xmax = ratio_time, ymax = y_max,
        type = tinyplot::type_segments(),
        ylab = "",
        xlab = "",
        axes = FALSE,
        frame = TRUE, 
		col = ratio_color,
		ylim = ratio_range
    )

    # add line to plot frame
    abline(h = ratio_mean, col = ratio_mean_color)
    
    # add title
    title(main = main_title, 
	      line = main_title_line,
          cex.main = main_title_cex)
		  
	# add X-axis
    if (is.null(x_axis_main_ticks)) {
        axis(1, cex.axis = this_axis_cex)
	} else {
		axis(1, 
             at = x_axis_main_ticks, 
             cex.axis = this_axis_cex)
	}
    if (!is.null(x_axis_label)) {
		mtext(x_axis_label, 1, 2, cex = this_lab_cex)
	}
	
	# add Y-axis
    if (is.null(y_axis_main_ticks)) {
        axis(2, cex.axis = this_axis_cex)
	} else {
        axis(2,
             at = y_axis_main_ticks, 
		     cex.axis = this_axis_cex)
	}
    if (!is.null(y_axis_label)) {
		mtext(y_axis_label, 2, 2, cex = this_lab_cex)
    }
	
    if (this_reset) { reset_par(old_par) }
}
