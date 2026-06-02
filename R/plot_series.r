#' Plot individual series.
#'
#' Generate plot of user-specified series.
#'
#' Version 2.0, 2026-04-08
#'
#' @param this_series Numeric vector; time series object to be plotted.
#'        This is a required entry.
#' @param main_title Character string; main title of plot.  
#'        Default is no title.
#' @param main_title_line Integer scalar; position of main title of plot.  
#'        Default is \code{2.75}.
#' @param main_title_cex Numeric scalar; scaling for main title of plot. 
#'        Default is \code{1.25}.
#' @param x_axis_label Character string; x-axis label for plot, if specified.
#' @param y_axis_label Character string; y-axis label for plot, if specified.
#' @param x_axis_main_ticks Numeric vector; X-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param y_axis_main_ticks Numeric vector; Y-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param y_limit Numeric vector of length 2; Range of values you wish 
#'        the plot to be plotted over. 
#'        Default is range of \code{this_series}.
#' @param start_plot Integer vector of length 2; Starting date for plot. 
#'        Default is starting date for the time series.
#' @param do_grid Logical scalar; indicates if certain plots will have 
#'        grid lines. Default is no grid lines.
#' @param draw_recess Logical scalar; indicates if certain plots will 
#'        have shaded areas for NBER recession dates \insertCite{nber}{blstinyplot}. 
#'        Default is no recession shading.
#' @param recess_start numeric matrix; Rows of dates for additional 
#'        recession starting and ending dates. 
#'        Default is not to add recession dates.
#' @param recess_col Character string; color used for shading of 
#'        recession region. Default is \code{"lightgrey"}.
#' @param recess_sub Logical scalar; indicates if x-axis label for 
#'        recession is produced for this plot. 
#'        Default is x-axis label is produced
#' @param this_trans Logical scalar; indicates if the adjustment was done 
#'        with a log transform. Default is TRUE.
#' @param use_ratio Logical scalar; indicates if plots of seasonal factors, 
#'        irregular, and residuals are done as ratio plots. 
#'        Default has these plots as time series line plots.
#' @param this_col Character string; color used for series. 
#'        Default is \code{'grey'}.
#' @param this_plot_type Character string; type of plot produced.
#'        Typical values are \code{"l"} (lines only), \code{"p"} (points only), 
#'        \code{"b"} (both points and lines), \code{"o"} (overplotted points 
#'        and lines), \code{"n"} (no plotting). 
#'        Default is "l".
#' @param this_line_type Integer scalar; line type used for series.
#'        Default is \code{1}.
#' @param this_line_width Integer scalar; line width used for series.
#'        Default is \code{1}.
#' @param this_point_type Integer scalar; point type used for series. 
#'        Default is no points plotted.
#' @param this_plot_cex Numeric scalar; scaling for the plot itself. 
#'        Default is \code{0.8}.
#' @param this_lab_cex Numeric scalar; scaling for plot labels. 
#'        Default is the value of \code{this_plot_cex}.
#' @param this_axis_cex Numeric scalar; scaling for plot axis. 
#'        Default is the value of \code{this_plot_cex}.
#' @param this_mar Numeric vector; set margins for the plot. 
#'        Default is \code{c(4,4,4,0,5)}.
#' @param this_reset Logical scalar; if TRUE, the values of \code{par} are reset. 
#'        Default is \code{TRUE}.
#' @return Generate plot of user-specified series. Can be first in a series 
#'         of plots, with other lines or points added after calling this routine. 
#'         If series not specified, print out error message and return NULL.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas <- 
#'     seasonal::seas(AirPassengers, 
#'         arima.model = "(0 1 1)(0 1 1)",
#'         outlier.types = "all", 
#'         x11 = "",
#'         forecast.maxlead = 36)
#' air_sa <- seasonal::final(air_seas)
#' plot_series(air_sa, 
#'      y_axis_label = "AirPass", 
#'      do_grid = TRUE,
#'      draw_recess = TRUE, 
#'      this_col = "black",
#'      start_plot = c(1958,1), 
#'      this_point_type = 1,
#'      main_title = "X-11 Seasonal Adjustment for Airline Passengers",
#'      main_title_line = 1.5,
#'      main_title_cex = 1.0, 
#'      this_reset = TRUE)
#' @import graphics
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
plot_series <- 
    function(this_series = NULL, 
             main_title = NULL, 
             main_title_line = 2.75, 
             main_title_cex = 1.25,
             x_axis_label = NULL, 
             y_axis_label = NULL, 
             x_axis_main_ticks = NULL,
             y_axis_main_ticks = NULL,
             y_limit = NULL, 
             start_plot = NULL, 
             do_grid = FALSE,
             draw_recess = FALSE, 
             recess_start = NULL, 
             recess_col = "lightgrey", 
             recess_sub = TRUE,
             this_trans = TRUE, 
             use_ratio = FALSE, 
             this_col = "grey", 
             this_plot_type = "l",
             this_line_type = 1,
             this_line_width = 1,
             this_point_type = NULL, 
             this_plot_cex = 0.8, 
             this_lab_cex = NULL,
             this_axis_cex = NULL, 
             this_mar = c(4,4,4,0.5), 
             this_reset = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 2.0, 2026-04-08

    if (is.null(this_series)) {
        stop("Argument this_series must be specified.")
    }
	
    if (this_reset) { 
	    old_par <- par(no.readonly = TRUE) 
    }
      
    # If start_plot specified, shorten series
    if (!is.null(start_plot)) {
        this_series <- window(this_series, start = start_plot)
    }

    if (is.null(y_limit)) {
        y_limit <- range(this_series)
    }

  # set cex values if not set by user
    if (is.null(this_lab_cex)) {
        this_lab_cex <- this_plot_cex
    }
    if (is.null(this_axis_cex)) {
        this_axis_cex <- this_plot_cex
    }

  # reset par based on user input.
    par(mar = this_mar, cex = this_plot_cex, cex.axis = this_axis_cex,
        cex.lab = this_lab_cex)

# Check to see if this is a ratio plot.
    if (use_ratio) {
        if (this_trans) {
            h_bar <- 1
        } else {
            h_bar <- 0
        }

        # Generate ratio plot.
        plot_ratio(ratio_series = this_series,
             ratio_range = y_limit,
             ratio_color = this_col, 
             main_title = main_title, 
             main_title_line = main_title_line, 
             main_title_cex = main_title_cex,
             x_axis_main_ticks = x_axis_main_ticks,
             y_axis_main_ticks = y_axis_main_ticks,
             y_axis_label = y_axis_label,
             this_plot_cex = this_plot_cex, 
             this_lab_cex = this_lab_cex,
             this_axis_cex = this_axis_cex, 
             this_mar = this_mar, 
             ratio_mean_color = this_col,
             ratio_mean = h_bar)

    } else {
		# create data frame for Plot
      this_df <- 
        data.frame(date = time(this_series), 
                   value = as.double(this_series))

# Generate plot with title.
      if (is.null(this_point_type)) { 
        tinyplot::plt(this_df$date, this_df$value, 
                      type = this_plot_type,
                      lty = this_line_type, 
                      col = this_col,
                      lwd = this_line_width,
                      ylab = "",
                      xlab = "",
                      xaxt = "n",
                      yaxt = "n") 
      } else {
        tinyplot::plt(this_df$date, this_df$value, 
                      type = this_plot_type, 
                      lty = this_line_type,  
                      pch = this_point_type, 
                      lwd = this_line_width, 
                      col = this_col,
                      ylab = "", 
                      xlab = "", 
                      xaxt = "n", 
                      yaxt = "n") 
      }		
		# Generate X-axis
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
			mtext(main_title, 3, main_title_line, cex = main_title_cex)
		}
		
		# add y-axis label, if specified.
		if (!is.null(y_axis_label)) {
			mtext(y_axis_label, 2, 2.5)
		}
	}

    # add grid.
    if (do_grid) {
        grid()
    }

    # add shaded regions for recessions.
    if (draw_recess) {
        draw_recession(recess_col, this_add_recess_start = recess_start,
                       this_sub_recess = recess_sub)
    }

    if (this_reset) { reset_par(old_par) }

}
