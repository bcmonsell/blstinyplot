#' Residual plot
#'
#' Generates a plot of the regARIMA residuals with diagnostic information
#'
#' Version 1.4, 2026-05-21
#'
#' @param seas_obj A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{blstinyplot}.
#'        This is a required entry.
#' @param main_title Character string; main title of plot.  
#'        Default is  \code{'ARIMA Residuals'}.
#' @param main_title_line Integer scalar; position of main title of plot.  
#'        Default is \code{2.75}.
#' @param main_title_cex Numeric scalar; scaling for main title of plot. 
#'        Default is \code{1.25}.
#' @param series_name Character scalar; name of the time series used in 
#'        \code{seas_obj}. Used only if specified.
#' @param x_axis_label Character string; x-axis label for plot, if specified.
#' @param y_axis_label Character string; y-axis label for plot, if specified.
#' @param x_axis_main_ticks Numeric vector; X-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param y_axis_main_ticks Numeric vector; Y-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is grid lines plotted.
#' @param draw_recess Logical scalar; indicates if certain plots will  
#'        have shaded areas for NBER recession dates \insertCite{nber}{blstinyplot}. 
#'        Default is recession shading not plotted.
#' @param recess_start numeric matrix; Rows of dates for additional recession 
#'        starting and ending dates. 
#'        Default is not to add recession dates.
#' @param recess_col Character string; color used for shading of recession region. 
#'        Default is \code{'lightgrey'}.
#' @param recess_sub Logical scalar; indicates if x-axis label for recession is 
#'        produced for this plot.  
#'        Default is x-axis label is produced
#' @param this_trans Logical scalar; indicates if the adjustment was done 
#'        with a log transform. Default is TRUE.
#' @param use_ratio Logical scalar; indicates if plots of seasonal factors,  
#'        irregular, and residuals are done as ratio plots.  
#'        Default has these plots as time series line plots.
#' @param this_col Character string; color used for residuals.  
#'        Default is \code{'green'}.
#' @param this_plot_cex Numeric scalar; scaling for the plot itself.  
#'        Default is \code{0.8}.
#' @param this_lab_cex Numeric scalar; scaling for plot labels.  
#'        Default is the value of \code{this_plot_cex}.
#' @param this_axis_cex Numeric scalar; scaling for plot axis.  
#'        Default is the value of \code{this_plot_cex}.
#' @param this_sub_cex Numeric scalar; scaling for plot labels.  
#'        Default is the value of \code{0.5}.
#' @param this_mar Numeric vector; set margins for the plot.  
#'        Default is \code{c(4,4,4,0,5)}.
#' @param sub_title_one_line Integer scalar; position of furst subtitle of plot.  
#'        Default is \code{1.5}.
#' @param sub_title_two_line Integer scalar; position of main title of plot.  
#'        Default is \code{0.5}.
#' @param this_reset Logical scalar; if TRUE, the values of \code{par} are reset.  
#'        Default is \code{TRUE}.
#' @return Generates a plot of the regARIMA residuals with diagnostic information 
#'         in the sub-headers.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas <- 
#'    seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)')
#' plot_resid(air_seas, 
#'            main_title = 'ARIMA Residuals for Airline Passengers', 
#'            use_ratio = TRUE, this_col='darkblue')
#' @import graphics
#' @import utils
#' @importFrom Rdpack reprompt
#' @export
plot_resid <- 
	function(seas_obj = NULL, 
        main_title = "ARIMA Residuals", 
        main_title_line = 2.75,
        main_title_cex = 1.0,
        series_name = NULL, 
        x_axis_label = NULL, 
        y_axis_label = NULL, 
        x_axis_main_ticks = NULL,
        y_axis_main_ticks = NULL,
        do_grid = TRUE,
        draw_recess = FALSE, 
        recess_start = NULL, 
        recess_col = NULL, 
        recess_sub = TRUE, 
        this_trans = TRUE, 
        use_ratio = FALSE, 
        this_col = "green", 
        this_plot_cex = 0.8, 
        this_lab_cex = NULL,
        this_axis_cex = NULL, 
        this_sub_cex = 0.5, 
        this_mar = c(4, 4, 4, 0.5), 
        sub_title_one_line = 1.5,
        sub_title_two_line = 0.5,
        this_reset = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 1.4, 2026-05-21

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
    if (is.null(main_title_cex)) {
        main_title_cex <- this_plot_cex + 0.1
    }

   # reset par based on user input.
    par(mar = this_mar, cex = this_plot_cex, cex.axis = this_axis_cex,
        cex.lab = this_lab_cex)
        
    # Extract regARIMA residuals
    resid <- seasonal::series(seas_obj, "rsd")

    # Generate main plot title
    if (!is.null(series_name)) {
        if (!is.null(main_title)) {
            main_title <- paste(main_title, " for ", series_name)
        }
    }

    # Generate ratio plot of residuals
    if (use_ratio) {
	    if(sum(resid < 0) > 0) {
			this_mean <- 0.0
		} else {
			this_mean <- 1.0
		}
		
        plot_ratio(ratio_series = resid, 
	         ratio_range = range(resid), 
	         main_title = main_title,  
	         main_title_line = main_title_line,  
	         main_title_cex = main_title_cex, 
	         ratio_mean = this_mean,  
	         ratio_color = this_col,  
	         ratio_mean_color = "grey",
	         x_axis_main_ticks = x_axis_main_ticks,
	         y_axis_main_ticks = y_axis_main_ticks,
			 x_axis_label = "Time",
			 y_axis_label = x_axis_label,
	         this_plot_cex = this_plot_cex,  
	         this_lab_cex = this_lab_cex, 
	         this_axis_cex = this_axis_cex,  
	         this_mar = this_mar,  
	         this_reset = FALSE)
    } else {
        # Generate line plot of residuals
        plot_series(this_series = resid, 
             main_title = main_title, 
             main_title_line = main_title_line, 
             main_title_cex = main_title_cex,
             x_axis_label = x_axis_label, 
             y_axis_label = y_axis_label, 
             x_axis_main_ticks = x_axis_main_ticks,
             y_axis_main_ticks = y_axis_main_ticks,
             y_limit = range(resid), 
             do_grid = do_grid,
             draw_recess = draw_recess, 
             recess_start = recess_start, 
             recess_col = recess_col, 
             this_trans = this_trans, 
             use_ratio = use_ratio, 
             this_col = this_col, 
             this_plot_cex = this_plot_cex, 
             this_lab_cex = this_lab_cex,
             this_axis_cex = this_axis_cex, 
             this_mar = this_mar, 
             this_reset = FALSE)
    }

    # Generate grid
    if (do_grid) {
        grid()
    }

    # Generate recession region shading
    if (draw_recess) {
        draw_recession(recess_col, this_add_recess_start = recess_start, 
                       this_sub_recess = recess_sub)
    }

    # get residual diagnostics
    thisA <- tryCatch(seasonal::udg(seas_obj, "a"), error = function(e) {
        print("Geary''s a not found")
        NULL
    })

    thisKurt <- tryCatch(seasonal::udg(seas_obj, "kurtosis"), error = function(e) {
        print("kurtosis not found")
        NULL
    })

    thisSkew <- tryCatch(seasonal::udg(seas_obj, "skewness"), error = function(e) {
        print("skewness not found")
        NULL
    })

    # generate subheaders for diagnostics
    sub1 <- NULL
    sub2 <- NULL
    sub3 <- NULL

    if (!is.null(thisA)) {
        if (length(thisA) > 1) {
            sub1 <- paste("Geary's a = ", sprintf("%4.1f", as.numeric(thisA[1])), " (", thisA[2],
                ")", sep = "")
        } else {
            sub1 <- paste("Geary's a = ", sprintf("%4.1f", thisA), sep = "")
        }
    }

    if (!is.null(thisKurt)) {
        if (length(thisKurt) > 1) {
            sub2 <- paste("Excess Kurtosis = ", sprintf("%4.1f", as.numeric(thisKurt[1])), " (", thisKurt[2],
                ")", sep = "")
        } else {
            sub2 <- paste("Excess Kurtosis = ", sprintf("%4.1f", thisKurt), sep = "")
        }
    }

    if (!is.null(thisSkew)) {
        if (length(thisSkew) > 1) {
            sub3 <- paste("Skewness = ", sprintf("%4.1f", as.numeric(thisSkew[1])), " (", thisSkew[2],
                ")", sep = "")
        } else {
            sub3 <- paste("Skewness = ", sprintf("%4.1f", thisSkew), sep = "")
        }
    }

    if (is.null(main_title)) {
        this_sub <- ""
        if (!is.null(sub1)) {
            this_sub <- sub1
        }
        if (!is.null(sub2)) {
            if (is.null(sub1)) {
                this_sub <- sub2
            } else {
                this_sub <- paste0(this_sub, ", ", sub2)
            }
        }
        if (length(this_sub) > 0) {
            if (!is.null(sub3)) {
                mtext(paste0(this_sub, ", ", sub3), 3, 
				      sub_title_one_line, cex = this_sub_cex)
            } else {
                mtext(this_sub, 3, 
				      sub_title_one_line, cex = this_sub_cex)
            }
        }
    } else {
        if (!is.null(sub1)) {
            mtext(sub1, 3, sub_title_one_line, cex = this_sub_cex)
        }
        if (!is.null(sub2)) {
            if (!is.null(sub3)) {
                mtext(paste(sub2, ", ", sub3, sep = ""), 3, 
				      sub_title_two_line, cex = this_sub_cex)
            } else {
                mtext(sub2, 3, sub_title_two_line, cex = this_sub_cex)
            }
        } else {
            if (!is.null(sub3)) {
                mtext(sub3, 3, sub_title_two_line, cex = this_sub_cex)
            }
        }
    }
    

    # restore graphics parameters
    if (this_reset) { reset_par(old_par) }

}
