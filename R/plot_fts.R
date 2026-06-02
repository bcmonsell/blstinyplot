#' Final t-statistics for the outlier identification procedure plot
#'
#' Generates a plot of the final t-statistics for the outlier identification procedure.
#'
#' Version 2.4, 2026-05-27
#'
#' @param seas_obj A \code{seas} object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{blstinyplot}.
#'        This is a required entry.
#' @param fts time series matrix containing final outlier t-statistics for all types 
#'        of outlier specified by the user.
#'        This is a required entry.
#' @param this_cex Numeric scalar; sets the \code{cex} plotting parameter. 
#'        Default sets \code{cex = 0.5}.
#' @param start_plot Integer vector of length 2; Starting date for plot. 
#'        Default is starting date for the time series. 
#' @param main_title Character string; main title of plot.  
#'        Default is  \code{'Outlier T-Values'}.
#' @param add_identified_otl Logical scalar; indicates if outlier plots will 
#'        include identified outliers. 
#'        Default is not including identified outliers. 
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param color_otl Character array of length 3; color used for different outliers, 
#'        with the order being \code{'ao','ls','tc'}. 
#'        Default is NULL.
#' @param this_palette Character string; default \code{RColorBrewer} palette 
#'        \insertCite{RColorBrewer}{blsplotGG}.
#'        Deault is \code{"Dark2"}.
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
#'        Default is \code{c(4, 4, 4, 0.5)}.
#' @param main_title_line Integer scalar; position of main title of plot.   
#'        Default is \code{2.25}.
#' @param sub_title_line Integer scalar; position of main title of plot.   
#'        Default is \code{1}.
#' @param x_axis_label Character string; x-axis label for plot, if specified.
#' @param y_axis_label Character string; y-axis label for plot, if specified.
#' @param x_axis_main_ticks Numeric vector; X-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param y_axis_main_ticks Numeric vector; Y-axis tick marks.
#'        Default is determined by the \code{axis} function.
#' @param plot_abs Logical scalar; if \code{TRUE}, plot the maximum absolute value 
#'        of the t-stats. If \code{FALSE}, 
#'        maintain the sign of the original t-statistic in the plot.
#'        Default is \code{TRUE}.
#' @param this_reset Logical scalar; if TRUE, the values of \code{par} are reset.  
#'        Default is \code{TRUE}.
#' @return Generates a plot of the final t-statistics from the automatic outlier 
#'         identification procedure.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas_outlier <- 
#'     seasonal::seas(AirPassengers, 
#'         arima.model = '(0 1 1)(0 1 1)', 
#'         outlier.types = 'all',  
#'         outlier.save = "fts")
#' air_fts_good <- seasonal::series(air_seas_outlier, "fts")
#' plot_fts(air_seas_outlier, air_fts_good, 
#'          main_title = 'Outlier T-Values for Airline Passengers')
#' plot_fts(air_seas_outlier, air_fts_good, 
#'          main_title = 'Outlier T-Values for Airline Passengers',
#'          add_identified_outliers = TRUE)
#' plot_fts(air_seas_outlier, air_fts_good, plot_abs = FALSE,
#'          main_title = 'Outlier T-Values for Airline Passengers',
#'          add_identified_outliers = TRUE)
#' @import graphics
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
plot_fts <- 
	function(seas_obj = NULL, 
		fts = NULL, 
		this_cex = 0.5, 
		start_plot = NULL, 
		main_title = "Outlier T-Values", 
		add_identified_otl = FALSE,
        do_grid = FALSE,
	    color_otl = NULL, 
		this_palette = "Dark2", 
		this_plot_cex = 0.8, 
		this_lab_cex = NULL, 
		this_main_cex = NULL,
		this_sub_cex = NULL, 
		this_axis_cex = NULL, 
        this_mar = c(4, 4, 4, 0.5), 
		main_title_line = 2.25, 
		sub_title_line = 1.0, 
		x_axis_label = NULL, 
        y_axis_label = NULL, 
        x_axis_main_ticks = NULL,
        y_axis_main_ticks = NULL,
		plot_abs = TRUE,
        this_reset = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 2.4, 2026-05-27
    
    # check if a value is specified for \code{seas_obj}
    if (is.null(seas_obj)) {
        stop("must specify a seas object")
    } else {
    # check if a seas object is specified for \code{seas_obj}
        if (!inherits(seas_obj, "seas")) {
            stop("First argument must be a seas object")
        }
    }
    
    if (is.null(fts)) {
        stop("must specify an fts matrix")
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
    if (is.null(this_main_cex)) {
        this_main_cex <- this_plot_cex + 0.1
    }
    if (is.null(this_sub_cex)) {
        this_sub_cex <- this_plot_cex
    }
    par(mar = this_mar, cex = this_plot_cex, cex.axis = this_axis_cex, cex.lab = this_lab_cex)
    
    # Initialize t-statistic data, point labels
    this_fts <- fts
    point_lab <- substr(dimnames(this_fts)[[2]], 3, 4)
    
    # include identified outliers in plot
    if (add_identified_otl) {
        this_auto_outlier <- 
			unlist(strsplit(get_auto_outlier_string(seas_obj), " "))
        n_iter <- seasonal::udg(seas_obj, "addoutlier")
        this_t_vec <- array(0, length(this_auto_outlier))
        for (i in 1:n_iter) {
            this_key <- paste("otlitr", i, "+", sep = ".")
            this_oit <- array(seasonal::udg(seas_obj, this_key))
            this_t_vec[!is.na(match(this_auto_outlier, this_oit[1]))] <- as.numeric(this_oit[4])
        }
        for (i in 1:length(this_auto_outlier)) {
            this_out <- this_auto_outlier[i]
            this_type <- substr(this_out, 1, 2)
            this_year <- as.numeric(substr(this_out, 3, 6))
            if (frequency(this_fts) == 12) {
                this_per <- get_month_index(substr(this_out, 8, 10))
                this_date <- this_year + (this_per - 1)/12
            }
            # Include code for other frequencies
            this_col <- seq(1, length(point_lab))[!is.na(match(point_lab, this_type))]
            this_row <- seq(1, length(fts[, 1]))[time(fts) == this_date]
            this_fts[this_row, this_col] <- this_t_vec[i]
        }
    }
    
    # get subset of t-statistics if starting date is specified
    if (!is.null(start_plot)) {
        this_fts <- window(this_fts, start = start_plot)
    }
    
    # get absolute maximum value of t-statistics, Maximum Position of each column of the Matrix
    fts_max <- 
		ts(apply(this_fts, 1, absmax, return_abs = plot_abs), 
		   start = start(this_fts), 
		   frequency = frequency(this_fts))
    fts_index <-
		cbind(time(this_fts), max.col(abs(this_fts), "first"))
    
    # initialize color vector for outlier points
    if (is.null(color_otl)) {
		  color_otl <- RColorBrewer::brewer.pal(3, "Dark2")
	  }
    col_vec <- array(" ", length(point_lab))
    
    # get critical values for outlier identification
    this_crit_vec <- array(0, length(point_lab))
    for (i in 1:length(point_lab)) {
        if (point_lab[i] == "AO") {
            this_crit <- seasonal::udg(seas_obj, "aocrit")
            if (length(this_crit) > 0) {
                this_crit_vec[i] <- as.numeric(this_crit[1])
            }
            col_vec[i] <- color_otl[1]
        }
        if (point_lab[i] == "LS") {
            this_crit <- seasonal::udg(seas_obj, "lscrit")
            if (length(this_crit) > 0) {
                this_crit_vec[i] <- as.numeric(this_crit[1])
            }
            col_vec[i] <- color_otl[2]
        }
        if (point_lab[i] == "TC") {
            this_crit <- seasonal::udg(seas_obj, "tccrit")
            if (length(this_crit) > 0) {
                this_crit_vec[i] <- as.numeric(this_crit[1])
            }
            col_vec[i] <- color_otl[3]
        }
    }
	
	fts_df <- data.frame(
		date = time(fts_max),
		fts  = as.double(fts_max),
		outlier = as.factor(point_lab[fts_index[, 2]])
    )
	
	if (plot_abs) {
		this_lim <- range(abs(fts_max), this_crit_vec)
	} else {
		this_lim <- range(fts_max, this_crit_vec, 0.0-this_crit_vec)
	}
	
    # Generate frame for the t-statistic plot
    tinyplot::plt(fts ~ date | outlier, fts_df,
		type = "p", 
		pch = 21,
		fill = "by",
		col = color_otl,
		grid = do_grid,
		ylab = " ", 
		xlab = " ", 
		ylim = this_lim)

	if (!is.null(main_title)) {
    # Generate plot subheader
        if (length(unique(this_crit_vec)) == 1) {
            this_sub_head <- paste("critical value = ", unique(this_crit_vec), sep = "")
        } else {
            this_sub_head <- paste("critical values = (", paste(this_crit_vec, collapse = " "), ")", sep = " ")
        }
    
        mtext(main_title, 3, main_title_line, cex = this_main_cex)
        mtext(this_sub_head, 3, sub_title_line, cex = this_sub_cex)
    }
    
    # lines for outlier critical values
    if (length(unique(this_crit_vec)) == 1) {
		abline(h = unique(this_crit_vec), col = "grey")
		if (!plot_abs) {
			abline(h = 0.0-unique(this_crit_vec), col = "grey")
		}
    } else {
		abline(h = this_crit_vec, col = col_vec)
		if (!plot_abs) {
			abline(h = 0.0-unique(this_crit_vec), col = col_vec)
		}
    }
	
	if (!plot_abs) { 
		abline(h = 0.0, col = "grey", lty = 2)
	}
    if (this_reset) { reset_par(old_par) }
    
}
