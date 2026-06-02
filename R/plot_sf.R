#' Seasonal factor (and the SI-ratios) plot grouped by month/quarter
#'
#' Generates a special plot of the seasonal factors (and the SI-ratios) grouped by month/quarter
#'
#' Version 1.3.1, 2026-05-14
#'
#' @param seas_obj A seas object for a single series generated from the 
#'        \code{seasonal} package \insertCite{seasonal2018}{blstinyplot}.
#'        This is a required entry.
#' @param this_table Character string; X-13ARIMA-SEATS table name or abbreviation 
#'        (such as \code{d10} or \code{s10}). Valid table names for this argument 
#'        can be found in the documentation for the \code{series}
#'        function of the \code{seasonal} package \insertCite{Sax2018}{blstinyplot}. 
#'        If not a valid table name, the function will print an error message and 
#'        return a \code{NULL}. This entry is required.
#' @param add_si Logical scalar; indicates if seasonal factor plots will include 
#'        the SI ratios and unmodified SI ratios for X-11 seasonal adjustments. 
#'        Default is not including SI ratios (\code{add_si = FALSE}).
#' @param main_title Character string; main title of plot.  
#'        Default is \code{'Seasonal Sub-Plots'}.
#' @param sub_title Character string; a subtitle for the plot. 
#'        Default is \code{NULL}, which generates a subtitle 
#'        which denotes the series that are plotted.
#' @param y_label Character string; y-axis label for plot, if specified.
#' @param y_limit Numeric vector of length 2; Range of values to be plotted over. 
#'        Default is range of the seasonal factors.
#' @param x_label Character string; label for x-axis of plot.  
#'        Default is a blank x-axis.
#' @param this_col Character array of length 2 or 4; color used for  
#'        seasonal factors, SI-ratios, Modified SI-ratios, and seasonal means. 
#'        Default is generated from the \code{RColorBrewer} palette \code{"Dark2"}, 
#'        with \code{"darkgrey"} added for the seasonal mean line.
#'        For a listing of R color codes, 
#'        see \insertCite{rchartsColors;textual}{blsplotGG}.
#' @param this_plot_cex Numeric scalar; scaling for the plot itself. 
#'        Default is \code{0.8}.
#' @param this_lab_cex Numeric scalar; scaling for plot labels. 
#'        Default is the value of \code{this_plot_cex}.
#' @param this_main_cex Numeric scalar; scaling for main plot title. 
#'        Default is the value of \code{this_plot_cex + 0.1}.
#' @param this_axis_cex Numeric scalar; scaling for plot axis. 
#'        Default is the value of \code{this_plot_cex}.
#' @param this_mar Numeric vector; set margins for the plot. 
#'        Default is \code{c(2.1, 1.5, 3.1, 0.5)}.
#' @param num_facet_rows Integer scalar; number of rows in the facet plot.  
#'        Default is generated from the series frequency (\code{2} for
#'        monthly series, 1 otherwise).
#' @param do_grid Logical scalar; indicates if certain plots will have grid lines. 
#'        Default is no grid lines. 
#' @param this_facet_bg Character scalar; Color used for the facet labels.
#'        Default is \code{"grey90"}.
#' @param this_facet_cex Integer scalar; size used for the facet labels.
#'        Default is \code{0.6}.
#' @param this_tinytheme Character scalar; setting for the \code{tinythere} function 
#'        from the \code{tinyplot} package \insertCite{tinyplot}{blstinyplot}.
#'        Default is \code{"minimal"}.
#' @param this_reset Logical scalar; if TRUE, the values of \code{par} are reset. 
#'        Default is \code{TRUE}.
#' @return Generates a special plot of the seasonal factors (and the SI-ratios) 
#'         grouped by month/quarter.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' air_seas <- seasonal::seas(AirPassengers, arima.model = '(0 1 1)(0 1 1)', 
#'                            x11='', x11.save = c("d8", "d9"))
#' plot_sf(air_seas, add_si = TRUE, 
#'         main_title = 'Air Passengers Seasonal Sub-Plots',
#'         sub_title = "SF = Cyan, SI = Orange, ModSI = Purple, Mean = Gray",
#'         do_grid = TRUE)
#' @import graphics
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
plot_sf <- 
    function(seas_obj = NULL, 
             this_table = NULL, 
             add_si = FALSE, 
             main_title = "Seasonal Sub-Plots", 
             sub_title = NULL,
             y_label = NULL,  
             y_limit = NULL,  
             x_label = " ",  
             this_col = NULL,  
             this_plot_cex = 0.8,  
             this_lab_cex = NULL,  
             this_main_cex = NULL,
             this_axis_cex = NULL,  
             this_mar = c(2.1, 1.5, 3.1, 0.5),  
             num_facet_rows = NULL,
             do_grid = FALSE,
             this_facet_bg = "grey90",
             this_facet_cex = 0.6,
             this_tinytheme = "minimal",
             this_reset = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 1.3.1, 2026-05-14

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
        this_main_cex <- this_plot_cex + 0.1
    }

  # reset par based on user input.
    par(mar = this_mar, cex = this_plot_cex, cex.axis = this_axis_cex, cex.lab = this_lab_cex)

    if (is.null(this_table)) {
        # Try to extract X-11 seasonal factors.
        sf <- tryCatch(seasonal::series(seas_obj, "d10"), error = function(e) {
            NULL
        })
        # If X-11 seasonal factors not available, try to extract SEATS seasonal factors.
        if (is.null(sf)) {
            sf <- tryCatch(seasonal::series(seas_obj, "s10"), error = function(e) {
                NULL
            })
            # If SEATS seasonal factors not available, print error message and stop
            if (is.null(sf)) {
                stop("unable to extract seasonal factors")
            }
            # If SEATS adjustment done, set add_si to FALSE
            if (add_si) {
                add_si <- FALSE
            }
        }
    } else {
        # Try to extract factors.
        sf <- tryCatch(seasonal::series(seas_obj, this_table), error = function(e) {
            NULL
        })
        # If seasonal factors not available, print error message and stop
        if (is.null(sf)) {
            stop(paste0("unable to extract table ", this_table))
        }
        if (add_si) {
            add_si <- FALSE
        }
    }

    # If add_si is TRUE, extract si ratios
    if (add_si) {
        si <- tryCatch(seasonal::series(seas_obj, "d8"), error = function(e) {
            NULL
        })
        # If X-11 SI ratios not available, print error message and stop
        if (is.null(si)) {
            stop("unable to extract SI ratios")
        }
        modsi <- tryCatch(seasonal::series(seas_obj, "d9"), error = function(e) {
            NULL
        })
        # If X-11 SI ratios not available, print error message and stop
        if (is.null(modsi)) {
            stop("unable to extract modified SI ratios")
        }

    }

    # Extract cycle and frequency
    sf_period <- cycle(sf)
    freq <- frequency(sf)

    # Get transformation, then set value of factor mean.
    this_trans <- seasonal::udg(seas_obj, "transform")
    if (this_trans == "Automatic selection") {
        this_trans <- seasonal::udg(seas_obj, "aictrans")
    }
    if (this_trans == "Log(y)") {
        h_bar <- 1
    } else {
        h_bar <- 0
    }
    
    if (is.null(y_label)) {
        y_label <- " "
    }
	
	# Generate monthly labels
    if (freq == 12) {
        this_label <-
           c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    } else {
        # Generate quarterly labels
        if (freq == 4) {
            this_label <- paste0("q", 1:4)
        } else {
            this_label <- paste0("p", 1:freq)
        }
    }
    
    if (is.null(this_col)) {
      if (add_si) {
        ncol <- 3
      } else {
        ncol <- 1
      }
      this_col <- c(RColorBrewer::brewer.pal(ncol, "Dark2"), "grey")
    }
	
	if (is.null(sub_title)) {
        sub_title <- 
			paste0("sf = ", cnv_color_codes(this_col[1]), ", ")
		if (add_si) {
			sub_title <- 
				paste0(sub_title, 
				       "si = ", cnv_color_codes(this_col[2]), ", ", 
				       "modsi = ", cnv_color_codes(this_col[3]), ", ")
		}
		sub_title <- 
			stringr::str_to_title(paste0(sub_title, "mean = ", 
				cnv_color_codes(this_col[ncol + 1])))
	}

    # create vector for factor mean.
    this_mu_vec <- vector(mode = "numeric")
    this_x_vec  <- vector(mode = "numeric")
    this_sf_vec <- vector(mode = "numeric")
    if (add_si) {
      this_si_vec     <- vector(mode = "numeric")
      this_modsi_vec  <- vector(mode = "numeric")
    }
	
	this_year <- time(sf) %/% 1.0
	this_period_vec <- vector(mode = "character")

    # loop through each month or quarter.
    for (i in 1:freq) {

      # save seasonal factors for period i, and generate number of factors.
      this_sf <- sf[sf_period == i]
      this_sf_vec <- c(this_sf_vec, this_sf)
      number_sf <- length(this_sf)

      # Generate X values for period i, mean of SF for period i
      this_x_vec      <- c(this_x_vec, this_year[sf_period == i])
      this_period_vec <- c(this_period_vec, rep(this_label[i], number_sf))
      this_mu_vec     <- c(this_mu_vec, rep(mean(this_sf), number_sf))

      if (add_si) {
        # Generate SI ratios for period i
        this_si <- si[sf_period == i]
        this_si_vec <- c(this_si_vec, this_si)
        this_modsi <- modsi[sf_period == i]
        this_modsi_vec <- c(this_modsi_vec, this_modsi)
      } 

    }
	
	# set number of rows for facet plot of sf and si
	if (is.null(num_facet_rows)) {
		if (freq == 12) {
			num_facet_row <- 2
		} else {
			num_facet_row <- 1
		}
	}
	
	# create data frame for plot
	sf_df <- 
		data.frame(sf   = as.double(this_sf_vec),
		           mean = as.double(this_mu_vec),
		           year = as.double(this_x_vec),
		           per  = factor(this_period_vec, levels = month.abb))
	if (add_si) {
		sf_df$si    <- as.double(this_si_vec)
		sf_df$modsi <- as.double(this_modsi_vec)
	} 
	
	# Set up range of x and y axis.
    if (is.null(y_limit)){
        y_limit <- range(sf_df$sf)
        if (add_si) {
            y_limit <- range(y_limit, sf_df$si)
        }
    }

	tinyplot::tinytheme(this_tinytheme)
	
	op = tinyplot::tpar(
		facet.bg  = this_facet_bg,
		facet.cex = this_facet_cex,
		grid      = do_grid,
		cex.main  = this_main_cex
	)
	
	tinyplot::tinyplot(
		sf ~ year, sf_df,
		facet = ~per, 
		facet.args = list(nrow = num_facet_row),
		type = "l",
		frame = FALSE,
		col = this_col[1],
		ylim = y_limit,
		main = main_title,
		sub = sub_title,
		xlab = x_label,
		ylab = y_label
	)
	tinyplot::tinyplot(
		mean ~ year, sf_df,
		facet = ~per, 
		facet.args = list(nrow = num_facet_row),
		type = "l",
		col = this_col[ncol + 1],
		add = TRUE
	)
	if (add_si) {
		tinyplot::tinyplot(
			si ~ year, sf_df,
			facet = ~per, 
			facet.args = list(nrow = num_facet_row),
			col = this_col[2],
			add = TRUE
		)
		tinyplot::tinyplot(
			modsi ~ year, sf_df,
			facet = ~per, 
			facet.args = list(nrow = num_facet_row),
			col = this_col[3],
			pch = 16,
			add = TRUE
		)
	}
	
    if (this_reset) { 
		tinyplot::tpar(op) 
		tinyplot::tinytheme()
		reset_par(old_par)
	}

}
