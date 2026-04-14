#' Color-blind friendly color palette
#'
#' Color palettes that can be used that can be distinguished by color-blind people 
#' (either from \insertCite{RColorBrewer;textual}{blstinyplot}
#' or \insertCite{cookbookrColorsggplot2;textual}{blstinyplot}).
#'
#' Version 1.1, 2026-04-01
#'
#' @param with_grey Logical scalar; whether color blind pallate contains \code{'grey'}, otherwise the palette contains \code{black}. Default is TRUE. 
#' @return vector of hexadecimal color codes that form a color palette that can be distinguished by color-blind people.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' this_color_blind <- color_blind_palette(FALSE)
#' @export
color_blind_palette <- function(with_grey = TRUE) {
    # Author: Brian C. Monsell (OEUS) Version 1.1, 2026-04-01
    
    # Based on value of \code{with_grey}, return a version of a color blind palette
    if (with_grey) {
        # The palette with grey:
        return(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
    } else {
        # The palette with black:
        return(c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))
    }
}
	       