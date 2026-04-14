#' Generates color names from hexidecimal input
#'
#' Generates vector of closest color names from hexidecimal color input. 
#' This uses the \code{color.id} function of the \code{plotrix} package 
#' \insertCite{plotrix}{blstinyplot} to do the conversion.
#'
#' Version 1.1, 2026-04-01
#'
#' @param color_vec vector of color codes
#'        This is a required entry.
#' @return vector of color names closest to hexidecimal color input.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' Moonrise_Codes <- 
#'    c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A", "#798E87", 
#'      "#C27D38", "#CCC591", "#29211F", "#85D4E3", "#F4B5BD", 
#'      "#9C964A", "#CDC08C", "#FAD77B")
#' Moonrise_All <- cnv_color_codes(Moonrise_Codes)
#' @importFrom Rdpack reprompt
#' @export
cnv_color_codes <- function(color_vec = NULL) {
    # Author: Brian C. Monsell (OEUS) Version 1.1, 2026-04-01
    
    # initial name vector for colors
    name_vec <- array(" ", length(color_vec))
    
    # Loop through each value in color_vec
    for (i in 1:length(color_vec)) {
        # use color.id to convert
        this_Col <- plotrix::color.id(color_vec[i])
        # if more than one name returned, use first one
        if (length(this_Col) > 1) {
            name_vec[i] <- this_Col[1]
        } else {
            name_vec[i] <- this_Col
        }
    }
    return(name_vec)
}

