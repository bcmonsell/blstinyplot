#' Color name for display
#'
#' Generates color names for display on plot labels and subheaders.
#'
#' Version 1.1, 2026-04-07
#'
#' @param this_color_code character string of color code to be used in plot
#'        This is a required entry.
#' @param strip_numbers logical scalar that controls if numbers at 
#'        the end of the text are stripped from the color name. 
#'        Default is TRUE.
#' @return character string of color name closest to hexidecimal color input 
#'        (if used) stripped of numbers if \code{strip_numbers = TRUE}.
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @examples
#' this_color_blind <- color_blind_palette()
#' this_color_blind_text <- array(NA, dim = 8)
#' for (i in 1:8) {
#'     this_color_blind_text[i] <- display_color(this_color_blind[i])
#' }
#' @export
display_color <- 
   function(this_color_code = NULL, strip_numbers = FALSE) {
    # Author: Brian C. Monsell (OEUS) Version 1.1, 2026-04-07
	
    # check if color code starts with \code{'#'}
    if (substr(this_color_code, 1, 1) == "#") {
        this_color_code <- cnv_color_codes(this_color_code)
    }
    # return result after removing numbers from the end of the code
    if (strip_numbers) {
        return(gsub('[0-9]+', '', this_color_code))
    } else {
        return(this_color_code)
    }
}