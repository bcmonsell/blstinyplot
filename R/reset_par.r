#' Reset \code{par()}
#'
#' Reset graphics parameters for plots; taken from \insertCite{resetPar;textual}{blstinyplot}.
#'
#' Version 1.1, 2026-04-07
#'
#' @param old_par List object; old graphical parameters saved from \code{par}.
#'        This is a required entry.
#' @return returns reset graphics parameters
#'
#' @author Brian C. Monsell, \email{monsell.brian@@bls.gov} or \email{bcmonsell@@gmail.com}
#'
#' @references
#'  \insertAllCited{}
#'
#' @examples
#' old.par <- par(no.readonly = TRUE)
#' par(mar=c(5.1, 3.1, 4.1, 1.1), mfrow=c(2,2))
#' xt_names <- names(xt_data_list)
#' for (i in 1:4) {
#'      plot(xt_data_list[[i]], main = xt_names[i], type="l")
#' }
#' reset_par(old.par)
#' @import graphics
#' @import stats
#' @importFrom Rdpack reprompt
#' @export
reset_par <- function(old_par = NULL){
    # Author: Brian C. Monsell (OEUS) Version 1.1, 2026-04-07
	
	if (is.null(old_par)) {
        cat("Argument old_par must be specified.")
		return(NULL)
	}
	
    par(old_par)
}