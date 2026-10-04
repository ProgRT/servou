#' Evidence a variation (delta) in the y axis on a previously plotted
#' grahp
#' 
#' @param x Position in the x axis of the annotation
#' 
#' @param y0  Starting value in the y axis
#'
#' @param y1  End value in the y axis
#'
#' @param lab  label to be printed beside the annotation
#' 
#' @export

dy <- function (x, y0, y1, lab) {
	delta <- y1 - y0
	margin <- delta * 0.01

	arrows(x, y0 + margin,
				 x, y1 - margin,
				 code=3,
				 length=0.1,
				 lwd=1.5
				 )

	text(x, y0 + delta/2,
			 paste0(lab, "    "),
			 adj=1,
			 font=2
			 )
}
