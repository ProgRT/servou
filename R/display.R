#' Display Servo-U data loaded by *parse_servou_data()*
#' 
#' @param dataset Data table returned by *load_servou_data()*.
#'
#' @export

display <- function(dataset){
	if (typeof(dataset) == "character") {
		dataset = load_servou_data(dataset)
	}
	graphics::par(mfcol=c(2,1), mar=c(5, 4, .5, 2))

	vPlot <- function(x, y, ...) {
		plot(x, y,
				 type="l",
				 xaxs='i',
				 bty='l',
				 xlab="Temps (s)",,
				 ...
				 );
	}

	vPlot(dataset$Dur, dataset$PRESSION, ylab="Pression (mbar)")
	vPlot(dataset$Dur, dataset[["D\u00C9BIT"]], ylab="D\u00E9bit (l/m)")
}
