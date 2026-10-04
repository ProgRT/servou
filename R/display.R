#' Display Servo-U data loaded by *parse_servou_data()*
#' 
#' @param dataset Data table returned by *load_servou_data()*.
#'
#' @param title Title to put on top of the page
#'
#' @param notitle Should a title be displayed
#'
#' @export

display <- function(dataset, title="", notitle=FALSE){

	if (typeof(dataset) == "character") {
		if (title == "") title <- dataset
		dataset = load_servou_data(dataset)
	}

	if (notitle == FALSE & title != "") omi <- c(0,0,.5,0) 
	else omi <- c(0,0,.25,0) 
	graphics::par(mfcol=c(2,1), mar=c(5, 4, .5, 2), omi=omi)

	vPlot <- function(x, y, ...) {
		plot(x, y,
				 type="l",
				 xaxs='i',
				 bty='l',
				 xlab="Temps (s)",,
				 ...
				 );
	}

	if("PRESSION" %in% colnames(dataset)){
		vPlot(dataset$Dur, dataset$PRESSION, ylab="Pression (mbar)")
	}
	else {
		vPlot(dataset$Dur, dataset$Pva, ylab="Pression (mbar)")
	}

	if(exists("title") & notitle == FALSE) {title(title, outer=TRUE)}
	vPlot(dataset$Dur, dataset[["D\u00C9BIT"]], ylab="D\u00E9bit (l/m)")
	abline(h=0, col=gray(0.6))
}
