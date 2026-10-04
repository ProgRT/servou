fitSumary <- function(fit) {
	paste(
			 paste("B2 =", format(fit$b2, digits=3)),
			 paste("Deviance =", format(fit$deviance, digits=3)),
			 paste("Start =", format(fit$start, digits=3)),
			 sep="\n"
	)
}
#' Adjust an inverse exponential curve to the decelerating portion of
#' a flow waveform to compute the _Flow Index_.
#' 
#' @param c Ventilation cycle (one inspiration and one expiration)
#' returned by servou::cycle or servou::cycles.
#' 
#' @export

fitPlot <- function(c, details=FALSE, title=NULL, posI="graphic") {
	fit <- fitDecel(c)
	i <- format(fit$index, digits=2) 
	iText <- paste("Ind. de débit =", i)

	if (posI == "title") {
		title = c(title, iText)
	}

	plot(fit$x, fit$y,
			 type='l',
			 ylab="Débit (l/m)",
			 xlab="Temps (s)",
			 main=title)
	lines(fit$x, fit$fitted, col='red')

	if (posI == "graphic") {
		text(
				 #fit$x[ceiling(.65 * length(fit$x))],
				 #fit$y[ceiling(length(fit$x)/2)],
				 max(fit$x),
				 max(fit$y),
				 iText, font=2,
				 adj=c(1, 1)
				 )
	}

	if (posI == "corner") {
		text(
				 max(fit$x), max(fit$y),
				 iText, font=2, adj=c(1,1)
				 )
	}

	if (details) {
		text( max(fit$x), max(fit$y), fitSumary(fit), adj=c(1,1))
	}
}

#' Apply `servou::fitDecel()` to every ventilation cycle contained in
#' `filename` ant display a sheet of graphical representation of the
#' results
#' 
#' @param file Filename containing the data to analyse and display
#'
#' @export

fit_decel_preview <- function(file){
	d <- servou::load_servou_data(file)
	cs <- servou::cycles(d)
	nplots <- length(cs) - 1
	print(paste(nplots, "plots to print"))
	
	x11(title=file)
	par(
			mfrow=c(ceiling(nplots/4),4),
			mar=c(5, 4, 8, 2)
	)

	for(i in 2:nplots){
		servou::fitPlot(cs[[i]], title=paste("Cycle", i))
	}
}

