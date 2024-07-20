fitSumary <- function(fit) {
	paste(
			 paste("B2 =", format(fit$b2, digits=3)),
			 paste("Deviance =", format(fit$deviance, digits=3)),
			 sep="\n"
	)
}

#' @export

fitPlot <- function(fit, details=FALSE) {
	i <- format(fit$index, digits=2) 
	title <- paste("Indice =", i) 

	plot(fit$x, fit$y,
			 type='l',
			 ylab="Débit (l/m)",
			 xlab="Temps (s)",
			 main=title)
	lines(fit$x, fit$fitted, col='red')

	if (details) {
		text( max(fit$x), max(fit$y), fitSumary(fit), adj=c(1,1))
	}
}

fitAll <- function(cycles, labels=NULL, ...) {
	par(mfrow=c(2,4))

	for(i in 1:length(cycles)) {
		f <- fitDecel(cycles[[i]])
		fitPlot(f, ...)
	}
}
