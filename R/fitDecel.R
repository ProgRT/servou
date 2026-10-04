#' Shift a vector by 1 to the right, repeating the first value and
#' poppint the last value to keet the same length
#' @export

expi <- function(c) {
	c[c$DÉ < 0 & c$Phase == "exp.",]
}

#' @export

inspi <- function(c) {
	c[c$Du < expi(c)$Du[1],]
}

cshift <- function (v) {
	c(v[1], v[1:(length(v)-1)])
}

fderiv <- function (d) {
	d$DÉ / cshift(d$DÉ)
}

#' Select the part of the inspiratory flow waveform folowing the
#' inspiratory rise time.

pp <- function(dataset, aTreshold=.01) {
	inspi <- inspi(dataset)

	df = inspi$DÉBIT

	rise <- inspi[df/cshift(df) > (1 + aTreshold),]
	riseEnd <- rise[nrow(rise),]
	inspi[inspi$Du > riseEnd$Du & fderiv(inspi) > 0.9,]
}

#' Fit an inverse pababolic function to the inspiratory flow waveform
#' to calculate the *flow index*.
#' 
#' @param cycle Cycle to fit
#' @param aTreshold Treshold to pass to pp()
#' @param silent Wether the errors and warnings of snls should be supressed
#' @export

fitDecel <- function(cycle, aTreshold=0.01, silent=TRUE) {
	decel <- pp(cycle, aTreshold=aTreshold)
	y <- decel$DÉ
	x <- as.numeric(decel$Du - decel$Du[1])

	start <- list(b1=max(y), b2=max(y)/max(x), b3=2)
	# res <- nls(y ~ b1 - b2 * x^b3, start=start)

	failed <- TRUE
	try(
			{
				res <- nls(y ~ b1 - b2 * x^b3, start=start);
				failed <- FALSE
			},
			silent=silent
	)

	if(failed) {
		ret <- list(index=NA)
	}

	else {
	ret <- list(
			 x=x,
			 y=y,
			 start=decel$Du[1],
			 index=coef(res)[3],
			 b2=coef(res)[2],
			 fitted=fitted(res),
			 deviance=deviance(res)
			 )
	}
	ret
}
