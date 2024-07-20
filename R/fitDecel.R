#' Shift a vector by 1 to the right, repeating the first value and
#' poppint the last value to keet the same length
#' @export

shift1 <- function (v) {
	c(v[1], v[0:(length(v)-1)])
}

#' Select the part of the inspiratory flow waveform folowing the
#' inspiratory rise time.

pp <- function(dataset, aTreshold=.01) {
	di = dataset[dataset$Phase == "insp.",]

	df = di$DÉBIT
	dm1f = c(df[1], df[0:(length(df)-1)])

	rise <- di[df/dm1f > (1 + aTreshold),]
	riseEnd <- rise[nrow(rise),]
	di[di$Du > riseEnd$Du,]
}

#' Fit an inverse pababolic function to the inspiratory flow waveform
#' to calculate the *flow index*.
#' 
#' @export

fitDecel <- function(dataset, aTreshold=0.01) {
	decel <- pp(dataset, aTreshold=aTreshold)
	y <- decel$DÉ
	x <- as.numeric(decel$Du - decel$Du[1])

	start <- list(b1=max(y), b2=max(y)/max(x), b3=2)
	res <- nls(y ~ b1 - b2 * x^b3, start=start)

	list(
			 x=x,
			 y=y,
			 start=decel$Du[1],
			 index=coef(res)[3],
			 b2=coef(res)[2],
			 fitted=fitted(res),
			 deviance=deviance(res)
			 )
}
