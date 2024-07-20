#' Extrait les le lignes où il y a passage de la phase expiratoire à
#' inspiratoire ou inversement
#' 
#' @param dataset Dataset returned by `servou::load_servou_data()`

changementPhase <- function(dataset) {
	p1 <- dataset$Phase
	p2 <- c(p1[1], p1[1:length(p1)-1])
	dataset[p1!=p2,]
}

declenchement <- function(dataset) {
	cf <- changementPhase(dataset)
	cf[cf$Phase == 'insp.',][["Dur\u00E9e"]]
}

cycle <- function(d, i) {
	decl = c(0, declenchement(d), Inf)
	d[d[["Dur\u00E9e"]] > decl[i] & d[["Dur\u00E9e"]] < decl[i+1],]
}

#' Return a list containing the dataset splited in ventilation
#' cycles
#'
#' @param d Dataset returned by `servou::load_servou_data()`
#' @return A list containing a dataset for each ventilation cycle
#' contained in the input dataset
#' @export

cycles <- function(d) {
	vCycles <- list()
	decl = c(0, declenchement(d), Inf)

	for (i in 1:(length(decl)-1)) {
		vCycles[[i]] <- cycle(d, i)
	}

	vCycles
}
