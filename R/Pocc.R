#' Caculate the variation of airway pressure generation by a pateint
#' inspiratory effort during an expiratory occlusion maneuver (Δ P occ)
#'
#' @param file Servou data file to analyse
#'
#' @export

Pocc <- function (fichier, numPause=1) {
	d <- servou::load_servou_data(fichier)
	param <- servou::parse_settings(fichier)

	PEP <- param$PEP
	cwp <- cyclesWithExpPause(fichier)

	if(length(cwp) == 0) delta <- NA
	else {
		cycle <- cwp[[numPause]]
		pause <- cycle[cycle$Phase == "maintien de la pause exp.",]
		delta <- PEP - min(pause$Pva)
	}
	delta
}

#' Create a graph evidencing the evaluating of Pmus by an expiratory
#' occlusion maneuver
#'
#' @param fichier Servou-U reccording file containing an expiratory
#' occlusion
#' 
#' @param numPause ventilationcycle inthe file with the expiratory
#' occlusion
#'
#' @param x Where to put the arrow represingting Delta Pocc (in x
#' unit)
#' 
#' @export

pmusGraph <- function (fichier, numPause=1, x=NULL, ...) {
	d <- servou::load_servou_data(fichier)
	param <- servou::parse_settings(fichier)
	PEP <- param$PEP

	cycles <- cyclesWithExpPause(fichier)
	cycle <- cycles[[numPause]]

	pause <- cycle[cycle$Phase == "maintien de la pause exp.",]

	if (is.null(x)) x <- pause$Du[1] - 0.2

	peak <- min(pause$Pva)
	delta <- PEP - peak
	margin <- delta * 0.01

	plot(
			 cycle$Du,
			 cycle$Pva,
			 type="l",
			 xlab="Temps (s)",
			 # ylab="P va (cm H₂O)",
			 ylab="Pva (mbar)",
			 xaxs="i",
			 ...
	)

	dy(x, peak + margin, PEP - margin,
		 # paste("Δ P occ =", round(delta,1), "cm H₂O")
		 paste("dPocc =", round(delta,1), "mbar")
	)
}

#' Create a graph evidencing the evaluating of Pmus by an expiratory
#' occlusion maneuver
#'
#' @param fichier Servou-U reccording file containing an expiratory
#' occlusion
#' 
#' @param numPause ventilationcycle inthe file with the expiratory
#' occlusion
#'
#' @param x Where to put the arrow represingting Delta Pocc (in x
#' unit)
#' 
#' @export

pmusHighlight <- function (fichier, numPause=1, x=NULL, ...) {
	d <- servou::load_servou_data(fichier)
	param <- servou::parse_settings(fichier)
	PEP <- param$PEP

	cycles <- cyclesWithExpPause(fichier)
	cycle <- cycles[[numPause]]

	pause <- cycle[cycle$Phase == "maintien de la pause exp.",]

	if (is.null(x)) x <- pause$Du[1] - 0.2

	peak <- min(pause$Pva)
	delta <- PEP - peak
	margin <- delta * 0.01
  label <- paste("Δ P occ =", round(delta,1), "cm H₂O")

#  plot(
#  		 cycle$Du,
#  		 cycle$Pva,
#  		 type="l",
#  		 xlab="Temps (s)",
#  		 ylab="P va (cm H₂O)",
#  		 xaxs="i",
#  		 ...
#  )

	dy(x, peak + margin, PEP - margin, label)
}
