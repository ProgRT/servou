cmh <- "cm H₂O"
#' Calculate the Pmus Index from an inspiratory pause
#'
#' @param fichier Filename of the Servo-U reccording file containing
#' an inspiratory pause
#' 
#' @param x Position of the annotation on the x axis
#' 
#' @export

PMI <- function(fichier, numPause=1, debug=FALSE, silent=FALSE) {
	cs <- servou::cyclesWithInspPause(fichier)

	if (length(cs) > 0) {
		c <- cs[[numPause]]
		params <- servou::parse_settings(fichier)
		pause <- c[c$Phase == "maintien de la pause insp.",]
		finPause <- tail(pause, n=25)

		pInsp <- params$PEP + params$`Niv. AI sur PEP`
		pDeb <- pause$Pva[1]
		pMax <- max(pause$Pva)
		pFin <- max(finPause$Pva)

		if(debug == TRUE){
			print(paste("pDeb =", pDeb))
			print(paste("pMax =", pMax))
			print(paste("pFin =", pFin))
		}

		if(pMax - pDeb > 0.1){
			PMI <- pMax - pInsp
		}

		else {
			PMI <- pFin - pInsp
		}
	}

	else  {
		PMI <- NA
		if(silent==FALSE) warning("There were no inspiratory pause in the file analysed")
	}

	PMI
}

#' Draw a graph showing the calculation of the Pmus Index from an
#' inspiratory pause
#'
#' @param fichier Filename of the Servo-U reccording file containing
#' an inspiratory pause
#' 
#' @param x Position of the annotation on the x axis
#' 
#' @export

PMIgraph <- function(fichier,
										 numPause=1,
										 x=NULL,
										 offsetStart=2.5,
										 offsetEnd=1,
										 ai_as_title=FALSE,
										 t=NULL,
										 debug=FALSE
										 ) {
	d <- servou::load_servou_data(fichier)
	cs <- servou::cyclesWithInspPause(fichier)
	c <- cs[[numPause]]
	pause <- c[c$Phase == "maintien de la pause insp.",]

	finPause <- tail(pause, n=25)
	tempsFin <- tail(pause, n=1)$Du
	tempsDeb <- head(pause, n=1)$Du

	dcrop <- d[d$Du > tempsDeb - offsetStart & d$Du < tempsFin + offsetEnd,]

	params <- servou::parse_settings(fichier)

	pInsp <- params$PEP + params$`Niv. AI sur PEP`
	pFin <- max(finPause$Pva)
	IPM = PMI(fichier)

	if (ai_as_title) t <- c( paste("AI =", params$`Niv. AI sur PEP`, cmh))

  plot(dcrop$Dur, dcrop$Pva,
		type="l",
		xlab="Temps (s)",
		ylab="P va (cm H₂O)",
		xaxs="i",
		main=t
	)

	if (is.null(x)) {
		#x <- tempsDeb - .5
		x <- tempsDeb
	}

	if (IPM > 0) {
		dy(x, pInsp, pInsp + IPM, paste("IPM = ", round(IPM, 1), cmh))
	}
	else {
		text(x, pInsp + IPM/2, paste("IPM < 0", cmh), adj=1, font=2)
	}

	if (debug == TRUE) {
		points(pause$Du, pause$Pva, col="green")
		points(finPause$Du, finPause$Pva, col="red")
	}
}

#' Draw a sheet of PMIgraph of every files in a folder
#'
#' @param path Folder containing the Servo-U data files
#' 
#' @export

PMIsheet <- function(path){
	fichiers <- list.files(path, "[0-9]{13}.txt", full.names=TRUE)
	CWP <- c()

	for (f in fichiers) {
		CWP <- c(CWP, length(cyclesWithInspPause(f)))
	}

	fichiers <- fichiers[CWP > 0]

	par(mfrow=c(ceiling(length(fichiers)/3),3), mar=c(5, 4, 4, 2))

	for (f in fichiers){
		if(length(CWP) > 0){
			PMIgraph(f, debug=TRUE, t=basename(f))
		}
	}
}
