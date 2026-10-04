#' Create a data frame sumarizing all the Servo-U data file contained
#' in a folder
#'
#' @param dossier Dossier contemamt les fichiers
#'
#' @export

filessumary <- function (dossier) {
	fichiers <- list.files(dossier, "[0-9]{13}\\.txt", full.names=TRUE)

	APPAREILS <- c()
	DATES <- c()
	AI <- c()
	PMIS <- c()
	POCCS <- c()
	FIS <- c()
	FREQS <- c()

	for (f in fichiers) {
		m <- parse_meta(f)
		s <- parse_settings(f)

		APPAREILS <- c(APPAREILS, m$Ventilateur)
		DATES <- c(DATES, s$Date)
		AI <- c(AI, s$Niv)
		POCCS <- c(POCCS, servou::Pocc(f))
		PMIS <- c(PMIS, servou::PMI(f, silent=TRUE))
		FIS <- c(FIS, mean_fi(f))
		FREQS <- c(FREQS, median(ftrig(f)))
	}

	data.frame(
		Fichier=fichiers,
		Appareil=APPAREILS,
		Date=DATES,
		AI=AI,
		PMI=PMIS,
		Pocc=POCCS,
		FI=FIS,
		Freq=FREQS
	)
}

#' Create a table sumarizing all the Servo-U data file contained in a
#' folder
#' 
#' @param df Data frame to use for creation of the table or folder
#' path to create it
#'
#' @export

files_table <- function(df) {
	if (typeof(df) == "character") df <- filessumary(df)

	df$Fichier <- basename(df$Fichier)
	df$Date <- as.POSIXct(df$Date, format="%d/%m/%y %H:%M:%S")

	options(knitr.kable.NA = "-")
	knitr::kable(df, "simple",
		col.names=c(
								"Fichier",
								"Appareil",
								"Date",
								"Aide inspi.",
								"PMI",
								"Δ P occ",
								"Ind. V'",
								"Freq."),
		align=c("l", "c", "c", "c", "c", "c", "c", "c"),
		format.args=list(decimal.mark=","),
		digits=1
	)
}

#' Display every ventilation cycle contained in
#' `filename`
#' 
#' @param file Filename containing the data to analyse and display
#'
#' @export

cycles_preview <- function(file){
	d <- servou::load_servou_data(file)
	cs <- servou::cycles(d)

	x11(title=file)
	par(
			mfrow=c(ceiling(length(cs)/4),4),
			mar=c(5, 4, 8, 2)
	)

	for(i in 1:length(cs)){
		plot(cs[[i]]$Du, cs[[i]]$DÉ, title=paste("Cycle", i))
	}
}

#' Calculate the mean Flow index of ventilation cycles in a Servo-U
#' datafile
#'
#' @param file Path of the Servo-U data file to analyse
#'
#' @export

mean_fi <- function(file) {
	d <- servou::load_servou_data(file)
	cs <- servou::cycles(d)
	ISS <- c()
	for(i in 2:(length(cs)-1)){
		ISS <- c(ISS, fitDecel(cs[[i]])$index)
	}
	mean(ISS, na.rm=TRUE)
}

#' Calculate the frequency of ventilator triggering by the patient
#'
#' @param file Path of the Servo-U data file to analyse
#'
#' @export

ftrig <- function(file) {
	if (typeof(file) == "character") d <- load_servou_data(file)
	else d <- file

	t <- d[d$Trigger != "", ]
	p <- as.numeric(diff(t$Durée))
	60/p
}
