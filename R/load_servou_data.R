#' Load Servo-U data
#' 
#' @param filename File name (including path) containing the data to
#' load. Must be a standard file exported by a Servo-U ventilator in
#' *french*.
#'
#' @return t Data table with the folowing columns: Durée, Phase,
#' PRESSION, DÉBIT, VOLUME, Trigger. Aditional columns may also be
#' present (e.g. EADI, Poes).
#'
#' @export

load_servou_data <- function(filename) {
	lines <- readLines(filename)
	headLength <- grep("DATA", lines)

	headstring <- lines[grep("Dur\u00E9e",lines)]
	headings <- unlist(stringr::str_split(headstring, "\t"))
	headings <- stringr::str_match(headings, "(\\w+)(\\s\\w+)?(?:\\s\\((.+)\\))?")

	t <- utils::read.delim(
									filename,
									dec=",",
									header=TRUE,
									col.names=headings[,2],
									skip=headLength,
									check.names=FALSE
	)
	t[["Dur\u00E9e"]] <- readr::parse_time(stringr::str_replace(t[["Dur\u00E9e"]], ":(\\d{3})", "\\.\\1"), "%H:%M:%OS")
	t[["Dur\u00E9e"]] <- t[["Dur\u00E9e"]] - t[["Dur\u00E9e"]][1]
	t
}
