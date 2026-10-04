#' Parse Servo-U settings
#' 
#' @param filename File name (including path) containing the data to
#' load. Must be a standard file exported by a Servo-U ventilator in
#' *french*.
#'
#' @return t List of ventiator settings.
#'
#' @export

parse_settings <- function(filename) {
	lines <- readLines(filename)
	sep <- grep("=======", lines)
	pEnd <- grep("DATA", lines) - 2

	l1 <- strsplit(lines[(sep+1):pEnd], "\t")
	names(l1) <- unlist(lapply(l1,function(p){p[1]}))
	l1 <- lapply(l1, function(p){
				 if(! is.na(p[3])){
					 value <- as.numeric(sub(",", ".", p[2]))
				 }
				 else { value <- p[2] }
				 value
  })

	l1
}

#' Parse Servo-U data file meta-information
#' 
#' @param filename File name (including path) containing the data to
#' load. Must be a standard file exported by a Servo-U ventilator in
#' *french*.
#'
#' @return t List of ventiator settings.
#'
#' @export

parse_meta <- function(filename) {
	lines <- readLines(filename)
	pEnd <- grep("=======", lines) - 1

	l1 <- strsplit(lines[2:pEnd], "\t")
	names(l1) <- unlist(lapply(l1,function(p){p[1]}))
	l1 <- lapply(l1, function(p){
				 if(! is.na(p[3])){
					 value <- as.numeric(sub(",", ".", p[2]))
				 }
				 else { value <- p[2] }
				 value
  })

	l1
}
#' Display a table of ventilator settings from Servo-U data file
#'
#' @param f Filename
#'
#' @export

#settings_table <- function (f) {
#	p <- servou::parse_settings(f)
#	df <- data.frame(Paramètre=names(p), Valeur=unlist(p), row.names=NULL)
#	knitr::kable(df)
#}
