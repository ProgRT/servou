#' Load Servo-U ventilation parameters
#' 
#' @param filename File name (including path) containing the data to
#' load. Must be a standard file exported by a Servo-U ventilator in
#' *french*.
#'
#' @return t Data table of ventiation parameters.
#'
#' @export

load_servou_meta <- function(filename) {
	lines <- readLines(filename)
	headLength <- grep("==========", lines)
	lines <- lines[2:(headLength-1)]
	p <- strsplit(lines, "\t")
	names(p) <- unlist(lapply(p, function(p){p[1]}))
	p <- lapply(p, function(p){trimws(p[2])})
	p
}
