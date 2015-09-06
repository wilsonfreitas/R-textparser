

#' @export
priority <- function(x) {
	structure(as.numeric(x), class='priority')
}

#' @export
parser <- function(regex, handler, ...) {
	that <- list(regex=regex, handler=handler)
	.args <- list(...)
	.classes <- sapply(.args, class)
	if ('priority' %in% .classes)
		that$priority <- as.numeric(.args[[which(.classes == 'priority')[1]]])
	else
		that$priority <- NA
	structure(that, class='parser')
}

