
#' @export
take <- function(x, ...) {
	UseMethod('take', x)
}

#' @export
take.list <- function(x, k, ...) {
	sapply(x, function(x) {
		v <- x[[k]]
		if (is.null(v))
			NA
		else
			v
	})
}
