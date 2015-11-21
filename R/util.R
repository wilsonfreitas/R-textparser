
#' @export
take <- function(x, ...) {
	UseMethod('take', x)
}

#' @export
take.list <- function(l, k) {
	sapply(l, function(x) {
		v <- x[[k]]
		if (is.null(v))
			NA
		else
			v
	})
}
