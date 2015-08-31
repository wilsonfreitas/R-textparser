
#' @export
.unformat <- function(x, f) {
	x <- lapply(x, f)
	as.data.frame(x, stringsAsFactors=FALSE)
}

#' @export
unformat <- function(x) .unformat(x, as.character)

#' @export
unfactor <- function(x) .unformat(x, function(x) {
	if (is.factor(x))
		as.character(x)
	else
		x
})
