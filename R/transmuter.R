
#' Transform data in a bunch using a rule based framework.
#'
#' Transform your data creating a set of rules which act on your data
#' reinforcing code maintenance and keeping your code clean.
#'
#' @name transmute
#' @docType package
#'
#' @import stringr
NULL

`%or%` <- function(value, other) {
	if (! is.null(value)) value else other
}

#' @export
#' @importFrom methods setRefClass
Transmuter <- setRefClass("Transmuter",
	fields=list(envir="environment", rules="list"),
	methods=list(
		apply_rules=function(.data) {
			res <- rule_result()
			for (.rule in iter_rules(.self$rules)) {
				res <- apply_rule(.rule, .data)
				if (res$applied)
					break
			}
			if (res$applied)
				res$value
			else
				.data
		},
		transmute=function(x) {
			if ( is(x, 'data.frame') ) {
				do.call('data.frame', c(lapply(x, .self$apply_rules), stringsAsFactors=FALSE, check.names=FALSE))
			} else {
				.self$apply_rules(x)
			}
		},
		print=function() {
			pl <- take(.self$rules, 'priority')
			l <- list()
			i <- 1
			for (.po in order(pl)) {
				.names <- names(.self$rules)
				.parser <- .self$rules[[.po]]
				l[[i]] <- c('name'=.names[.po], 'regex'=.parser[[1]], 'priority'=.parser[['priority']])
				i <- i + 1
			}
			base::print(as.data.frame(do.call(rbind, l)), row.names=FALSE, right=FALSE, na.print="-")
		}
	)
)

#' @export
transmuter <- function(...) {
	objs <- list(...)

	idx <- sapply(objs, function(x) is(x, 'match_rule'))
	rules <- objs[idx]

	idx <- sapply(objs, function(x) is(x, 'Transmuter'))
	parents <- objs[idx]
	for (parent in parents) {
		rules <- append(rules, parent$rules)
	}
	
	Transmuter$new(envir=new.env(), rules=rules)
}

#' @export
print.Transmuter <- function(x, ...) {
	x$print()
}

# #' @export
# transmute <- function(...) {
# 	UseMethod('transmute')
# }
#
# #' @export
# transmute.data.frame <- function(data, ...) {
# 	.parser <- transmuter.default(NULL, ...)
# 	.parser$parse(data)
# }
