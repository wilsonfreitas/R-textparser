
#' Transform data in a bunch using a rule based framework.
#'
#' Transform your data creating a set of rules which act on your data
#' reinforcing code maintenance and keeping your code clean.
#'
#' @name transmute
#' @docType package
#'
#' @import stringr
#' @import methods
NULL

`%or%` <- function(value, other) {
	if (! is.null(value)) value else other
}

#' @export
setClass("Transmuter",
	representation(envir="environment", rules="list"),
	prototype(envir=NULL, rules=NULL)
)

#' @export
setGeneric("transmute", function(x, data, ...) standardGeneric("transmute"))

#' @export
setGeneric("apply_rules", function(x, data, ...) standardGeneric("apply_rules"))

#' @export
setMethod("apply_rules",
	signature("Transmuter", "ANY"),
	function(x, data) {
		res <- rule_result()
		for (.rule in iter_rules(x@rules)) {
			res <- apply_rule(.rule, data)
			if (res$applied)
				return(res$value)
		}
		data
	}
)

#' @export
setMethod("transmute",
	signature("Transmuter", "data.frame"),
	function(x, data) {
		rules_res <- lapply(data, function(.data) apply_rules(x, .data))
		do.call('data.frame', c(rules_res, stringsAsFactors=FALSE, check.names=FALSE))
	}
)

#' @export
setMethod("transmute",
	signature("Transmuter", "ANY"),
	function(x, data) apply_rules(x, data)
)

#' @export
setMethod("print",
	signature(x="Transmuter"),
	function(x, ...) {
		pl <- take(x@rules, 'priority')
		l <- list()
		i <- 1
		for (.po in order(pl)) {
			.names <- names(x@rules)
			.parser <- x@rules[[.po]]
			l[[i]] <- c('name'=.names[.po], 'regex'=.parser[[1]], 'priority'=.parser[['priority']])
			i <- i + 1
		}
		print.data.frame(as.data.frame(do.call(rbind, l)), row.names=FALSE, right=FALSE, na.print="-")
		invisible(x)
	}
)

#' @export
transmuter <- function(...) {
	objs <- list(...)

	idx <- sapply(objs, function(x) is(x, 'match_rule'))
	rules <- objs[idx]

	idx <- sapply(objs, function(x) is(x, 'Transmuter'))
	parents <- objs[idx]
	for (parent in parents) {
		rules <- append(rules, slot(parent, 'rules'))
	}

	new("Transmuter", envir=new.env(), rules=rules)
}

#' @export
transmute_regex <- function(.x, .r, .f, apply_to = c('any', 'all')) {
  trm <- transmuter(match_regex(.r, .f, apply_to = apply_to))
  transmute(trm, .x)
}

