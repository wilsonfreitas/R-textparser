
#' Compact text parser
#' 
#' Transform text into data with simple rules.
#' 
#' @name textparser
#' @docType package
#' 
#' @import stringr
NULL

take <- function(x, ...) {
	UseMethod('take', x)
}

take.list <- function(l, k) {
	sapply(l, function(x) {
		v <- x[[k]]
		if (is.null(v))
			NA
		else
			v
	})
}

#' @export
#' @importFrom methods setRefClass
TextParser <- setRefClass("TextParser",
	fields=list(envir="environment", parsers="list"),
	methods=list(
		apply_parsers=function(text) {
			if ( !is.character(text) )
				return(text)
			pl <- take(.self$parsers, 'priority')
			for (.po in order(pl)) {
				.parser <- .self$parsers[[.po]]
				result <- stringr::str_match(text, .parser$regex)
				if ( ! is.na(result[1,1]) )
					return(.parser$handler(text, result))
			}
			return(text)
		},
		parse=function(x) {
			if ( is.data.frame(x) ) {
				as.data.frame(lapply(x, .self$apply_parsers), stringsAsFactors=FALSE)
			} else {
				.self$apply_parsers(x)
			}
		},
		print=function() {
			pl <- take(.self$parsers, 'priority')
			l <- list()
			i <- 1
			for (.po in order(pl)) {
				.names <- names(.self$parsers)
				.parser <- .self$parsers[[.po]]
				l[[i]] <- c('name'=.names[.po], 'regex'=.parser[[1]], 'priority'=.parser[['priority']])
				i <- i + 1
			}
			base::print(as.data.frame(do.call(rbind, l)), row.names=FALSE, right=FALSE, na.print="-")
		}
	)
)

#' @export
# textparser.default <- function(expr, parent=NULL) {
# 	if ( is.null(parent) )
# 		envir <- new.env()
# 	else
# 		envir <- parent$envir
# 	eval(substitute(eval(quote({ expr }))), envir)
# 	parser_names <- Filter(function(x) stringr::str_detect(x, '^parse'), ls(envir=envir))
# 	parsers <- lapply(parser_names, function(x) {
# 		get(x, envir=envir)
# 	})
# 	names(parsers) <- parser_names
# 	TextParser$new(envir=envir, parsers=parsers)
# }

#' @export
textparser <- function(data=NULL, ...) {
	UseMethod('textparser')
}

#' @export
textparser.data.frame <- function(data, ...) {
	.parser <- textparser.default(NULL, ...)
	.parser$parse(data)
}

#' @export
textparser.default <- function(data=NULL, ...) {
	objs <- append(list(...), data)
	classes <- sapply(objs, class)
	parsers <- objs[which(classes == 'parser')]
	parents <- objs[which(classes == 'TextParser')]
	for (parent in parents) {
		parsers <- append(parsers, parent$parsers)
	}
	TextParser$new(envir=new.env(), parsers=parsers)
}

#' @export
print.TextParser <- function(x, ...) {
	x$print()
}
