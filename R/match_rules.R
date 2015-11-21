
# setClass(
# 	Class="RegexRule",
# 	representation=representation(
# 		regex="character",
# 		handler="function",
# 		priority="priority"
# 	)
# )

.apply_to <- function(x) {
	switch(x,
		all=all,
		any=any,
		majority=function(x) {
			.len <- length(x)
			.sum <- sum(x)
			.sum/.len > 0.5
		}
	)
}

rule_result <- function(applied=FALSE, value=NULL) {
	structure(list(applied=applied, value=value), class='rule_result')
}

#' @export
match_regex <- function(regex, handler, priority=NA, apply_to=c('any', 'all'), na.rm=TRUE) {
	apply_to <- .apply_to(match.arg(apply_to))
	structure(
		list(regex=regex, handler=handler, priority=priority, apply_to=apply_to, na.rm=na.rm),
		class=c('match_rule', 'regex_rule')
	)
}

#' @export
match_class <- function(class, handler, priority=NA, na.rm=TRUE) {
	structure(
		list(class=class, handler=handler, priority=priority, na.rm=na.rm),
		class=c('match_rule', 'class_rule')
	)
}

#' @export
match_predicate <- function(predicate, handler, priority=NA, na.rm=TRUE) {
	structure(
		list(predicate=predicate, handler=handler, priority=priority, na.rm=na.rm),
		class=c('match_rule', 'predicate_rule')
	)
}

apply_rule <- function(rule, .data) {
	UseMethod('apply_rule')
}

apply_rule.class_rule <- function(rule, .data) {
	if ( ! is(.data, rule$class) )
		return(rule_result())
	result <- rule$handler(.data)
	rule_result(TRUE, result)
}

apply_rule.predicate_rule <- function(rule, .data) {
	idx <- rule$predicate(.data)
	if ( length(which(idx)) == 0 )
		return(rule_result())
	result <- rule$handler(.data, idx)
	rule_result(TRUE, result)
}

apply_rule.regex_rule <- function(rule, .data) {
	if ( ! is(.data, 'character') )
		return(rule_result())
	detect <- stringr::str_detect(.data, rule$regex)
	apply_to <- rule$apply_to(detect, na.rm=rule$na.rm)
	result <- if ( apply_to ) rule$handler(.data, stringr::str_match(.data, rule$regex))
	rule_result(apply_to, result)
}

iter_rules <- function(rules) {
	idx <- take(rules, 'priority')
	idx <- order(idx)
	rules[idx]
}
