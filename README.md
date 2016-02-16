![trans-mute](transmute.png "transmute")

transmute is an R package which transforms data based on a set of rules that match data against:

- regular expressions
- data class
- functions predicates applied to data

This package helps with transformations in huge datasets with many columns that have to be transformed.
This is a kind of *trasnform data in a bunch using a rule based framework*.

```{r}
library(transmute)
trm <- transmuter( # the transmuter drives the transformation
	match_regex('^(A|E)$', function (text, match) {
		factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
	}),
	match_class('numeric', as.integer),
	match_regex('^\\d+$', as.integer),
	match_regex('^\\d{8}$', function(text, match) as.Date(text, format="%Y%m%d"), priority=1),
	match_predicate(is.na, function(x, idx, ...) {
		x[idx] <- 0
		x
	}, priority=2)
)
df <- data.frame(
	type=c('A', 'E'),
	`strike price`=c('12', '20'),
	spot=c(12.2, 19.8),
	series=c('ABC1', 'ABC2'),
	maturity=c('20160229', '20160215'),
	trades=c(NA, 129),
	stringsAsFactors=FALSE,
	check.names=FALSE
)
df
#   type strike price spot series maturity trades
# 1    A           12 12.2   ABC1 20160229     NA
# 2    E           20 19.8   ABC2 20160215    129
str(df)
# 'data.frame':	2 obs. of  6 variables:
#  $ type        : chr  "A" "E"
#  $ strike price: chr  "12" "20"
#  $ spot        : num  12.2 19.8
#  $ series      : chr  "ABC1" "ABC2"
#  $ maturity    : chr  "20160229" "20160215"
#  $ trades      : num  NA 129
transmute(trm, df)
#       type strike price spot series   maturity trades
# 1 American           12   12   ABC1 2016-02-29      0
# 2 European           20   19   ABC2 2016-02-15    129
str(transmute(trm, df))
# 'data.frame':	2 obs. of  6 variables:
#  $ type        : Factor w/ 2 levels "American","European": 1 2
#  $ strike price: int  12 20
#  $ spot        : int  12 19
#  $ series      : chr  "ABC1" "ABC2"
#  $ maturity    : Date, format: "2016-02-29" "2016-02-15"
#  $ trades      : num  0 129
```