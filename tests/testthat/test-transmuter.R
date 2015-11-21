context('transmuter')

test_that('it should create a transmuter using Transmuter constructor', {
	trm <- Transmuter$new(envir=new.env(), rules=list())
	expect_is(trm, 'Transmuter')
	expect_equal(trm$transmute(1), 1)
	expect_equal(trm$transmute('1'), '1')
	# trm <- transmuter(
	# 	parse_integer=match_regex(
	# 		'^\\d+$',
	# 		function(text, match) {
	# 			as.integer(text)
	# 		}
	# 	),
	# 	format_date=match_type(
	# 		'Date',
	# 		function(x) format(x, format='%d %b %Y')
	# 	),
	# 	format_by_name=match_name(
	# 		'V1.2',
	# 		as.character,
	# 		use_regex=FALSE
	# 	),
	# format_by_content=match_function(
	# 	is.na,
	# 	function(x) 0,
	# 	priority(9)
	# )
	# )
	# expect_true(is(trm, 'transmuter'))
	# expect_equal(trm$transmute('1'), 1)
	# expect_equal(trm$transmute('a'), 'a')
	# expect_equal(trm$transmute('1.1'), '1.1')
})

test_that('it should create a transmuter with one rule', {
	trm <- Transmuter$new(
		envir=new.env(),
		rules=list(
			match_regex('^\\d+$', as.integer)
		)
	)
	expect_true(is(trm, 'Transmuter'))
	expect_equal(trm$transmute('1'), 1)
	expect_equal(trm$transmute('a'), 'a')
	expect_equal(trm$transmute('1.1'), '1.1')
})

test_that('it should create a transmuter with one rule using the constructor', {
	trm <- transmuter(
		match_regex('^\\d+$', as.integer)
	)
	expect_true(is(trm, 'Transmuter'))
	expect_equal(trm$transmute('1'), 1)
	expect_equal(trm$transmute('a'), 'a')
	expect_equal(trm$transmute('1.1'), '1.1')
})

test_that('it should transform a data.frame', {
	trm <- transmuter(
		match_regex('^(A|E)$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		}),
		match_regex('^\\d+$', as.integer)
	)

	df <- data.frame(
		type=c('A', 'E'),
		`strike price`=c('12', '20'),
		spot=c(12.2, 19.8),
		series=c('ABC1', 'ABC2'),
		stringsAsFactors=FALSE,
		check.names=FALSE
	)
	.names <- names(df)
	df <- trm$transmute(df)
	expect_equal(unname(sapply(df, class)), c('factor', 'integer', 'numeric', 'character'))
	expect_equal(names(df), .names)
})

test_that('it should parse values considering priority', {
	trm <- transmuter(
		match_regex('\\d+', as.integer),
		match_regex('\\d{8}', function(text, match) as.Date(text, format='%Y%m%d'), priority=1)
	)
	expect_equal(trm$transmute('1'), 1)
	expect_equal(trm$transmute('20100101'), as.Date('2010-01-01'))
})

test_that('it should inherit parser', {
	trm1 <- transmuter(
		match_regex('^\\d+$', function(text, match) as.integer(text))
	)

	expect_true(is.character(trm1$transmute('E')))
	expect_true(is.integer(trm1$transmute('10')))
	expect_true(trm1$transmute('10') == 10)

	trm2 <- transmuter(
		match_regex('^A|E$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		}),
		trm1
	)

	expect_true(is.factor(trm2$transmute('E')))
	expect_equal(as.character(trm2$transmute('E')), 'European')
	expect_true(is.integer(trm2$transmute('10')))
	expect_true(trm2$transmute('10') == 10)
})

test_that('it should parse sign', {
	trm <- transmuter(
		match_regex('\\+|-', function(text, match) {
			idx <- text == '-'
			x <- rep(1, length(text))
			x[idx] <- -1
			x
		})
	)
	expect_equal(trm$transmute('+'), 1)
	expect_equal(trm$transmute('-'), -1)
	expect_equal(trm$transmute(c('-', '+')), c(-1, 1))
})

test_that('it should create a transmuter with a match_class rule', {
	trm <- transmuter(
		match_class('Date', as.character)
	)
	expect_equal(trm$transmute('1'), '1')
	expect_equal(trm$transmute(as.Date('2015-11-21')), '2015-11-21')
})
