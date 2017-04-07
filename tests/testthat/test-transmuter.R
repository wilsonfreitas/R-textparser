context('transmuter')

test_that('it should create a transmuter using Transmuter constructor', {
	trm <- new('Transmuter', envir=new.env(), rules=list())
	expect_is(trm, 'Transmuter')
	expect_equal(transmute(trm, 1), 1)
	expect_equal(transmute(trm, '1'), '1')
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
	trm <- new('Transmuter',
		envir=new.env(),
		rules=list(
			match_regex('^\\d+$', as.integer)
		)
	)
	expect_true(is(trm, 'Transmuter'))
	expect_equal(transmute(trm, '1'), 1)
	expect_equal(transmute(trm, 'a'), 'a')
	expect_equal(transmute(trm, '1.1'), '1.1')
})

test_that('it should create a transmuter with one rule using the constructor', {
	trm <- transmuter(
		match_regex('^\\d+$', as.integer)
	)
	expect_true(is(trm, 'Transmuter'))
	expect_equal(transmute(trm, '1'), 1)
	expect_equal(transmute(trm, 'a'), 'a')
	expect_equal(transmute(trm, '1.1'), '1.1')
})

test_that('it should transform a data.frame', {
	trm <- transmuter(
		match_regex('^(A|E)$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		}),
		match_regex('^\\d+$', as.integer),
		match_regex('^\\d{8}$', function(text, match) as.Date(text, format="%Y%m%d"), priority=1)
	)

	df <- data.frame(
		type=c('A', 'E'),
		`strike price`=c('12', '20'),
		spot=c(12.2, 19.8),
		series=c('ABC1', 'ABC2'),
		maturity=c('20160229', '20160215'),
		stringsAsFactors=FALSE,
		check.names=FALSE
	)
	.names <- names(df)
	df <- transmute(trm, df)
	expect_equal(unname(sapply(df, class)), c('factor', 'integer', 'numeric', 'character', 'Date'))
	expect_equal(names(df), .names)
})

test_that('it should parse values considering priority', {
	trm <- transmuter(
		match_regex('\\d+', as.integer),
		match_regex('\\d{8}', function(text, match) as.Date(text, format='%Y%m%d'), priority=1)
	)
	expect_equal(transmute(trm, '1'), 1)
	expect_equal(transmute(trm, '20100101'), as.Date('2010-01-01'))
})

test_that('it should inherit parser', {
	trm1 <- transmuter(
		match_regex('^\\d+$', function(text, match) as.integer(text))
	)

	expect_true(is.character(transmute(trm1, 'E')))
	expect_true(is.integer(transmute(trm1, '10')))
	expect_true(transmute(trm1, '10') == 10)

	trm2 <- transmuter(
		match_regex('^A|E$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		}),
		trm1
	)

	expect_true(is.factor(transmute(trm2, 'E')))
	expect_equal(as.character(transmute(trm2, 'E')), 'European')
	expect_true(is.integer(transmute(trm2, '10')))
	expect_true(transmute(trm2, '10') == 10)
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
	expect_equal(transmute(trm, '+'), 1)
	expect_equal(transmute(trm, '-'), -1)
	expect_equal(transmute(trm, c('-', '+')), c(-1, 1))
})

test_that('it should create a transmuter with a match_class rule', {
	trm <- transmuter(
		match_class('Date', as.character)
	)
	expect_equal(transmute(trm, '1'), '1')
	expect_equal(transmute(trm, as.Date('2015-11-21')), '2015-11-21')
})


test_that('it should create a transmuter with a match_predicate rule', {
	trm <- transmuter(
		match_predicate(is.na, function(x, idx, ...) {
			x[idx] <- 0
			x
		})
	)
	expect_equal(transmute(trm, '1'), '1')
	expect_equal(transmute(trm, NA), 0)
	expect_equal(transmute(trm, c(NA, 1)), c(0, 1))

	df <- data.frame(
		var=c(NA, 1)
	)
	df <- transmute(trm, df)
	expect_equal(df$var, c(0, 1))
})

test_that('it should transmute data with transmute function', {
	trm <- transmuter(
		match_regex('^\\d+$', as.integer)
	)
	expect_equal(transmute(trm, '1'), 1)
	expect_equal(transmute(trm, 'a'), 'a')
	expect_equal(transmute(trm, '1.1'), '1.1')
})

test_that('it should transmute data with direct transmute_regex function', {
  expect_equal(transmute_regex('1', '^\\d+$', as.integer), 1)
  expect_is(transmute_regex('1', '^\\d+$', as.integer), 'integer')
  expect_equal(transmute_regex('a', '^\\d+$', as.integer), 'a')
  expect_equal(transmute_regex('1.1', '^\\d+$', as.integer), '1.1')
})
