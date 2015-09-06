context('textparser')

test_that('it should create an integer textparser', {
	parser <- textparser(
		parse_integer=parser(
			'^\\d+$',
			function(text, match) {
				as.integer(text)
			}
		)
	)
	expect_true(is(parser, 'TextParser'))
	expect_equal(parser$parse('1'), 1)
	expect_equal(parser$parse('a'), 'a')
	expect_equal(parser$parse('1.1'), '1.1')
})

test_that('it should parse text with portuguese rules', {
	parser <- textparser(
		parse_numeric_ptBR=parser('^-? *\\d+,\\d+?$',
			function(text, match) {
				text <- str_replace(text, ',', '.')
				text <- str_replace_all(text, ' ', '')
				as.double(text)
			}
		),
		
		parse_numeric_with_thousands_ptBR=parser('^-?\\s*(\\d+\\.)+\\d+,\\d+?$',
			function(text, match) {
				text <- str_replace_all(text, '\\.', '')
				text <- str_replace(text, ',', '.')
				text <- str_replace_all(text, ' ', '')
				as.double(text)
			}
		),
		
		parse_date_ptBR=parser('^\\d{2}([/.-])\\d{2}([/.-])\\d{4}$', 
			function(text, match) {
				sep1 <- match[1,2]
				sep2 <- match[1,3]
				as.Date(text, format=paste0('%d', sep1, '%m', sep2, '%Y'))
			}
		)
	)
	expect_equal(parser$parse('1,1'), 1.1)
	expect_equal(parser$parse('1101,101'), 1101.101)
	expect_equal(parser$parse('-201,201'), -201.201)
	expect_equal(parser$parse('-  201,201'), -201.201)
	expect_equal(parser$parse('1.101,101'), 1101.101)
	expect_equal(parser$parse('12/07/1976'), as.Date('1976-07-12'))
})

test_that('transform to factor', {
	parser <- textparser(
		parse_factor=parser('^A|E$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		})
	)
	expect_true(is.factor(parser$parse(c('E', 'A', 'E'))))
	expect_equal(as.character(parser$parse('A')), 'American')
	expect_equal(as.character(parser$parse('E')), 'European')
})

test_that('transform a data.frame', {
	parser <- textparser(
		parse_factor=parser('^(A|E)$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		}),
		parse_integer=parser('^\\d+$', function(text, match) as.integer(text))
	)
	
	df <- data.frame(
		type=c('A', 'E'),
		strike=c('12', '20'),
		spot=c(12.2, 19.8),
		series=c('ABC1', 'ABC2'),
		stringsAsFactors=FALSE
	)
	df <- parser$parse(df)
	expect_equal(unname(sapply(df, class)), c('factor', 'integer', 'numeric', 'character'))
})

test_that('it should inherit parser', {
	parse <- textparser(
		parse_integer=parser('^\\d+$', function(text, match) as.integer(text))
	)
	
	expect_true(is.character(parse$parse('E')))
	expect_true(is.integer(parse$parse('10')))
	expect_true(parse$parse('10') == 10)
	
	parse <- textparser(
		parse_factor=parser('^A|E$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		}),
		parse
	)
	
	expect_true(is.factor(parse$parse('E')))
	expect_equal(as.character(parse$parse('E')), 'European')
	expect_true(is.integer(parse$parse('10')))
	expect_true(parse$parse('10') == 10)
})

test_that('it should parse values considering priority', {
	parser <- textparser(
		parse_integer=parser('\\d+', function(text, match) as.integer(text)),
		parse_ymd_date=parser('\\d{8}', function(text, match) as.Date(text, format='%Y%m%d'), priority(1))
	)
	expect_equal(parser$parse('1'), 1)
	expect_equal(parser$parse('20100101'), as.Date('2010-01-01'))
})

test_that('it should print textparser', {
	parser <- textparser(
		to_integer=parser('\\d+', function(text, match) as.integer(text)),
		to_ymd_date=parser('\\d{8}', function(text, match) {
			as.Date(text, format='%Y%m%d')
		}, priority(1))
	)
	cat('\n')
	print(parser)
})

test_that('it should apply parser to a data.frame', {
	df <- data.frame(
		type=c('A', 'E'),
		strike=c('12', '20'),
		spot=c(12.2, 19.8),
		series=c('ABC1', 'ABC2'),
		stringsAsFactors=FALSE
	)
	
	df <- textparser(df,
		parse_factor=parser('^(A|E)$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		}),
		parse_integer=parser('^\\d+$', function(text, match) as.integer(text))
	)
	
	expect_equal(unname(sapply(df, class)), c('factor', 'integer', 'numeric', 'character'))
})
