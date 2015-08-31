
context('textparser')

test_that('test textparser', {
	parse <- textparser({
		parse_integer <- list('^\\d+$', function(text, match) as.integer(text))
	})
	expect_equal(parse('1'), 1)
	expect_equal(parse('a'), 'a')
	expect_equal(parse('1.1'), '1.1')
})

test_that('portuguese rules', {
	parse <- textparser({
		parse_numeric_ptBR <- list('^-? *\\d+,\\d+?$',
			function(text, match) {
				text <- str_replace(text, ',', '.')
				text <- str_replace_all(text, ' ', '')
				as.double(text)
			}
		)
		
		parse_numeric_with_thousands_ptBR <- list('^-?\\s*(\\d+\\.)+\\d+,\\d+?$',
			function(text, match) {
				text <- str_replace_all(text, '\\.', '')
				text <- str_replace(text, ',', '.')
				text <- str_replace_all(text, ' ', '')
				as.double(text)
			}
		)
		
		parse_date_ptBR <- list('^\\d{2}([/.-])\\d{2}([/.-])\\d{4}$', 
			function(text, match) {
				sep1 <- match[1,2]
				sep2 <- match[1,3]
				as.Date(text, format=paste0('%d', sep1, '%m', sep2, '%Y'))
			}
		)
	})
	expect_equal(parse('1,1'), 1.1)
	expect_equal(parse('1101,101'), 1101.101)
	expect_equal(parse('-201,201'), -201.201)
	expect_equal(parse('-  201,201'), -201.201)
	expect_equal(parse('1.101,101'), 1101.101)
	expect_equal(parse('12/07/1976'), as.Date('1976-07-12'))
})

test_that('transform to factor', {
	parse <- textparser({
		parse_factor <- list('^A|E$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		})
	})
	expect_true(is.factor(parse(c('E', 'A', 'E'))))
	expect_equal(as.character(parse('A')), 'American')
	expect_equal(as.character(parse('E')), 'European')
})

test_that('transform a data.frame', {
	parse <- textparser({
		parse_factor <- list('^(A|E)$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		})
		parse_integer <- list('^\\d+$', function(text, match) as.integer(text))
	})
	
	df <- data.frame(
		type=c('A', 'E'),
		strike=c('12', '20'),
		spot=c(12.2, 19.8),
		series=c('ABC1', 'ABC2'),
		stringsAsFactors=FALSE
	)
	df <- parse(df)
	expect_equal(unname(sapply(df, class)), c('factor', 'integer', 'numeric', 'character'))
})

test_that('it should inherit parser', {
	parse <- textparser({
		parse_integer <- list('^\\d+$', function(text, match) as.integer(text))
	})
	
	expect_true(is.character(parse('E')))
	expect_true(is.integer(parse('10')))
	expect_true(parse('10') == 10)
	
	parse <- textparser({
		parse_factor <- list('^A|E$', function (text, match) {
			factor(text, levels=c('A', 'E'), labels=c('American', 'European'))
		})
	}, parser=parse)
	
	expect_true(is.factor(parse('E')))
	expect_equal(as.character(parse('E')), 'European')
	expect_true(is.integer(parse('10')))
	expect_true(parse('10') == 10)
})


