context('unformat')

test_that('convert all data.frame columns to character', {
	df <- data.frame(
		type=c('A', 'E'),
		strike=c('12', '20'),
		spot=c(12.2, 19.8),
		series=c('ABC1', 'ABC2'),
		stringsAsFactors=FALSE
	)
	expect_true(all(sapply(unformat(df), class) == 'character'))
})


test_that('convert all data.frame factors to character', {
	df <- data.frame(
		type=c('A', 'E'),
		strike=c('12', '20'),
		spot=c(12.2, 19.8),
		series=c('ABC1', 'ABC2')
	)
	expect_equal(unname(sapply(unfactor(df), class)), c('character', 'character', 'numeric', 'character'))
})
