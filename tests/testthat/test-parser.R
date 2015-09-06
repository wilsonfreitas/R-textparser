context('parser')

test_that('it should instantiate a parser', {
	p <- parser('.*', identity, priority(1))
	expect_is(p, 'parser')
	expect_equal(p$priority, 1)
})