context('match_rules')

test_that('it should create a regex match_rule', {
  rule <- match_regex('\\d', as.integer)
  expect_is(rule, 'match_rule')
  expect_is(rule, 'regex_rule')
})

test_that('it should apply a regex_rule', {
  rule <- match_regex('^\\d+$', as.integer)

  result <- apply_rule(rule, '11')
  expect_is(result, 'rule_result')
  expect_true(result$applied)
  expect_equal(result$value, 11)

  result <- apply_rule(rule, 11)
  expect_false(result$applied) # regex_rule only handles character data

  result <- apply_rule(rule, 'a')
  expect_false(result$applied)

  result <- apply_rule(rule, '1.1')
  expect_false(result$applied)
})

test_that('it should apply a regex_rule with a group', {
  rule <- match_regex('delta (\\d+)', function(data, match) {
    as.integer(match[,2])
  })

  res <- apply_rule(rule, c('delta 50', 'delta 25'))
  expect_equal(res$value, c(50, 25))

  res <- apply_rule(rule, c('delta 50', NA, 'delta 25'))
  expect_equal(res$value, c(50, NA, 25))
})

test_that('it should apply a regex_rule if at least one element has been matched', {
  rule <- match_regex('\\d+', function(data, match) {
    as.integer(match[,1])
  }, apply_to='any')

  res <- apply_rule(rule, c('50', '25', 'W'))
  expect_equal(res$value, c(50, 25, NA))
})

test_that('it should apply a regex_rule if all elements match', {
  rule <- match_regex('\\d+', function(data, match) {
    as.integer(match[,1])
  }, apply_to='all')

  res <- apply_rule(rule, c('50', '25', 'W'))
  expect_false(res$applied)
})

test_that('it should apply a regex_rule if all elements match', {
  rule <- match_regex('\\d+', function(data, match) {
    as.integer(match[,1])
  }, apply_to='all')

  res <- apply_rule(rule, c('50', '25', 'W'))
  expect_false(res$applied)
})

test_that('it should iterate thru a list of rules ', {
  rules <- list(
    match_regex('NA', identity),
    match_regex('1', identity, priority=1),
    match_regex('2', identity, priority=2),
    match_regex('NA2', identity)
  )

  rules <- iter_rules(rules)
  expect_equal(take(rules, 'regex'), c('1', '2', 'NA', 'NA2'))
})

test_that('it should create a check class_rule', {
  rule <- match_class('Date', as.character)
  expect_is(rule, 'match_rule')
  expect_is(rule, 'class_rule')
})

test_that('it should apply a class_rule', {
  rule <- match_class('Date', as.character)

  result <- apply_rule(rule, '11')
  expect_is(result, 'rule_result')
  expect_false(result$applied)

  result <- apply_rule(rule, as.Date('2015-11-21'))
  expect_true(result$applied) # regex_rule only handles character data
  expect_equal(result$value, '2015-11-21')
})
