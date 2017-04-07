
to_date <- function(format = "%Y-%m-%d") {
  function(x) as.Date(strptime(x, format))
}

to_datetime <- function(format = "%Y-%m-%d %H:%M:%S") {
  function(x) as.POSIXct(strptime(x, format))
}

to_dbl <- function(dec = NULL, thousands = NULL, percent = FALSE) {
  .func <- identity
  .mult <- 1
  if (percent) {
    .func <- purrr::compose(function(x) sub("\\s*%", "", x), .func)
    .mult <- 0.01
  }
  if (!is.null(thousands))
    .func <- purrr::compose(function(x) gsub(thousands, "", x, fixed = TRUE), .func)
  if (!is.null(dec))
    .func <- purrr::compose(function(x) sub(dec, ".", x, fixed = TRUE), .func)
  function(x) as.numeric(.func(x)) * .mult
}

to_int <- function() {
  as.integer
}
