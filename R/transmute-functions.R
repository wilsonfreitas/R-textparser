
composite <- function (...) {
  fs <- list(...)
  function(...) Reduce(function(x, f) f(x), fs, ...)
}

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
    .func <- composite(function(x) sub("\\s*%", "", x), .func)
    .mult <- 0.01
  }
  if (!is.null(dec))
    .func <- composite(function(x) sub(dec, ".", x, fixed = TRUE), .func)
  if (!is.null(thousands))
    .func <- composite(function(x) gsub(thousands, "", x, fixed = TRUE), .func)
  function(x) as.numeric(.func(x)) * .mult
}

to_int <- function() {
  as.integer
}

as_dbl <- function(x, dec = NULL, thousands = NULL, percent = FALSE) {
  .func <- to_dbl(dec, thousands, percent)
  .func(x)
}
