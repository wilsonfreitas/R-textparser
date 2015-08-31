

#' @export
textparser <- function(expr, parent=NULL) {
  if ( is.null(parent) )
    envir <- new.env()
  else
    envir <- environment(parent)$envir
  eval(substitute(eval(quote({ expr }))), envir)
  parser_names <- Filter(function(x) stringr::str_detect(x, '^parse'), ls(envir=envir))
  parsers <- lapply(parser_names, function(x) {
    get(x, envir=envir)
  })
  apply_parsers <- function(text) {
    if ( !is.character(text) )
      return(text)
    for (.parser in parsers) {
      result <- stringr::str_match(text, .parser[[1]])
      if ( ! is.na(result[1,1]) )
        return(.parser[[2]](text, result))
    }
    return(text)
  }
  function(x) {
    if ( is.data.frame(x) ) {
      as.data.frame(lapply(x, apply_parsers), stringsAsFactors=FALSE)
    } else {
      apply_parsers(x)
    }
  }
}
