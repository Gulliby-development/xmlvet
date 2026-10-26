fail <- function(message) {
  condition <- structure(
    list(message = message, call = NULL),
    class = c("xmlvet_failure", "error", "condition")
  )
  stop(condition)
}

syntax_fault <- function(index, code, message) {
  condition <- structure(
    list(message = message, call = NULL, index = index, code = code),
    class = c("xmlvet_syntax", "error", "condition")
  )
  stop(condition)
}

path_fault <- function(reason, pos) {
  condition <- structure(
    list(message = reason, call = NULL, pos = pos),
    class = c("xmlvet_path", "error", "condition")
  )
  stop(condition)
}
