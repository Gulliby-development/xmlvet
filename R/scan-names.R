skip_space <- function(doc, i) {
  index <- as.integer(i)
  while (
    index >= 1L &&
      index <= doc$n &&
      is_space(doc$x[[index]])
  ) {
    index <- index + 1L
  }
  index
}

read_name <- function(doc, i) {
  index <- as.integer(i)
  if (
    !is.na(index) &&
      index >= 1L &&
      index <= doc$n &&
      doc$name_start[[index]]
  ) {
    index <- index + 1L
    while (index <= doc$n && doc$name_char[[index]]) {
      index <- index + 1L
    }
  }
  index
}
