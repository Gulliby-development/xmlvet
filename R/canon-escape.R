escape_text <- function(text) {
  points <- utf8ToInt(text)
  pieces <- character(length(points))
  for (index in seq_along(points)) {
    point <- points[[index]]
    if (point == 38L) {
      pieces[[index]] <- "&amp;"
    } else if (point == 60L) {
      pieces[[index]] <- "&lt;"
    } else if (point == 62L) {
      pieces[[index]] <- "&gt;"
    } else if (point == 13L) {
      pieces[[index]] <- "&#xD;"
    } else {
      pieces[[index]] <- intToUtf8(point)
    }
  }
  paste0(pieces, collapse = "")
}

escape_attr <- function(text) {
  points <- utf8ToInt(text)
  pieces <- character(length(points))
  for (index in seq_along(points)) {
    point <- points[[index]]
    if (point == 38L) {
      pieces[[index]] <- "&amp;"
    } else if (point == 60L) {
      pieces[[index]] <- "&lt;"
    } else if (point == 34L) {
      pieces[[index]] <- "&quot;"
    } else if (point == 9L) {
      pieces[[index]] <- "&#x9;"
    } else if (point == 10L) {
      pieces[[index]] <- "&#xA;"
    } else if (point == 13L) {
      pieces[[index]] <- "&#xD;"
    } else {
      pieces[[index]] <- intToUtf8(point)
    }
  }
  paste0(pieces, collapse = "")
}
