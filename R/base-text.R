plain_text <- function(text) {
  points <- utf8ToInt(text)
  if (length(points) > 0L) {
    printable <- points >= 32L & points <= 126L
    points[!printable] <- 63L
  }
  intToUtf8(points)
}

show_text <- function(text) {
  paste0('"', plain_text(text), '"')
}

plural <- function(count, word) {
  suffix <- if (count == 1) "" else "s"
  paste0(count, " ", word, suffix)
}

show_value <- function(text) {
  points <- utf8ToInt(text)
  pieces <- character(length(points))
  for (index in seq_along(points)) {
    point <- points[[index]]
    if (point == 92L) {
      pieces[[index]] <- intToUtf8(c(92L, 92L))
    } else if (point == 10L) {
      pieces[[index]] <- intToUtf8(92L)
      pieces[[index]] <- paste0(pieces[[index]], "n")
    } else if (point == 9L) {
      pieces[[index]] <- intToUtf8(92L)
      pieces[[index]] <- paste0(pieces[[index]], "t")
    } else if (point == 13L) {
      pieces[[index]] <- intToUtf8(92L)
      pieces[[index]] <- paste0(pieces[[index]], "r")
    } else {
      pieces[[index]] <- intToUtf8(point)
    }
  }
  paste0(pieces, collapse = "")
}

quoted_value <- function(text) {
  paste0('"', show_value(text), '"')
}

order_codepoints <- function(text) {
  size <- length(text)
  code_points <- lapply(text, utf8ToInt)
  indices <- seq_len(size)
  comes_before <- function(left_index, right_index) {
    left <- code_points[[left_index]]
    right <- code_points[[right_index]]
    common <- min(length(left), length(right))
    position <- 1L
    decided <- FALSE
    result <- FALSE
    while (position <= common && !decided) {
      if (left[[position]] != right[[position]]) {
        result <- left[[position]] < right[[position]]
        decided <- TRUE
      }
      position <- position + 1L
    }
    if (!decided) {
      result <- length(left) < length(right)
    }
    result
  }
  if (size > 1L) {
    for (index in seq.int(2L, size)) {
      candidate <- index
      position <- index
      while (position > 1L) {
        previous <- indices[[position - 1L]]
        if (!comes_before(candidate, previous)) {
          break
        }
        indices[[position]] <- previous
        position <- position - 1L
      }
      indices[[position]] <- candidate
    }
  }
  indices
}
