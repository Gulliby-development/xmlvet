make_doc <- function(text) {
  points <- utf8ToInt(text)
  if (length(points) > 0L && points[[1L]] == 65279L) {
    points <- points[-1L]
  }
  normalized <- integer(length(points))
  count <- 0L
  index <- 1L
  while (index <= length(points)) {
    point <- points[[index]]
    count <- count + 1L
    if (point == 13L) {
      normalized[[count]] <- 10L
      if (index < length(points) && points[[index + 1L]] == 10L) {
        index <- index + 1L
      }
    } else {
      normalized[[count]] <- point
    }
    index <- index + 1L
  }
  x <- normalized[seq_len(count)]
  n <- length(x)
  line <- integer(n + 1L)
  col <- integer(n + 1L)
  current_line <- 1L
  current_col <- 1L
  if (n > 0L) {
    for (index in seq_len(n)) {
      line[[index]] <- current_line
      col[[index]] <- current_col
      if (x[[index]] == 10L) {
        current_line <- current_line + 1L
        current_col <- 1L
      } else {
        current_col <- current_col + 1L
      }
    }
  }
  line[[n + 1L]] <- current_line
  col[[n + 1L]] <- current_col
  invalid <- which(!is_xml_char(x))
  first_bad <- if (length(invalid) > 0L) invalid[[1L]] else NA_integer_
  list(
    x = x,
    n = n,
    line = line,
    col = col,
    name_start = is_name_start(x),
    name_char = is_name_char(x),
    first_bad = first_bad
  )
}

doc_at <- function(doc, i) {
  index <- as.integer(i)
  outside <- is.na(index) || index < 1L || index > doc$n
  if (outside) -1L else doc$x[[index]]
}

doc_text <- function(doc, a, b) {
  if (b < a) {
    ""
  } else {
    start <- max(1L, a)
    end <- min(doc$n, b)
    if (start > end) {
      ""
    } else {
      intToUtf8(doc$x[seq.int(start, end)])
    }
  }
}

doc_starts <- function(doc, i, text) {
  points <- utf8ToInt(text)
  index <- as.integer(i)
  valid <- !is.na(index) && index >= 1L && index <= doc$n + 1L
  if (!valid) {
    FALSE
  } else if (length(points) == 0L) {
    TRUE
  } else {
    end <- index + length(points) - 1L
    if (end > doc$n) {
      FALSE
    } else {
      all(doc$x[seq.int(index, end)] == points)
    }
  }
}

doc_find <- function(doc, i, text) {
  points <- utf8ToInt(text)
  index <- as.integer(i)
  found <- NA_integer_
  if (!is.na(index) && index >= 1L && index <= doc$n + 1L) {
    if (length(points) == 0L) {
      found <- index
    } else {
      last <- doc$n - length(points) + 1L
      while (index <= last && is.na(found)) {
        if (doc_starts(doc, index, text)) {
          found <- index
        }
        index <- index + 1L
      }
    }
  }
  found
}

doc_word_at <- function(doc, i, word) {
  points <- utf8ToInt(word)
  index <- as.integer(i)
  valid <- !is.na(index) && index >= 1L && index <= doc$n + 1L
  if (!valid) {
    FALSE
  } else if (length(points) == 0L) {
    TRUE
  } else if (index + length(points) - 1L > doc$n) {
    FALSE
  } else {
    position <- 1L
    matches <- TRUE
    while (position <= length(points) && matches) {
      actual <- doc$x[[index + position - 1L]]
      expected <- points[[position]]
      if (actual >= 65L && actual <= 90L) {
        actual <- actual + 32L
      }
      if (expected >= 65L && expected <= 90L) {
        expected <- expected + 32L
      }
      matches <- actual == expected
      position <- position + 1L
    }
    matches
  }
}
