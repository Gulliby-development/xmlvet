parse_path <- function(text) {
  x <- utf8ToInt(text)
  n <- length(x)
  code_at <- function(index) {
    if (index >= 1L && index <= n) x[[index]] else NA_integer_
  }
  is_code <- function(index, expected) {
    !is.na(code_at(index)) && code_at(index) == expected
  }
  read_name <- function(index) {
    start <- index
    code <- code_at(index)
    if (!is.na(code) && is_name_start(code)) {
      index <- index + 1L
      code <- code_at(index)
      while (!is.na(code) && is_name_char(code)) {
        index <- index + 1L
        code <- code_at(index)
      }
      list(text = intToUtf8(x[seq.int(start, index - 1L)]), pos = index)
    } else {
      list(text = NULL, pos = index)
    }
  }

  if (n == 0L || x[[1L]] != 47L) {
    path_fault("a path starts with \"/\"", 1L)
  }

  steps <- list()
  pos <- 1L
  done <- FALSE
  while (!done) {
    if (!is_code(pos, 47L)) {
      path_fault("a step is missing", pos)
    }
    pos <- pos + 1L
    if (is_code(pos, 47L)) {
      axis <- "descendant"
      pos <- pos + 1L
    } else {
      axis <- "child"
    }

    code <- code_at(pos)
    if (is.na(code) || code == 47L) {
      path_fault("a step is missing", pos)
    }

    test <- NULL
    name <- NULL
    terminal <- FALSE
    if (code == 42L) {
      test <- "element"
      name <- "*"
      pos <- pos + 1L
    } else if (code == 64L) {
      name_info <- read_name(pos + 1L)
      if (is.null(name_info$text)) {
        path_fault("an attribute name is missing", pos + 1L)
      }
      test <- "attribute"
      name <- name_info$text
      pos <- name_info$pos
      terminal <- TRUE
    } else if (is_name_start(code)) {
      name_info <- read_name(pos)
      name <- name_info$text
      pos <- name_info$pos
      text_call <- name == "text" && is_code(pos, 40L)
      if (text_call && is_code(pos + 1L, 41L)) {
        test <- "text"
        name <- NULL
        pos <- pos + 2L
        terminal <- TRUE
      } else {
        test <- "element"
      }
    } else {
      path_fault("unexpected character", pos)
    }

    predicates <- list()
    if (terminal && is_code(pos, 91L)) {
      path_fault("a predicate is not allowed here", pos)
    }
    if (test == "element") {
      while (is_code(pos, 91L)) {
        bracket <- pos
        pos <- pos + 1L
        code <- code_at(pos)
        is_position <- !is.na(code) && code >= 49L && code <= 57L
        is_attribute <- !is.na(code) && code == 64L
        if (!is_position && !is_attribute) {
          path_fault("a position or an attribute test is missing", pos)
        }

        if (is_position) {
          digit_start <- pos
          digits <- 0L
          value <- 0L
          code <- code_at(pos)
          is_digit <- !is.na(code) && code >= 48L && code <= 57L
          while (is_digit) {
            digits <- digits + 1L
            if (digits > 6L) {
              path_fault("the position is too large", digit_start)
            }
            value <- value * 10L + code - 48L
            pos <- pos + 1L
            code <- code_at(pos)
            is_digit <- !is.na(code) && code >= 48L && code <= 57L
          }
          predicates[[length(predicates) + 1L]] <- list(
            type = "position",
            value = as.integer(value)
          )
          if (is.na(code_at(pos))) {
            path_fault("a predicate is not closed", bracket)
          }
          if (code_at(pos) != 93L) {
            path_fault("unexpected character", pos)
          }
          pos <- pos + 1L
        } else {
          pos <- pos + 1L
          name_info <- read_name(pos)
          if (is.null(name_info$text)) {
            path_fault("an attribute name is missing", pos)
          }
          predicate_name <- name_info$text
          pos <- name_info$pos
          code <- code_at(pos)
          if (is.na(code)) {
            path_fault("a predicate is not closed", bracket)
          }
          if (code == 93L) {
            predicates[[length(predicates) + 1L]] <- list(
              type = "has_attribute",
              name = predicate_name
            )
            pos <- pos + 1L
          } else if (code == 61L) {
            pos <- pos + 1L
            quote <- code_at(pos)
            if (is.na(quote) || (quote != 34L && quote != 39L)) {
              path_fault("a quoted value is missing", pos)
            }
            quote_pos <- pos
            pos <- pos + 1L
            value_start <- pos
            while (!is.na(code_at(pos)) && code_at(pos) != quote) {
              pos <- pos + 1L
            }
            if (is.na(code_at(pos))) {
              path_fault("a quoted value is not closed", quote_pos)
            }
            value_codes <- if (pos > value_start) {
              x[seq.int(value_start, pos - 1L)]
            } else {
              integer(0)
            }
            predicates[[length(predicates) + 1L]] <- list(
              type = "attribute_equals",
              name = predicate_name,
              value = intToUtf8(value_codes)
            )
            pos <- pos + 1L
            if (is.na(code_at(pos))) {
              path_fault("a predicate is not closed", bracket)
            }
            if (code_at(pos) != 93L) {
              path_fault("unexpected character", pos)
            }
            pos <- pos + 1L
          } else {
            path_fault("unexpected character", pos)
          }
        }
      }
    }

    if (terminal && pos <= n) {
      path_fault("nothing may follow a text() or attribute step", pos)
    }
    steps[[length(steps) + 1L]] <- list(
      axis = axis,
      test = test,
      name = name,
      predicates = predicates
    )

    if (pos > n) {
      done <- TRUE
    } else if (code_at(pos) != 47L) {
      path_fault("unexpected character", pos)
    }
  }
  steps
}
