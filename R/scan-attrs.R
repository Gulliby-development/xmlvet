read_attr_value <- function(doc, q) {
  quote <- doc_at(doc, q)
  valid_quote <- quote == 34L || quote == 39L
  if (!valid_quote) {
    syntax_fault(q, "XV020", "malformed attribute")
  }

  pieces <- character()
  position <- q + 1L
  run_start <- position
  closed <- FALSE
  while (position <= doc$n && !closed) {
    point <- doc_at(doc, position)
    if (point == quote) {
      if (run_start < position) {
        chunk <- doc$x[seq.int(run_start, position - 1L)]
        chunk[chunk == 9L | chunk == 10L] <- 32L
        pieces <- c(pieces, intToUtf8(chunk))
      }
      closed <- TRUE
    } else if (point == 60L) {
      syntax_fault(
        position,
        "XV023",
        paste0(show_text("<"), " is not allowed in an attribute value")
      )
    } else if (point == 38L) {
      if (run_start < position) {
        chunk <- doc$x[seq.int(run_start, position - 1L)]
        chunk[chunk == 9L | chunk == 10L] <- 32L
        pieces <- c(pieces, intToUtf8(chunk))
      }
      reference <- read_reference(doc, position)
      pieces <- c(pieces, reference$text)
      position <- reference$pos
      run_start <- position
    } else {
      position <- position + 1L
    }
  }

  if (!closed) {
    syntax_fault(q, "XV020", "malformed attribute")
  }
  list(value = paste0(pieces, collapse = ""), pos = position + 1L)
}

read_start_tag <- function(doc, i, ord) {
  fault <- function(index) {
    syntax_fault(index, "XV015", "malformed start tag")
  }

  name_start <- i + 1L
  name_end <- read_name(doc, name_start)
  if (name_end == name_start) {
    fault(name_start)
  }
  element_name <- doc_text(doc, name_start, name_end - 1L)
  position <- name_end
  attr_names <- character()
  attr_values <- character()
  done <- FALSE
  empty <- FALSE

  while (!done) {
    next_position <- skip_space(doc, position)
    has_space <- next_position > position
    point <- doc_at(doc, next_position)
    valid_name_start <- FALSE
    if (next_position <= doc$n) {
      valid_name_start <- doc$name_start[[next_position]]
    }
    if (point == 62L) {
      position <- next_position + 1L
      done <- TRUE
    } else if (point == 47L && doc_at(doc, next_position + 1L) == 62L) {
      position <- next_position + 2L
      empty <- TRUE
      done <- TRUE
    } else if (!has_space || !valid_name_start) {
      fault(next_position)
    } else {
      attr_start <- next_position
      attr_end <- read_name(doc, attr_start)
      attr_name <- doc_text(doc, attr_start, attr_end - 1L)
      position <- skip_space(doc, attr_end)
      if (doc_at(doc, position) != 61L) {
        syntax_fault(position, "XV020", "malformed attribute")
      }
      position <- skip_space(doc, position + 1L)
      quote <- doc_at(doc, position)
      if (quote != 34L && quote != 39L) {
        syntax_fault(position, "XV020", "malformed attribute")
      }
      value <- read_attr_value(doc, position)
      duplicate <- any(vapply(
        attr_names,
        identical,
        logical(1L),
        attr_name
      ))
      if (duplicate) {
        syntax_fault(
          attr_start,
          "XV019",
          paste0("duplicate attribute ", show_text(attr_name))
        )
      }
      attr_names <- c(attr_names, attr_name)
      attr_values <- c(attr_values, value$value)
      position <- value$pos
    }
  }

  node <- new_element(
    element_name,
    attr_names,
    attr_values,
    list(),
    doc$line[[i]],
    doc$col[[i]],
    ord
  )
  list(node = node, pos = position, empty = empty)
}
