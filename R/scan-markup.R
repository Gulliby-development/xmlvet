markup_space_at <- function(doc, index) {
  index >= 1L && index <= doc$n && is_space(doc$x[[index]])
}

markup_ascii_equal <- function(text, expected) {
  actual <- utf8ToInt(text)
  target <- utf8ToInt(expected)
  if (length(actual) != length(target)) {
    FALSE
  } else {
    for (index in seq_along(actual)) {
      if (actual[[index]] >= 65L && actual[[index]] <= 90L) {
        actual[[index]] <- actual[[index]] + 32L
      }
      if (target[[index]] >= 65L && target[[index]] <= 90L) {
        target[[index]] <- target[[index]] + 32L
      }
    }
    all(actual == target)
  }
}

markup_version_value <- function(value) {
  points <- utf8ToInt(value)
  if (length(points) < 3L) {
    FALSE
  } else {
    points[[1L]] == 49L &&
      points[[2L]] == 46L &&
      all(points[seq.int(3L, length(points))] >= 48L) &&
      all(points[seq.int(3L, length(points))] <= 57L)
  }
}

markup_encoding_value <- function(value) {
  points <- utf8ToInt(value)
  if (length(points) == 0L) {
    FALSE
  } else {
    first <- points[[1L]]
    first_letter <- (first >= 65L && first <= 90L) ||
      (first >= 97L && first <= 122L)
    rest <- TRUE
    if (length(points) > 1L) {
      for (index in seq.int(2L, length(points))) {
        point <- points[[index]]
        letter <- (point >= 65L && point <= 90L) ||
          (point >= 97L && point <= 122L)
        digit <- point >= 48L && point <= 57L
        if (!(letter || digit || point %in% c(46L, 95L, 45L))) {
          rest <- FALSE
        }
      }
    }
    first_letter && rest
  }
}

read_comment <- function(doc, i) {
  content_start <- i + 4L
  marker <- doc_find(doc, content_start, "--")
  if (is.na(marker)) {
    syntax_fault(i, "XV013", "malformed comment")
  }
  if (doc_at(doc, marker + 2L) != 62L) {
    syntax_fault(marker, "XV013", "malformed comment")
  }
  list(text = doc_text(doc, content_start, marker - 1L), pos = marker + 3L)
}

read_cdata <- function(doc, i) {
  content_start <- i + 9L
  marker <- doc_find(doc, content_start, "]]>")
  if (is.na(marker)) {
    syntax_fault(i, "XV014", "unterminated CDATA section")
  }
  list(text = doc_text(doc, content_start, marker - 1L), pos = marker + 3L)
}

read_pi <- function(doc, i) {
  target_start <- i + 2L
  target_end <- read_name(doc, target_start)
  if (target_end == target_start) {
    syntax_fault(i, "XV012", "malformed processing instruction")
  }
  target <- doc_text(doc, target_start, target_end - 1L)
  if (markup_ascii_equal(target, "xml")) {
    if (target == "xml") {
      syntax_fault(i, "XV005", "misplaced XML declaration")
    }
    syntax_fault(
      i,
      "XV011",
      paste0("reserved processing instruction target ", show_text(target))
    )
  }
  if (doc_starts(doc, target_end, "?>")) {
    return(list(target = target, data = "", pos = target_end + 2L))
  }
  if (!markup_space_at(doc, target_end)) {
    syntax_fault(i, "XV012", "malformed processing instruction")
  }
  data_start <- skip_space(doc, target_end)
  marker <- doc_find(doc, data_start, "?>")
  if (is.na(marker)) {
    syntax_fault(i, "XV012", "malformed processing instruction")
  }
  list(
    target = target,
    data = doc_text(doc, data_start, marker - 1L),
    pos = marker + 2L
  )
}

read_declaration <- function(doc) {
  malformed <- function() {
    syntax_fault(1L, "XV006", "malformed XML declaration")
  }
  index <- 6L
  if (!markup_space_at(doc, index)) {
    malformed()
  }
  index <- skip_space(doc, index)
  name_end <- read_name(doc, index)
  if (name_end == index || doc_text(doc, index, name_end - 1L) != "version") {
    malformed()
  }
  index <- skip_space(doc, name_end)
  if (doc_at(doc, index) != 61L) {
    malformed()
  }
  index <- skip_space(doc, index + 1L)
  quote <- doc_at(doc, index)
  if (!(quote == 34L || quote == 39L)) {
    malformed()
  }
  value_start <- index + 1L
  value_end <- doc_find(doc, value_start, intToUtf8(quote))
  if (is.na(value_end)) {
    malformed()
  }
  version <- doc_text(doc, value_start, value_end - 1L)
  if (!markup_version_value(version)) {
    malformed()
  }
  if (version != "1.0") {
    syntax_fault(
      1L,
      "XV007",
      paste0("unsupported XML version ", show_text(version))
    )
  }
  index <- value_end + 1L
  seen_encoding <- FALSE
  seen_standalone <- FALSE
  repeat {
    if (doc_starts(doc, index, "?>")) {
      break
    }
    if (!markup_space_at(doc, index)) {
      malformed()
    }
    index <- skip_space(doc, index)
    if (doc_starts(doc, index, "?>")) {
      break
    }
    name_start <- index
    name_end <- read_name(doc, name_start)
    if (name_end == name_start) {
      malformed()
    }
    name <- doc_text(doc, name_start, name_end - 1L)
    if (name == "encoding") {
      if (seen_encoding || seen_standalone) {
        malformed()
      }
      seen_encoding <- TRUE
    } else if (name == "standalone") {
      if (seen_standalone) {
        malformed()
      }
      seen_standalone <- TRUE
    } else {
      malformed()
    }
    index <- skip_space(doc, name_end)
    if (doc_at(doc, index) != 61L) {
      malformed()
    }
    index <- skip_space(doc, index + 1L)
    quote <- doc_at(doc, index)
    if (!(quote == 34L || quote == 39L)) {
      malformed()
    }
    value_start <- index + 1L
    value_end <- doc_find(doc, value_start, intToUtf8(quote))
    if (is.na(value_end)) {
      malformed()
    }
    value <- doc_text(doc, value_start, value_end - 1L)
    if (name == "encoding") {
      if (!markup_encoding_value(value)) {
        malformed()
      }
      if (!markup_ascii_equal(value, "UTF-8")) {
        syntax_fault(
          1L,
          "XV008",
          paste0("unsupported encoding ", show_text(value))
        )
      }
    } else if (value != "yes" && value != "no") {
      malformed()
    }
    index <- value_end + 1L
  }
  index + 2L
}

read_doctype <- function(doc, i) {
  malformed <- function() {
    syntax_fault(i, "XV009", "malformed DOCTYPE")
  }
  internal_subset <- function(index) {
    syntax_fault(
      index,
      "XV010",
      "DOCTYPE internal subset is not supported"
    )
  }
  if (!doc_starts(doc, i, "<!DOCTYPE")) {
    malformed()
  }
  index <- i + 9L
  if (!markup_space_at(doc, index)) {
    malformed()
  }
  index <- skip_space(doc, index)
  name_end <- read_name(doc, index)
  if (name_end == index) {
    malformed()
  }
  index <- name_end
  separated <- markup_space_at(doc, index)
  if (separated) {
    index <- skip_space(doc, index)
  }
  if (doc_at(doc, index) == 91L) {
    internal_subset(index)
  }
  if (doc_at(doc, index) == 62L) {
    index + 1L
  } else {
    if (!separated) {
      malformed()
    }
    system_end <- read_name(doc, index)
    if (
      system_end == index ||
        doc_text(doc, index, system_end - 1L) != "SYSTEM"
    ) {
      malformed()
    }
    index <- system_end
    if (!markup_space_at(doc, index)) {
      malformed()
    }
    index <- skip_space(doc, index)
    quote <- doc_at(doc, index)
    if (!(quote == 34L || quote == 39L)) {
      malformed()
    }
    value_end <- doc_find(doc, index + 1L, intToUtf8(quote))
    if (is.na(value_end)) {
      malformed()
    }
    index <- skip_space(doc, value_end + 1L)
    if (doc_at(doc, index) == 91L) {
      internal_subset(index)
    }
    if (doc_at(doc, index) != 62L) {
      malformed()
    }
    index + 1L
  }
}
