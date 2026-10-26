read_reference <- function(doc, i) {
  fault <- function() {
    syntax_fault(i, "XV021", "invalid reference")
  }
  if (doc_starts(doc, i, "&amp;")) {
    return(list(text = "&", pos = i + 5L))
  }
  if (doc_starts(doc, i, "&lt;")) {
    return(list(text = "<", pos = i + 4L))
  }
  if (doc_starts(doc, i, "&gt;")) {
    return(list(text = ">", pos = i + 4L))
  }
  if (doc_starts(doc, i, "&quot;")) {
    return(list(text = intToUtf8(34L), pos = i + 6L))
  }
  if (doc_starts(doc, i, "&apos;")) {
    return(list(text = intToUtf8(39L), pos = i + 6L))
  }
  if (!doc_starts(doc, i, "&#")) {
    fault()
  }

  position <- i + 2L
  hexadecimal <- doc_at(doc, position) == 120L
  if (hexadecimal) {
    position <- position + 1L
  }
  digits <- 0L
  significant <- 0L
  value <- 0
  while (TRUE) {
    point <- doc_at(doc, position)
    if (hexadecimal) {
      if (point >= 48L && point <= 57L) {
        digit <- point - 48L
      } else if (point >= 65L && point <= 70L) {
        digit <- point - 55L
      } else if (point >= 97L && point <= 102L) {
        digit <- point - 87L
      } else {
        break
      }
      radix <- 16
    } else {
      if (point < 48L || point > 57L) {
        break
      }
      digit <- point - 48L
      radix <- 10
    }
    digits <- digits + 1L
    if (significant > 0L || digit > 0L) {
      significant <- significant + 1L
      if (significant > 8L) {
        fault()
      }
      value <- value * radix + digit
    }
    position <- position + 1L
  }
  if (digits == 0L || doc_at(doc, position) != 59L) {
    fault()
  }
  if (value > 1114111 || !is_xml_char(as.integer(value))) {
    fault()
  }
  list(text = intToUtf8(as.integer(value)), pos = position + 1L)
}
