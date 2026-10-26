canon_node <- function(node, comments) {
  stack <- list(node)
  fragments <- character()

  while (length(stack) > 0L) {
    last <- length(stack)
    current <- stack[[last]]
    stack[[last]] <- NULL

    if (identical(current$type, "canon_close")) {
      fragments[[length(fragments) + 1L]] <- paste0(
        "</", current$name, ">"
      )
    } else if (identical(current$type, "element")) {
      fragments[[length(fragments) + 1L]] <- paste0("<", current$name)
      attr_names <- current$attr_names
      attr_values <- current$attr_values
      attr_order <- order_codepoints(attr_names)
      for (index in attr_order) {
        fragments[[length(fragments) + 1L]] <- paste0(
          " ", attr_names[[index]], "=\"",
          escape_attr(attr_values[[index]]), "\""
        )
      }
      fragments[[length(fragments) + 1L]] <- ">"
      stack[[length(stack) + 1L]] <- list(
        type = "canon_close",
        name = current$name
      )
      kids <- current$kids
      if (length(kids) > 0L) {
        for (index in rev(seq_along(kids))) {
          stack[[length(stack) + 1L]] <- kids[[index]]
        }
      }
    } else if (identical(current$type, "text")) {
      fragments[[length(fragments) + 1L]] <- escape_text(current$value)
    } else if (identical(current$type, "comment")) {
      if (comments) {
        fragments[[length(fragments) + 1L]] <- paste0(
          "<!--", current$value, "-->"
        )
      }
    } else if (identical(current$type, "pi")) {
      if (identical(current$data, "")) {
        fragments[[length(fragments) + 1L]] <- paste0(
          "<?", current$target, "?>"
        )
      } else {
        fragments[[length(fragments) + 1L]] <- paste0(
          "<?", current$target, " ", current$data, "?>"
        )
      }
    }
  }

  paste0(fragments, collapse = "")
}

canon_document <- function(doc, comments) {
  items <- character()
  prolog <- doc$prolog
  for (index in seq_along(prolog)) {
    item <- prolog[[index]]
    if (comments || !identical(item$type, "comment")) {
      items[[length(items) + 1L]] <- canon_node(item, comments)
    }
  }
  items[[length(items) + 1L]] <- canon_node(doc$root, comments)

  epilog <- doc$epilog
  for (index in seq_along(epilog)) {
    item <- epilog[[index]]
    if (comments || !identical(item$type, "comment")) {
      items[[length(items) + 1L]] <- canon_node(item, comments)
    }
  }

  paste0(items, collapse = intToUtf8(10L))
}
