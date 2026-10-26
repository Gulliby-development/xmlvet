new_element <- function(name, attr_names, attr_values, kids, line, col, ord) {
  list(
    type = "element",
    name = name,
    attr_names = attr_names,
    attr_values = attr_values,
    kids = kids,
    line = line,
    col = col,
    ord = ord
  )
}

new_text <- function(value, ord) {
  list(type = "text", value = value, ord = ord)
}

new_comment <- function(value) {
  list(type = "comment", value = value)
}

new_pi <- function(target, data) {
  list(type = "pi", target = target, data = data)
}

append_text <- function(kids, value, ord) {
  if (identical(value, "")) {
    kids
  } else {
    n <- length(kids)
    if (n > 0L && identical(kids[[n]]$type, "text")) {
      kids[[n]]$value <- paste0(kids[[n]]$value, value)
    } else {
      kids[[n + 1L]] <- new_text(value, ord)
    }
    kids
  }
}

string_value <- function(node) {
  stack <- list(node)
  values <- character()

  while (length(stack) > 0L) {
    n <- length(stack)
    current <- stack[[n]]
    stack[[n]] <- NULL

    if (identical(current$type, "text")) {
      values[[length(values) + 1L]] <- current$value
    } else if (identical(current$type, "element")) {
      kids <- current$kids
      if (length(kids) > 0L) {
        for (i in rev(seq_along(kids))) {
          stack[[length(stack) + 1L]] <- kids[[i]]
        }
      }
    }
  }

  paste0(values, collapse = "")
}
