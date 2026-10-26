diff_text_job <- function(path, parts, ignore_space, text_index) {
  text <- paste0(parts, collapse = "")
  keep <- TRUE

  if (ignore_space) {
    points <- utf8ToInt(text)
    first <- 1L
    last <- length(points)
    while (first <= last && points[[first]] %in% c(32L, 9L, 10L)) {
      first <- first + 1L
    }
    while (last >= first && points[[last]] %in% c(32L, 9L, 10L)) {
      last <- last - 1L
    }
    keep <- first <= last
    if (keep) {
      text <- intToUtf8(points[seq.int(first, last)])
    }
  }

  job <- NULL
  if (keep) {
    text_index <- text_index + 1L
    line <- paste0(
      path,
      "/text()[",
      text_index,
      "]=",
      quoted_value(text)
    )
    job <- list(type = "line", line = line)
  }

  list(job = job, index = text_index)
}

list_document <- function(doc, ignore_space) {
  root <- doc$root
  root_path <- paste0("/", root$name, "[1]")
  stack <- list(list(type = "element", node = root, path = root_path))
  lines <- character()

  while (length(stack) > 0L) {
    stack_index <- length(stack)
    current <- stack[[stack_index]]
    stack[[stack_index]] <- NULL

    if (identical(current$type, "line")) {
      lines <- c(lines, current$line)
    } else {
      node <- current$node
      path <- current$path
      lines <- c(lines, path)

      if (length(node$attr_names) > 0L) {
        attr_order <- order_codepoints(node$attr_names)
        for (attr_index in attr_order) {
          attr_line <- paste0(
            path,
            "/@",
            node$attr_names[[attr_index]],
            "=",
            quoted_value(node$attr_values[[attr_index]])
          )
          lines <- c(lines, attr_line)
        }
      }

      child_jobs <- list()
      element_names <- character()
      element_counts <- integer()
      text_parts <- character()
      text_index <- 0L

      for (child_index in seq_along(node$kids)) {
        child <- node$kids[[child_index]]
        if (identical(child$type, "text")) {
          text_parts[[length(text_parts) + 1L]] <- child$value
        } else if (identical(child$type, "element")) {
          if (length(text_parts) > 0L) {
            flushed <- diff_text_job(
              path,
              text_parts,
              ignore_space,
              text_index
            )
            if (!is.null(flushed$job)) {
              child_jobs[[length(child_jobs) + 1L]] <- flushed$job
            }
            text_index <- flushed$index
            text_parts <- character()
          }

          name_index <- match(child$name, element_names)
          if (is.na(name_index)) {
            element_names <- c(element_names, child$name)
            element_counts <- c(element_counts, 1L)
            sibling_index <- 1L
          } else {
            element_counts[[name_index]] <- element_counts[[name_index]] + 1L
            sibling_index <- element_counts[[name_index]]
          }
          child_path <- paste0(
            path,
            "/",
            child$name,
            "[",
            sibling_index,
            "]"
          )
          child_jobs[[length(child_jobs) + 1L]] <- list(
            type = "element",
            node = child,
            path = child_path
          )
        }
      }

      if (length(text_parts) > 0L) {
        flushed <- diff_text_job(path, text_parts, ignore_space, text_index)
        if (!is.null(flushed$job)) {
          child_jobs[[length(child_jobs) + 1L]] <- flushed$job
        }
      }

      if (length(child_jobs) > 0L) {
        for (job_index in rev(seq_along(child_jobs))) {
          stack[[length(stack) + 1L]] <- child_jobs[[job_index]]
        }
      }
    }
  }

  lines
}
