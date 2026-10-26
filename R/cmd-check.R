run_check <- function(args, flags, settings, inputs) {
  if (length(args) == 0L) {
    fail("missing FILE")
  }

  read_inputs <- lapply(args, inputs$read)
  parsed_inputs <- lapply(read_inputs, function(input) {
    parse_document(input$text)
  })

  output <- ""
  has_problem <- FALSE
  quiet <- "quiet" %in% flags
  for (index in seq_along(read_inputs)) {
    input <- read_inputs[[index]]
    parsed <- parsed_inputs[[index]]
    if (parsed$ok) {
      count <- 0L
      stack <- list(parsed$doc$root)
      while (length(stack) > 0L) {
        node_index <- length(stack)
        node <- stack[[node_index]]
        stack[[node_index]] <- NULL
        if (identical(node$type, "element")) {
          count <- count + 1L
          kids <- node$kids
          if (length(kids) > 0L) {
            for (kid_index in seq_along(kids)) {
              kid <- kids[[kid_index]]
              if (identical(kid$type, "element")) {
                stack[[length(stack) + 1L]] <- kid
              }
            }
          }
        }
      }
      if (!quiet) {
        line <- paste0(
          input$label,
          ": ok, ",
          plural(count, "element"),
          "\n"
        )
        output <- paste0(output, line)
      }
    } else {
      has_problem <- TRUE
      line <- paste0(
        input$label,
        ":",
        parsed$line,
        ":",
        parsed$col,
        ": ",
        parsed$code,
        " ",
        parsed$message,
        "\n"
      )
      output <- paste0(output, line)
    }
  }

  code <- if (has_problem) 1L else 0L
  list(out = output, code = code)
}
