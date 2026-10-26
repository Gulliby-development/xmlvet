run_diff <- function(args, flags, settings, inputs) {
  if (length(args) < 2L) {
    fail("missing FILE")
  }
  if (length(args) > 2L) {
    fail("too many arguments")
  }

  first_input <- inputs$read(args[[1L]])
  second_input <- inputs$read(args[[2L]])

  first_parsed <- parse_document(first_input$text)
  if (!first_parsed$ok) {
    fail(
      paste0(
        first_input$label,
        ":",
        first_parsed$line,
        ":",
        first_parsed$col,
        ": ",
        first_parsed$code,
        " ",
        first_parsed$message
      )
    )
  }
  second_parsed <- parse_document(second_input$text)
  if (!second_parsed$ok) {
    fail(
      paste0(
        second_input$label,
        ":",
        second_parsed$line,
        ":",
        second_parsed$col,
        ": ",
        second_parsed$code,
        " ",
        second_parsed$message
      )
    )
  }

  ignore_space <- "--ignore-space" %in% flags
  first <- list_document(first_parsed$doc, ignore_space)
  second <- list_document(second_parsed$doc, ignore_space)
  difference <- first_difference(first, second)

  if (is.null(difference)) {
    list(out = "same\n", code = 0L)
  } else {
    list(out = difference, code = 1L)
  }
}
