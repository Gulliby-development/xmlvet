run_get <- function(args, flags, settings, inputs) {
  if (length(args) == 0L) {
    fail("missing PATH")
  }
  if (length(args) == 1L) {
    fail("missing FILE")
  }
  if (length(args) > 2L) {
    fail("too many arguments")
  }

  path <- args[[1L]]
  steps <- tryCatch(
    parse_path(path),
    xmlvet_path = function(problem) {
      fail(paste0(
        "invalid path ",
        show_text(path),
        ": ",
        conditionMessage(problem),
        " at character ",
        problem$pos
      ))
    }
  )

  input <- inputs$read(args[[2L]])
  parsed <- parse_document(input$text)
  if (!parsed$ok) {
    fail(paste0(
      input$label,
      ":",
      parsed$line,
      ":",
      parsed$col,
      ": ",
      parsed$code,
      " ",
      parsed$message
    ))
  }

  values <- evaluate_path(parsed$doc$root, steps)
  if ("--count" %in% flags) {
    list(
      out = paste0(length(values), "\n"),
      code = if (length(values) == 0L) 1 else 0
    )
  } else if (length(values) == 0L) {
    list(out = "", code = 1)
  } else {
    rendered <- vapply(values, show_value, character(1))
    output <- paste0(rendered, "\n", collapse = "")
    list(out = output, code = 0)
  }
}
