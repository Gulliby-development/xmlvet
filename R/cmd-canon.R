run_canon <- function(args, flags, settings, inputs) {
  if (length(args) == 0L) {
    fail("missing FILE")
  }
  if (length(args) > 1L) {
    fail("too many arguments")
  }

  input <- inputs$read(args[[1L]])
  parsed <- parse_document(input$text)
  if (!parsed$ok) {
    fail(paste0(
      input$label, ":", parsed$line, ":", parsed$col, ": ",
      parsed$code, " ", parsed$message
    ))
  }

  text <- canon_document(parsed$doc, "--comments" %in% flags)
  list(out = paste0(text, "\n"), code = 0)
}
