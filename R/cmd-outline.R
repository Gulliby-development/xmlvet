run_outline <- function(args, flags, settings, inputs) {
  if (length(args) == 0L) {
    fail("missing FILE")
  }
  if (length(args) > 1L) {
    fail("too many arguments")
  }

  depth_text <- settings[["--depth"]]
  depth <- NULL
  if (!is.null(depth_text)) {
    points <- utf8ToInt(depth_text)
    valid_depth <- length(points) > 0L && length(points) <= 2L
    if (valid_depth) {
      first <- points[[1L]]
      valid_depth <- first >= 49L && first <= 57L
    }
    if (valid_depth && length(points) == 2L) {
      second <- points[[2L]]
      valid_depth <- second >= 48L && second <= 57L
    }
    if (!valid_depth) {
      fail(paste0(
        "invalid --depth ",
        show_text(depth_text),
        " (expected a whole number from 1 to 99)"
      ))
    }
    depth <- as.integer(depth_text)
  }

  input <- inputs$read(args[[1L]])
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
  found <- count_paths(parsed$doc$root)
  rendered <- render_outline(found, depth)
  list(out = rendered, code = 0)
}
