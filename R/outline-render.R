render_outline <- function(found, depth) {
  selected <- seq_along(found$paths)
  if (!is.null(depth)) {
    selected <- selected[found$levels[selected] <= depth]
  }
  lines <- character(length(selected))
  if (length(selected) > 0L) {
    for (position in seq_along(selected)) {
      index <- selected[[position]]
      line <- paste0(found$paths[[index]], " ", found$counts[[index]])
      attributes <- found$attrs[[index]]
      if (length(attributes) > 0L) {
        order <- order_codepoints(attributes)
        rendered <- paste0("@", attributes[order])
        line <- paste0(line, " ", paste(rendered, collapse = " "))
      }
      lines[[position]] <- paste0(line, "\n")
    }
  }
  paste0(lines, collapse = "")
}
