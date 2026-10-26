first_difference <- function(first, second) {
  difference <- NULL
  shared <- min(length(first), length(second))

  for (index in seq_len(shared)) {
    if (!identical(first[[index]], second[[index]])) {
      difference <- paste0(
        "- ",
        first[[index]],
        "\n+ ",
        second[[index]],
        "\n"
      )
      break
    }
  }

  if (is.null(difference) && length(first) != length(second)) {
    index <- shared + 1L
    first_line <- if (index > length(first)) "(end)" else first[[index]]
    second_line <- if (index > length(second)) "(end)" else second[[index]]
    difference <- paste0(
      "- ",
      first_line,
      "\n+ ",
      second_line,
      "\n"
    )
  }

  difference
}
