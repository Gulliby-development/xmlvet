parse_options <- function(argv, flags, valued) {
  parsed_flags <- character()
  settings <- list()
  args <- character()
  argument_count <- length(argv)
  index <- 1L
  while (index <= argument_count) {
    argument <- argv[[index]]
    if (startsWith(argument, "--")) {
      points <- utf8ToInt(argument)
      equal_position <- match(61L, points)
      if (is.na(equal_position)) {
        option_name <- argument
      } else {
        option_name <- intToUtf8(points[seq_len(equal_position - 1L)])
      }
      if (option_name %in% valued) {
        if (!is.na(equal_position)) {
          if (equal_position == length(points)) {
            value <- ""
          } else {
            value <- intToUtf8(
              points[seq.int(equal_position + 1L, length(points))]
            )
          }
        } else if (index < argument_count) {
          index <- index + 1L
          value <- argv[[index]]
        } else {
          fail(paste0(plain_text(option_name), " needs a value"))
        }
        settings[[option_name]] <- value
      } else if (argument %in% flags) {
        if (!(argument %in% parsed_flags)) {
          parsed_flags <- c(parsed_flags, argument)
        }
      } else {
        fail(paste0("unknown option ", show_text(argument)))
      }
    } else {
      args <- c(args, argument)
    }
    index <- index + 1L
  }
  list(flags = parsed_flags, settings = settings, args = args)
}
