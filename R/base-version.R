# The Version field of the text of a DESCRIPTION file: the first line that
# starts with "Version:", without the blanks around the value.
description_version <- function(bytes) {
  text <- rawToChar(bytes)
  Encoding(text) <- "UTF-8"
  lines <- sub("\r$", "", strsplit(text, "\n", fixed = TRUE)[[1L]])
  found <- lines[startsWith(lines, "Version:")]
  if (length(found) == 0L) {
    fail("DESCRIPTION has no Version field")
  }
  value <- gsub("^[ \t]+|[ \t]+$", "", substring(found[[1L]], 9L))
  if (!grepl("^[0-9]+[.-][0-9]+([.-][0-9]+)*$", value)) {
    fail(paste0("invalid Version ", show_text(value)))
  }
  value
}

# The "--version" command: prints "xmlvet VERSION" and returns the exit code.
run_version <- function(io) {
  tryCatch(
    {
      reader <- io$read_description
      bytes <- if (is.function(reader)) {
        tryCatch(reader(), error = function(condition) NULL)
      }
      if (is.null(bytes)) {
        fail("cannot read DESCRIPTION")
      }
      io$out(paste0("xmlvet ", description_version(bytes), "\n"))
      0L
    },
    xmlvet_failure = function(problem) {
      io$err(paste0("xmlvet: ", conditionMessage(problem), "\n"))
      2L
    }
  )
}
