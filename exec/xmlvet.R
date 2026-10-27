#!/usr/bin/env Rscript
arguments <- commandArgs()
script_arg <- arguments[startsWith(arguments, "--file=")]
script_path <- normalizePath(substring(script_arg[[1L]], 8L))
project_dir <- dirname(dirname(script_path))
source_dir <- file.path(project_dir, "R")
sources <- list.files(source_dir, pattern = "[.]R$", full.names = TRUE)
sources <- sources[order(basename(sources), method = "radix")]
for (source_path in sources) {
  source(source_path)
}

read_bytes <- function(path) {
  connection <- suppressWarnings(file(path, "rb"))
  on.exit(suppressWarnings(close(connection)))
  suppressWarnings(open(connection))
  chunks <- list()
  repeat {
    chunk <- suppressWarnings(
      readBin(connection, what = "raw", n = 65536L)
    )
    if (length(chunk) == 0L) {
      break
    }
    chunks[[length(chunks) + 1L]] <- chunk
  }
  if (length(chunks) == 0L) {
    raw()
  } else {
    do.call(c, chunks)
  }
}

io <- list(
  read_stdin = function() read_bytes("stdin"),
  read_file = function(path) read_bytes(path),
  read_description = function() {
    read_bytes(file.path(project_dir, "DESCRIPTION"))
  },
  out = function(text) {
    writeLines(text, stdout(), sep = "", useBytes = TRUE)
  },
  err = function(text) {
    writeLines(text, stderr(), sep = "", useBytes = TRUE)
  }
)
quit(
  save = "no",
  status = main(commandArgs(trailingOnly = TRUE), io)
)
