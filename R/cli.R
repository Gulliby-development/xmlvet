usage_text <- function() {
  paste0(
    "usage: xmlvet check [--quiet] FILE...\n",
    "       xmlvet canon [--comments] FILE\n",
    "       xmlvet outline [--depth N] FILE\n",
    "       xmlvet get PATH FILE\n",
    "       xmlvet diff [--ignore-space] FILE FILE\n"
  )
}

main <- function(argv, io) {
  if (identical(argv, "--help")) {
    io$out(usage_text())
    code <- 0L
  } else if (identical(argv, "--version")) {
    code <- run_version(io)
  } else {
    result <- tryCatch(
      {
        if (length(argv) == 0L) {
          fail("missing command (check, canon, outline, get or diff)")
        }
        command <- argv[[1L]]
        commands <- c("check", "canon", "outline", "get", "diff")
        if (!(command %in% commands)) {
          fail(paste0("unknown command ", show_text(command)))
        }

        command_args <- argv[-1L]
        flags <- switch(command,
          check = "--quiet",
          canon = "--comments",
          diff = "--ignore-space",
          character()
        )
        valued <- if (identical(command, "outline")) {
          "--depth"
        } else {
          character()
        }
        parsed <- parse_options(command_args, flags, valued)
        inputs <- new_inputs(io)
        adapter <- switch(command,
          check = run_check,
          canon = run_canon,
          outline = run_outline,
          get = run_get,
          diff = run_diff
        )
        if (identical(command, "check")) {
          flags <- if ("--quiet" %in% parsed$flags) "quiet" else character()
        } else {
          flags <- parsed$flags
        }
        adapter(parsed$args, flags, parsed$settings, inputs)
      },
      xmlvet_failure = function(problem) {
        io$err(paste0("xmlvet: ", conditionMessage(problem), "\n"))
        NULL
      }
    )
    if (is.null(result)) {
      code <- 2L
    } else {
      if (nzchar(result$out)) {
        io$out(result$out)
      }
      code <- result$code
    }
  }
  code
}
