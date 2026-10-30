test_that("main handles help and command diagnostics", {
  expected_usage <- paste0(
    "usage: xmlvet check [--quiet] FILE...\n",
    "       xmlvet canon [--comments] FILE\n",
    "       xmlvet outline [--depth N] FILE\n",
    "       xmlvet get [--count] PATH FILE\n",
    "       xmlvet diff [--ignore-space] FILE FILE\n"
  )
  expect_identical(usage_text(), expected_usage)

  state <- new.env(parent = emptyenv())
  state$out_calls <- 0L
  state$out_text <- ""
  state$err_calls <- 0L
  state$err_text <- ""
  io <- make_io()
  io$out <- function(text) {
    state$out_calls <- state$out_calls + 1L
    state$out_text <- paste0(state$out_text, text)
  }
  io$err <- function(text) {
    state$err_calls <- state$err_calls + 1L
    state$err_text <- paste0(state$err_text, text)
  }

  expect_identical(as.integer(main("--help", io)), 0L)
  expect_identical(state$out_calls, 1L)
  expect_identical(state$out_text, expected_usage)
  expect_identical(state$err_calls, 0L)
  expect_identical(state$err_text, "")

  cases <- list(
    list(
      argv = character(),
      error = "missing command (check, canon, outline, get or diff)"
    ),
    list(argv = "bogus", error = "unknown command \"bogus\""),
    list(
      argv = c("--help", "x"),
      error = "unknown command \"--help\""
    )
  )
  for (case in cases) {
    result <- run_main(case$argv)
    expect_identical(as.integer(result$code), 2L)
    expect_identical(result$out, "")
    expect_identical(result$err, paste0("xmlvet: ", case$error, "\n"))
  }
})

test_that("main validates options before arity and input", {
  cases <- list(
    list(
      argv = c("bogus", "--bad"),
      error = "unknown command \"bogus\""
    ),
    list(
      argv = c("check", "--Quiet", "x"),
      error = "unknown option \"--Quiet\""
    ),
    list(argv = c("canon", "--"), error = "unknown option \"--\""),
    list(
      argv = c("get", "--help"),
      error = "unknown option \"--help\""
    ),
    list(
      argv = c("outline", "--depth"),
      error = "--depth needs a value"
    ),
    list(
      argv = c("outline", "--depth", "--bad", "x"),
      error = paste0(
        "invalid --depth \"--bad\" ",
        "(expected a whole number from 1 to 99)"
      )
    ),
    list(
      argv = c("check", "--quiet=1"),
      error = "unknown option \"--quiet=1\""
    )
  )

  state <- new.env(parent = emptyenv())
  state$reads <- character()
  state$out_calls <- 0L
  state$out_text <- ""
  state$err_calls <- 0L
  state$err_text <- ""
  io <- make_io()
  io$read_file <- function(path) {
    state$reads <- c(state$reads, path)
    stop("unexpected read")
  }
  io$read_stdin <- function() {
    state$reads <- c(state$reads, "stdin")
    stop("unexpected read")
  }
  io$out <- function(text) {
    state$out_calls <- state$out_calls + 1L
    state$out_text <- paste0(state$out_text, text)
  }
  io$err <- function(text) {
    state$err_calls <- state$err_calls + 1L
    state$err_text <- paste0(state$err_text, text)
  }

  for (case in cases) {
    state$reads <- character()
    state$out_calls <- 0L
    state$out_text <- ""
    state$err_calls <- 0L
    state$err_text <- ""

    expect_identical(as.integer(main(case$argv, io)), 2L)
    expect_identical(state$out_calls, 0L)
    expect_identical(state$out_text, "")
    expect_identical(state$err_calls, 1L)
    expect_identical(
      state$err_text,
      paste0("xmlvet: ", case$error, "\n")
    )
    expect_identical(state$reads, character())
  }
})

test_that("main validates arity and values before reading", {
  cases <- list(
    list(argv = "check", error = "missing FILE"),
    list(argv = c("canon"), error = "missing FILE"),
    list(argv = c("outline"), error = "missing FILE"),
    list(argv = c("get", "/a"), error = "missing FILE"),
    list(argv = c("get"), error = "missing PATH"),
    list(argv = c("diff", "a.xml"), error = "missing FILE"),
    list(
      argv = c("canon", "a", "b"),
      error = "too many arguments"
    ),
    list(
      argv = c("outline", "--depth", "0", "a", "b"),
      error = "too many arguments"
    ),
    list(
      argv = c("get", "/a[", "a", "b"),
      error = "too many arguments"
    ),
    list(
      argv = c("diff", "a", "b", "c"),
      error = "too many arguments"
    ),
    list(
      argv = c("outline", "--depth", "0", "missing.xml"),
      error = paste0(
        "invalid --depth \"0\" ",
        "(expected a whole number from 1 to 99)"
      )
    ),
    list(
      argv = c("get", "/a[", "missing.xml"),
      error = paste0(
        "invalid path \"/a[\": ",
        "a position or an attribute test is missing at character 4"
      )
    )
  )

  state <- new.env(parent = emptyenv())
  state$reads <- character()
  state$out_calls <- 0L
  state$out_text <- ""
  state$err_calls <- 0L
  state$err_text <- ""
  io <- make_io()
  io$read_file <- function(path) {
    state$reads <- c(state$reads, path)
    stop("unexpected read")
  }
  io$read_stdin <- function() {
    state$reads <- c(state$reads, "stdin")
    stop("unexpected read")
  }
  io$out <- function(text) {
    state$out_calls <- state$out_calls + 1L
    state$out_text <- paste0(state$out_text, text)
  }
  io$err <- function(text) {
    state$err_calls <- state$err_calls + 1L
    state$err_text <- paste0(state$err_text, text)
  }

  for (case in cases) {
    state$reads <- character()
    state$out_calls <- 0L
    state$out_text <- ""
    state$err_calls <- 0L
    state$err_text <- ""

    expect_identical(as.integer(main(case$argv, io)), 2L)
    expect_identical(state$out_calls, 0L)
    expect_identical(state$out_text, "")
    expect_identical(state$err_calls, 1L)
    expect_identical(
      state$err_text,
      paste0("xmlvet: ", case$error, "\n")
    )
    expect_identical(state$reads, character())
  }
})

test_that("main check output and quiet flag are atomic", {
  bad <- "<a>\n  <b>\n</a>"
  cases <- list(
    list(
      argv = c("check", "small.xml"),
      files = list("small.xml" = "<a x=\"1\"><b>t</b><b/></a>"),
      out = "small.xml: ok, 3 elements\n",
      code = 0L,
      reads = "small.xml"
    ),
    list(
      argv = c("check", "bad.xml", "small.xml"),
      files = list(
        "bad.xml" = bad,
        "small.xml" = "<a x=\"1\"><b>t</b><b/></a>"
      ),
      out = paste0(
        "bad.xml:3:1: XV017 end tag \"a\" does not match start tag ",
        "\"b\" opened at 2:3\n",
        "small.xml: ok, 3 elements\n"
      ),
      code = 1L,
      reads = c("bad.xml", "small.xml")
    ),
    list(
      argv = c("check", "--quiet", "--quiet", "small.xml"),
      files = list("small.xml" = "<a x=\"1\"><b>t</b><b/></a>"),
      out = "",
      code = 0L,
      reads = "small.xml"
    ),
    list(
      argv = c("check", "--quiet", "bad.xml", "small.xml"),
      files = list(
        "bad.xml" = bad,
        "small.xml" = "<a x=\"1\"><b>t</b><b/></a>"
      ),
      out = paste0(
        "bad.xml:3:1: XV017 end tag \"a\" does not match start tag ",
        "\"b\" opened at 2:3\n"
      ),
      code = 1L,
      reads = c("bad.xml", "small.xml")
    )
  )

  for (case in cases) {
    state <- new.env(parent = emptyenv())
    state$reads <- character()
    state$out_calls <- 0L
    state$out_text <- ""
    state$err_calls <- 0L
    state$err_text <- ""
    io <- make_io(case$files)
    read_file <- io$read_file
    io$read_file <- function(path) {
      state$reads <- c(state$reads, path)
      read_file(path)
    }
    io$out <- function(text) {
      state$out_calls <- state$out_calls + 1L
      state$out_text <- paste0(state$out_text, text)
    }
    io$err <- function(text) {
      state$err_calls <- state$err_calls + 1L
      state$err_text <- paste0(state$err_text, text)
    }

    expect_identical(as.integer(main(case$argv, io)), case$code)
    expect_identical(state$out_text, case$out)
    expect_identical(state$out_calls, if (nzchar(case$out)) 1L else 0L)
    expect_identical(state$err_calls, 0L)
    expect_identical(state$err_text, "")
    expect_identical(state$reads, case$reads)
  }
})

test_that("main canon command emits canonical output once", {
  cases <- list(
    list(
      argv = c("canon", "a.xml"),
      out = "<a b=\"2\" z=\"1\"><b>t</b><b></b></a>\n"
    ),
    list(
      argv = c("canon", "--comments", "a.xml"),
      out = "<!--c-->\n<a b=\"2\" z=\"1\"><b>t</b><b></b></a>\n"
    )
  )
  files <- list(
    "a.xml" = "<!--c--><a z=\"1\" b=\"2\"><b>t</b><b/></a>"
  )

  for (case in cases) {
    state <- new.env(parent = emptyenv())
    state$out_calls <- 0L
    state$out_text <- ""
    state$err_calls <- 0L
    state$err_text <- ""
    io <- make_io(files)
    io$out <- function(text) {
      state$out_calls <- state$out_calls + 1L
      state$out_text <- paste0(state$out_text, text)
    }
    io$err <- function(text) {
      state$err_calls <- state$err_calls + 1L
      state$err_text <- paste0(state$err_text, text)
    }

    expect_identical(as.integer(main(case$argv, io)), 0L)
    expect_identical(state$out_calls, 1L)
    expect_identical(state$out_text, case$out)
    expect_identical(state$err_calls, 0L)
    expect_identical(state$err_text, "")
  }
})

test_that("main outline honors depth and the last repeated value", {
  cases <- list(
    list(
      argv = c("outline", "a.xml"),
      out = "/a 1 @b @z\n/a/b 2\n"
    ),
    list(
      argv = c(
        "outline", "--depth=2", "--depth", "1", "a.xml"
      ),
      out = "/a 1 @b @z\n"
    )
  )
  files <- list(
    "a.xml" = "<!--c--><a z=\"1\" b=\"2\"><b>t</b><b/></a>"
  )

  for (case in cases) {
    state <- new.env(parent = emptyenv())
    state$out_calls <- 0L
    state$out_text <- ""
    state$err_calls <- 0L
    state$err_text <- ""
    io <- make_io(files)
    io$out <- function(text) {
      state$out_calls <- state$out_calls + 1L
      state$out_text <- paste0(state$out_text, text)
    }
    io$err <- function(text) {
      state$err_calls <- state$err_calls + 1L
      state$err_text <- paste0(state$err_text, text)
    }

    expect_identical(as.integer(main(case$argv, io)), 0L)
    expect_identical(state$out_calls, 1L)
    expect_identical(state$out_text, case$out)
    expect_identical(state$err_calls, 0L)
    expect_identical(state$err_text, "")
  }
})

test_that("main get preserves empty values and escapes control values", {
  files <- list(
    "a.xml" = "<a z=\"1\" b=\"2\"><b>t</b><b/></a>",
    "controls.xml" = "<a>x&#9;y&#10;z&#13;w</a>"
  )
  cases <- list(
    list(
      argv = c("get", "/a/b", "a.xml"),
      out = "t\n\n",
      code = 0L
    ),
    list(
      argv = c("get", "/a/c", "a.xml"),
      out = "",
      code = 1L
    ),
    list(
      argv = c("get", "/a", "controls.xml"),
      out = "x\\ty\\nz\\rw\n",
      code = 0L
    )
  )

  for (case in cases) {
    state <- new.env(parent = emptyenv())
    state$out_calls <- 0L
    state$out_text <- ""
    state$err_calls <- 0L
    state$err_text <- ""
    io <- make_io(files)
    io$out <- function(text) {
      state$out_calls <- state$out_calls + 1L
      state$out_text <- paste0(state$out_text, text)
    }
    io$err <- function(text) {
      state$err_calls <- state$err_calls + 1L
      state$err_text <- paste0(state$err_text, text)
    }

    expect_identical(as.integer(main(case$argv, io)), case$code)
    expect_identical(state$out_text, case$out)
    expect_identical(state$out_calls, if (nzchar(case$out)) 1L else 0L)
    expect_identical(state$err_calls, 0L)
    expect_identical(state$err_text, "")
  }
})

test_that("main diff reports equality, differences, and ignored space", {
  cases <- list(
    list(
      argv = c("diff", "a.xml", "a.xml"),
      files = list("a.xml" = "<a z=\"1\" b=\"2\"><b>t</b><b/></a>"),
      out = "same\n",
      code = 0L,
      reads = c("a.xml", "a.xml")
    ),
    list(
      argv = c("diff", "a.xml", "other.xml"),
      files = list(
        "a.xml" = "<a z=\"1\" b=\"2\"><b>t</b><b/></a>",
        "other.xml" = "<a z=\"2\" b=\"2\"><b>t</b><b/></a>"
      ),
      out = "- /a[1]/@z=\"1\"\n+ /a[1]/@z=\"2\"\n",
      code = 1L,
      reads = c("a.xml", "other.xml")
    ),
    list(
      argv = c("diff", "--ignore-space", "a.xml", "other.xml"),
      files = list(
        "a.xml" = "<a> x </a>",
        "other.xml" = "<a>x</a>"
      ),
      out = "same\n",
      code = 0L,
      reads = c("a.xml", "other.xml")
    )
  )

  for (case in cases) {
    state <- new.env(parent = emptyenv())
    state$reads <- character()
    state$out_calls <- 0L
    state$out_text <- ""
    state$err_calls <- 0L
    state$err_text <- ""
    io <- make_io(case$files)
    read_file <- io$read_file
    io$read_file <- function(path) {
      state$reads <- c(state$reads, path)
      read_file(path)
    }
    io$out <- function(text) {
      state$out_calls <- state$out_calls + 1L
      state$out_text <- paste0(state$out_text, text)
    }
    io$err <- function(text) {
      state$err_calls <- state$err_calls + 1L
      state$err_text <- paste0(state$err_text, text)
    }

    expect_identical(as.integer(main(case$argv, io)), case$code)
    expect_identical(state$out_calls, 1L)
    expect_identical(state$out_text, case$out)
    expect_identical(state$err_calls, 0L)
    expect_identical(state$err_text, "")
    expect_identical(state$reads, case$reads)
  }
})

test_that("main failures are atomic and reads preserve argument order", {
  bad <- "<a>\n  <b>\n</a>"
  problem <- paste0(
    "bad.xml:3:1: XV017 end tag \"a\" does not match start tag ",
    "\"b\" opened at 2:3"
  )
  cases <- list(
    list(
      argv = c("check", "bad.xml", "missing.xml"),
      files = list("bad.xml" = bad),
      stdin = NULL,
      reads = c("bad.xml", "missing.xml"),
      error = "missing.xml: cannot read the file"
    ),
    list(
      argv = c("diff", "bad.xml", "missing.xml"),
      files = list("bad.xml" = bad),
      stdin = NULL,
      reads = c("bad.xml", "missing.xml"),
      error = "missing.xml: cannot read the file"
    ),
    list(
      argv = c("check", "small.xml", "latin.xml", "bad.xml"),
      files = list(
        "small.xml" = "<a/>",
        "latin.xml" = as.raw(
          c(0x3C, 0x61, 0x3E, 0xE9, 0x3C, 0x2F, 0x61, 0x3E)
        ),
        "bad.xml" = bad
      ),
      stdin = NULL,
      reads = c("small.xml", "latin.xml"),
      error = "latin.xml: input is not valid UTF-8"
    ),
    list(
      argv = c("check", "-", "-"),
      files = list(),
      stdin = "<a/>",
      reads = "stdin",
      error = "standard input is named twice"
    ),
    list(
      argv = c("canon", "bad.xml"),
      files = list("bad.xml" = bad),
      stdin = NULL,
      reads = "bad.xml",
      error = problem
    ),
    list(
      argv = c("outline", "bad.xml"),
      files = list("bad.xml" = bad),
      stdin = NULL,
      reads = "bad.xml",
      error = problem
    ),
    list(
      argv = c("get", "/a", "bad.xml"),
      files = list("bad.xml" = bad),
      stdin = NULL,
      reads = "bad.xml",
      error = problem
    ),
    list(
      argv = c("diff", "bad.xml", "small.xml"),
      files = list("bad.xml" = bad, "small.xml" = "<a/>"),
      stdin = NULL,
      reads = c("bad.xml", "small.xml"),
      error = problem
    ),
    list(
      argv = c("check", "-"),
      files = list(),
      stdin = "<a/>",
      reads = "stdin",
      out = "<stdin>: ok, 1 element\n",
      code = 0L
    ),
    list(
      argv = c("check", "same.xml", "same.xml"),
      files = list("same.xml" = "<a/>"),
      stdin = NULL,
      reads = c("same.xml", "same.xml"),
      out = "same.xml: ok, 1 element\nsame.xml: ok, 1 element\n",
      code = 0L
    ),
    list(
      argv = c("check", "-5"),
      files = list("-5" = "<a/>"),
      stdin = NULL,
      reads = "-5",
      out = "-5: ok, 1 element\n",
      code = 0L
    )
  )

  for (case in cases) {
    state <- new.env(parent = emptyenv())
    state$reads <- character()
    state$out_calls <- 0L
    state$out_text <- ""
    state$err_calls <- 0L
    state$err_text <- ""
    io <- make_io(case$files, case$stdin)
    read_file <- io$read_file
    read_stdin <- io$read_stdin
    io$read_file <- function(path) {
      state$reads <- c(state$reads, path)
      read_file(path)
    }
    io$read_stdin <- function() {
      state$reads <- c(state$reads, "stdin")
      read_stdin()
    }
    io$out <- function(text) {
      state$out_calls <- state$out_calls + 1L
      state$out_text <- paste0(state$out_text, text)
    }
    io$err <- function(text) {
      state$err_calls <- state$err_calls + 1L
      state$err_text <- paste0(state$err_text, text)
    }

    if (is.null(case$error)) {
      expect_identical(as.integer(main(case$argv, io)), case$code)
      expect_identical(state$out_text, case$out)
      expect_identical(state$out_calls, 1L)
      expect_identical(state$err_calls, 0L)
      expect_identical(state$err_text, "")
    } else {
      expect_identical(as.integer(main(case$argv, io)), 2L)
      expect_identical(state$out_text, "")
      expect_identical(state$out_calls, 0L)
      expect_identical(state$err_calls, 1L)
      expect_identical(
        state$err_text,
        paste0("xmlvet: ", case$error, "\n")
      )
    }
    expect_identical(state$reads, case$reads)
  }
})

test_that("main propagates unrelated output callback errors", {
  state <- new.env(parent = emptyenv())
  state$out_calls <- 0L
  state$err_calls <- 0L
  io <- make_io(stdin = "<a/>")
  io$out <- function(text) {
    state$out_calls <- state$out_calls + 1L
    stop("sentinel")
  }
  io$err <- function(text) {
    state$err_calls <- state$err_calls + 1L
  }

  expect_error(main(c("canon", "-"), io), "sentinel")
  expect_identical(state$out_calls, 1L)
  expect_identical(state$err_calls, 0L)

  normal <- make_io(stdin = "<a/>")
  normal_state <- new.env(parent = emptyenv())
  normal_state$out_calls <- 0L
  normal_state$out_text <- ""
  normal_state$err_calls <- 0L
  normal_state$err_text <- ""
  normal$out <- function(text) {
    normal_state$out_calls <- normal_state$out_calls + 1L
    normal_state$out_text <- paste0(normal_state$out_text, text)
  }
  normal$err <- function(text) {
    normal_state$err_calls <- normal_state$err_calls + 1L
    normal_state$err_text <- paste0(normal_state$err_text, text)
  }

  expect_identical(as.integer(main(c("canon", "-"), normal)), 0L)
  expect_identical(normal_state$out_calls, 1L)
  expect_identical(normal_state$out_text, "<a></a>\n")
  expect_identical(normal_state$err_calls, 0L)
  expect_identical(normal_state$err_text, "")
})
