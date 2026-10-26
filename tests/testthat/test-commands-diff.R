test_that("run_diff returns equality and structural differences", {
  cases <- list(
    list(
      first = "<a/>",
      second = "<a/>",
      expected = list(out = "same\n", code = 0L)
    ),
    list(
      first = "<a z=\"2\" a=\"1\"/>",
      second = "<a a=\"1\" z=\"2\"/>",
      expected = list(out = "same\n", code = 0L)
    ),
    list(
      first = "<a>x<!--c--><?p?>y</a>",
      second = "<a>xy</a>",
      expected = list(out = "same\n", code = 0L)
    ),
    list(
      first = "<a x=\"1\"/>",
      second = "<a x=\"2\"/>",
      expected = list(
        out = "- /a[1]/@x=\"1\"\n+ /a[1]/@x=\"2\"\n",
        code = 1L
      )
    ),
    list(
      first = "<a>x</a>",
      second = "<a>y</a>",
      expected = list(
        out = "- /a[1]/text()[1]=\"x\"\n+ /a[1]/text()[1]=\"y\"\n",
        code = 1L
      )
    ),
    list(
      first = "<a><b/></a>",
      second = "<a><c/></a>",
      expected = list(
        out = "- /a[1]/b[1]\n+ /a[1]/c[1]\n",
        code = 1L
      )
    ),
    list(
      first = "<a><b/></a>",
      second = "<a/>",
      expected = list(out = "- /a[1]/b[1]\n+ (end)\n", code = 1L)
    )
  )

  for (case in cases) {
    io <- make_io(list(first.xml = case$first, second.xml = case$second))
    inputs <- new_inputs(io)
    result <- run_diff(
      c("first.xml", "second.xml"),
      character(),
      list(),
      inputs
    )
    expect_identical(result, case$expected)
    expect_identical(io$state$out, "")
    expect_identical(io$state$err, "")
  }
})

test_that("run_diff applies only the requested whitespace trimming", {
  cases <- list(
    list(
      first = "<a> x </a>",
      second = "<a>x</a>",
      flags = character(),
      expected = list(
        out = "- /a[1]/text()[1]=\" x \"\n+ /a[1]/text()[1]=\"x\"\n",
        code = 1L
      )
    ),
    list(
      first = "<a> \t\nx\n\t </a>",
      second = "<a>x</a>",
      flags = "--ignore-space",
      expected = list(out = "same\n", code = 0L)
    ),
    list(
      first = "<a> \t\n</a>",
      second = "<a/>",
      flags = "--ignore-space",
      expected = list(out = "same\n", code = 0L)
    ),
    list(
      first = "<a>x y</a>",
      second = "<a>x  y</a>",
      flags = "--ignore-space",
      expected = list(
        out = paste0(
          "- /a[1]/text()[1]=\"x y\"\n",
          "+ /a[1]/text()[1]=\"x  y\"\n"
        ),
        code = 1L
      )
    )
  )

  for (case in cases) {
    io <- make_io(list(first.xml = case$first, second.xml = case$second))
    inputs <- new_inputs(io)
    result <- run_diff(
      c("first.xml", "second.xml"),
      case$flags,
      list(),
      inputs
    )
    expect_identical(result, case$expected)
    expect_identical(io$state$out, "")
    expect_identical(io$state$err, "")
  }
})

test_that("run_diff validates, reads, and parses in contract order", {
  cases <- list(
    list(
      args = character(),
      files = list(),
      stdin = NULL,
      failure = "missing FILE"
    ),
    list(
      args = "missing.xml",
      files = list(),
      stdin = NULL,
      failure = "missing FILE"
    ),
    list(
      args = c("first.xml", "second.xml", "third.xml"),
      files = list(),
      stdin = NULL,
      failure = "too many arguments"
    ),
    list(
      args = c("missing.xml", "good.xml"),
      files = list(good.xml = "<a/>"),
      stdin = NULL,
      failure = "missing.xml: cannot read the file"
    ),
    list(
      args = c("good.xml", "missing.xml"),
      files = list(good.xml = "<a/>"),
      stdin = NULL,
      failure = "missing.xml: cannot read the file"
    ),
    list(
      args = c("bad.xml", "missing.xml"),
      files = list(bad.xml = "<a>"),
      stdin = NULL,
      failure = "missing.xml: cannot read the file"
    ),
    list(
      args = c("bad.xml", "latin.xml"),
      files = list(bad.xml = "<a>", latin.xml = as.raw(0xFF)),
      stdin = NULL,
      failure = "latin.xml: input is not valid UTF-8"
    ),
    list(
      args = c("bad\u00e9.xml", "good.xml"),
      files = list(good.xml = "<a/>"),
      stdin = NULL,
      failure = "bad?.xml: cannot read the file"
    ),
    list(
      args = c("-", "-"),
      files = list(),
      stdin = "<a>",
      failure = "standard input is named twice"
    ),
    list(
      args = c("-", "file.xml"),
      files = list(file.xml = "<b/>"),
      stdin = "<a/>",
      expected = list(out = "- /a[1]\n+ /b[1]\n", code = 1L)
    ),
    list(
      args = c("file.xml", "-"),
      files = list(file.xml = "<b/>"),
      stdin = "<a/>",
      expected = list(out = "- /b[1]\n+ /a[1]\n", code = 1L)
    ),
    list(
      args = c("same.xml", "same.xml"),
      files = list(same.xml = "<a/>"),
      stdin = NULL,
      expected = list(out = "same\n", code = 0L)
    ),
    list(
      args = c("first.xml", "second.xml"),
      files = list(first.xml = "<a></b>", second.xml = "<c>"),
      stdin = NULL,
      failure = paste0(
        "first.xml:1:4: XV017 end tag \"b\" does not match start tag ",
        "\"a\" opened at 1:1"
      )
    ),
    list(
      args = c("first.xml", "second.xml"),
      files = list(first.xml = "<a/>", second.xml = "<b>"),
      stdin = NULL,
      failure = paste0(
        "second.xml:1:4: XV018 unclosed element \"b\" opened at 1:1"
      )
    )
  )

  for (case in cases) {
    io <- make_io(case$files, case$stdin)
    inputs <- new_inputs(io)

    if (is.null(case$failure)) {
      result <- run_diff(case$args, character(), list(), inputs)
      expect_identical(result, case$expected)
    } else {
      expect_identical(
        failure_of(run_diff(case$args, character(), list(), inputs)),
        case$failure
      )
    }
    expect_identical(io$state$out, "")
    expect_identical(io$state$err, "")
  }
})
