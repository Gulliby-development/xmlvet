test_that("check counts elements and preserves successful input order", {
  calls <- new.env(parent = emptyenv())
  calls$reads <- character()
  files <- list(
    "solo.xml" = "<solo/>",
    "nested.xml" = paste0(
      "<root><!-- note --><?p data?><child>text",
      "<leaf/></child><empty/></root>"
    )
  )
  io <- make_io(files, "<stdin-root/>")
  read_file <- io$read_file
  read_stdin <- io$read_stdin
  io$read_file <- function(path) {
    calls$reads <- c(calls$reads, paste0("file:", path))
    read_file(path)
  }
  io$read_stdin <- function() {
    calls$reads <- c(calls$reads, "stdin")
    read_stdin()
  }
  inputs <- new_inputs(io)

  result <- run_check(
    c("solo.xml", "nested.xml", "-", "solo.xml"),
    character(),
    list(),
    inputs
  )

  expect_identical(
    result,
    list(
      out = paste0(
        "solo.xml: ok, 1 element\n",
        "nested.xml: ok, 4 elements\n",
        "<stdin>: ok, 1 element\n",
        "solo.xml: ok, 1 element\n"
      ),
      code = 0L
    )
  )
  expect_identical(
    calls$reads,
    c("file:solo.xml", "file:nested.xml", "stdin", "file:solo.xml")
  )
  expect_identical(io$state$out, "")
  expect_identical(io$state$err, "")
})

test_that("check reports ordered mixed results after reading all files", {
  calls <- new.env(parent = emptyenv())
  calls$reads <- character()
  files <- list(
    "catalog.xml" = paste0(
      "<?xml version=\"1.0\"?>\n",
      "<catalog>\n",
      "  <book id=\"b2\" lang=\"en\">Tom &amp; Jerry</book>\n",
      "  <book lang=\"fr\" id=\"b1\"><title>Plain</title></book>\n",
      "</catalog>\n"
    ),
    "bad.xml" = "<a>\n  <b>\n</a>",
    "small.xml" = "<a x=\"1\"><b>t</b><b/></a>"
  )
  io <- make_io(files)
  read_file <- io$read_file
  io$read_file <- function(path) {
    calls$reads <- c(calls$reads, path)
    read_file(path)
  }
  inputs <- new_inputs(io)

  result <- run_check(
    c("catalog.xml", "bad.xml", "small.xml"),
    character(),
    list(),
    inputs
  )

  expect_identical(
    result,
    list(
      out = paste0(
        "catalog.xml: ok, 4 elements\n",
        "bad.xml:3:1: XV017 end tag \"a\" does not match start tag ",
        "\"b\" opened at 2:3\n",
        "small.xml: ok, 3 elements\n"
      ),
      code = 1L
    )
  )
  expect_identical(calls$reads, c("catalog.xml", "bad.xml", "small.xml"))
  expect_identical(io$state$out, "")
  expect_identical(io$state$err, "")
})

test_that("check quiet mode keeps only problem lines", {
  cases <- list(
    list(
      args = c("ok.xml", "bad.xml"),
      flags = "quiet",
      files = list("ok.xml" = "<r><c/></r>", "bad.xml" = "<a>"),
      out = "bad.xml:1:4: XV018 unclosed element \"a\" opened at 1:1\n",
      code = 1L
    ),
    list(
      args = c("one.xml", "two.xml"),
      flags = "quiet",
      files = list("one.xml" = "<a/>", "two.xml" = "<b><c/></b>"),
      out = "",
      code = 0L
    ),
    list(
      args = c("bad.xml", "worse.xml"),
      flags = "quiet",
      files = list("bad.xml" = "<a>", "worse.xml" = "</a>"),
      out = paste0(
        "bad.xml:1:4: XV018 unclosed element \"a\" opened at 1:1\n",
        "worse.xml:1:1: XV016 malformed end tag\n"
      ),
      code = 1L
    )
  )

  for (case in cases) {
    io <- make_io(case$files)
    result <- run_check(
      case$args,
      case$flags,
      list(),
      new_inputs(io)
    )

    expect_identical(result, list(out = case$out, code = case$code))
    expect_identical(io$state$out, "")
    expect_identical(io$state$err, "")
  }
})

test_that("check propagates input failures without reporting XML problems", {
  missing_calls <- new.env(parent = emptyenv())
  missing_calls$reads <- character()
  missing_io <- make_io()
  missing_read_file <- missing_io$read_file
  missing_read_stdin <- missing_io$read_stdin
  missing_io$read_file <- function(path) {
    missing_calls$reads <- c(missing_calls$reads, path)
    missing_read_file(path)
  }
  missing_io$read_stdin <- function() {
    missing_calls$reads <- c(missing_calls$reads, "stdin")
    missing_read_stdin()
  }
  missing_inputs <- new_inputs(missing_io)

  expect_identical(
    failure_of(run_check(character(), character(), list(), missing_inputs)),
    "missing FILE"
  )
  expect_identical(missing_calls$reads, character())
  expect_identical(missing_io$state$out, "")
  expect_identical(missing_io$state$err, "")

  ordered_calls <- new.env(parent = emptyenv())
  ordered_calls$reads <- character()
  ordered_io <- make_io(list("bad.xml" = "<a>"))
  ordered_read_file <- ordered_io$read_file
  ordered_io$read_file <- function(path) {
    ordered_calls$reads <- c(ordered_calls$reads, path)
    ordered_read_file(path)
  }
  ordered_inputs <- new_inputs(ordered_io)

  expect_identical(
    failure_of(run_check(
      c("bad.xml", "missing.xml"),
      character(),
      list(),
      ordered_inputs
    )),
    "missing.xml: cannot read the file"
  )
  expect_identical(ordered_calls$reads, c("bad.xml", "missing.xml"))
  expect_identical(ordered_io$state$out, "")
  expect_identical(ordered_io$state$err, "")

  utf8_calls <- new.env(parent = emptyenv())
  utf8_calls$reads <- character()
  utf8_io <- make_io(list(
    "valid.xml" = "<a/>",
    "latin.xml" = as.raw(c(0x3C, 0x61, 0x3E, 0xE9, 0x3C, 0x2F, 0x61, 0x3E))
  ))
  utf8_read_file <- utf8_io$read_file
  utf8_io$read_file <- function(path) {
    utf8_calls$reads <- c(utf8_calls$reads, path)
    utf8_read_file(path)
  }
  utf8_inputs <- new_inputs(utf8_io)

  expect_identical(
    failure_of(run_check(
      c("valid.xml", "latin.xml"),
      character(),
      list(),
      utf8_inputs
    )),
    "latin.xml: input is not valid UTF-8"
  )
  expect_identical(utf8_calls$reads, c("valid.xml", "latin.xml"))
  expect_identical(utf8_io$state$out, "")
  expect_identical(utf8_io$state$err, "")

  stdin_calls <- new.env(parent = emptyenv())
  stdin_calls$reads <- 0L
  stdin_io <- make_io(stdin = "<a/>")
  stdin_read <- stdin_io$read_stdin
  stdin_io$read_stdin <- function() {
    stdin_calls$reads <- stdin_calls$reads + 1L
    stdin_read()
  }
  stdin_inputs <- new_inputs(stdin_io)

  expect_identical(
    failure_of(run_check(c("-", "-"), character(), list(), stdin_inputs)),
    "standard input is named twice"
  )
  expect_identical(stdin_calls$reads, 1L)
  expect_identical(stdin_io$state$out, "")
  expect_identical(stdin_io$state$err, "")
})
