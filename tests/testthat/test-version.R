test_that("description_version reads the Version field", {
  cases <- list(
    list("Package: x\nVersion: 0.2.0\nTitle: T\n", "0.2.0"),
    list("Version:1.0\n", "1.0"),
    list("Package: x\r\nVersion:  3-1-4  \r\n", "3-1-4"),
    list(
      "Description: a\n    Version: 9.9\nVersion: 2.0.1\nVersion: 7.7\n",
      "2.0.1"
    ),
    list("Version: 10.20.30.40", "10.20.30.40")
  )
  for (case in cases) {
    expect_identical(description_version(charToRaw(case[[1L]])), case[[2L]])
  }
})

test_that("description_version rejects a missing or invalid Version", {
  cases <- list(
    list("Package: x\n", "DESCRIPTION has no Version field"),
    list("", "DESCRIPTION has no Version field"),
    list("version: 1.0\n", "DESCRIPTION has no Version field"),
    list("Version:\n", "invalid Version \"\""),
    list("Version: 1\n", "invalid Version \"1\""),
    list("Version: 1.0-beta\n", "invalid Version \"1.0-beta\""),
    list("Version: .1\n", "invalid Version \".1\""),
    list("Version: 1..2\n", "invalid Version \"1..2\"")
  )
  for (case in cases) {
    expect_identical(
      failure_of(description_version(charToRaw(case[[1L]]))),
      case[[2L]]
    )
  }
})

test_that("main prints the version and reports a DESCRIPTION problem", {
  io <- make_io()
  io$read_description <- function() charToRaw("Package: x\nVersion: 4.5.6\n")
  expect_identical(as.integer(main("--version", io)), 0L)
  expect_identical(io$state$out, "xmlvet 4.5.6\n")
  expect_identical(io$state$err, "")

  io <- make_io()
  expect_identical(as.integer(main("--version", io)), 2L)
  expect_identical(io$state$out, "")
  expect_identical(io$state$err, "xmlvet: cannot read DESCRIPTION\n")

  io <- make_io()
  io$read_description <- function() stop("no such file")
  expect_identical(as.integer(main("--version", io)), 2L)
  expect_identical(io$state$err, "xmlvet: cannot read DESCRIPTION\n")

  io <- make_io()
  io$read_description <- function() charToRaw("Package: x\n")
  expect_identical(as.integer(main("--version", io)), 2L)
  expect_identical(
    io$state$err,
    "xmlvet: DESCRIPTION has no Version field\n"
  )
})

test_that("--version is the whole command line", {
  result <- run_main(c("--version", "x"))
  expect_identical(result$code, 2L)
  expect_identical(result$err, "xmlvet: unknown command \"--version\"\n")
})

test_that("the DESCRIPTION of the project says 0.2.0", {
  description <- test_path("..", "..", "DESCRIPTION")
  bytes <- readBin(description, what = "raw", n = file.info(description)$size)
  expect_identical(description_version(bytes), "0.2.0")
})

test_that("executable prints the version of DESCRIPTION", {
  script <- normalizePath(test_path("..", "..", "exec", "xmlvet.R"))
  out <- tempfile()
  on.exit(unlink(out), add = TRUE)

  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(shQuote(script), "--version"),
    stdout = out
  )
  out_text <- gsub("\r\n", "\n", rawToChar(readBin(
    out,
    what = "raw", n = file.info(out)$size
  )), fixed = TRUE)

  expect_identical(status, 0L)
  expect_identical(out_text, "xmlvet 0.2.0\n")
})
