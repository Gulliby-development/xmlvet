test_that("executable rejects malformed UTF-8 on stdin", {
  script <- normalizePath(test_path("..", "..", "exec", "xmlvet.R"))
  input <- tempfile()
  out <- tempfile()
  err <- tempfile()
  on.exit(unlink(c(input, out, err)), add = TRUE)

  writeBin(
    as.raw(c(0x3C, 0x61, 0x3E, 0xFF, 0x3C, 0x2F, 0x61, 0x3E)),
    input
  )
  status <- withCallingHandlers(
    system2(
      file.path(R.home("bin"), "Rscript"),
      c(shQuote(script), "check", "-"),
      stdin = input,
      stdout = out,
      stderr = err
    ),
    warning = function(condition) {
      expected_warning <- endsWith(
        conditionMessage(condition),
        "had status 2"
      )
      if (expected_warning) {
        invokeRestart("muffleWarning")
      }
    }
  )
  out_bytes <- readBin(out, what = "raw", n = file.info(out)$size)
  err_bytes <- readBin(err, what = "raw", n = file.info(err)$size)
  err_text <- gsub("\r\n", "\n", rawToChar(err_bytes), fixed = TRUE)

  expect_identical(status, 2L)
  expect_length(out_bytes, 0L)
  expect_identical(
    err_text,
    "xmlvet: <stdin>: input is not valid UTF-8\n"
  )
})
