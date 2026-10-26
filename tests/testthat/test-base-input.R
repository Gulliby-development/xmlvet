test_that("decode_bytes preserves valid UTF-8 bytes", {
  expect_identical(decode_bytes(raw()), "")

  decoded <- decode_bytes(
    as.raw(c(0x41, 0xC2, 0xA2, 0xE2, 0x82, 0xAC, 0xF0, 0x9F, 0x98, 0x80))
  )
  expect_identical(decoded, "A\u00a2\u20ac\U0001f600")
  expect_identical(Encoding(decoded), "UTF-8")

  expect_identical(
    decode_bytes(as.raw(c(0xEF, 0xBB, 0xBF, 0x61, 0x0D, 0x0A))),
    "\ufeffa\r\n"
  )
  expect_identical(decode_bytes(as.raw(0x01)), "\u0001")
})

test_that("decode_bytes rejects malformed UTF-8 without warnings", {
  invalid <- list(
    as.raw(0x00),
    as.raw(c(0x61, 0x00, 0x62)),
    as.raw(0x80),
    as.raw(0xFF),
    as.raw(c(0xC0, 0xAF)),
    as.raw(c(0xE0, 0x80, 0xAF)),
    as.raw(c(0xF0, 0x80, 0x80, 0xAF)),
    as.raw(c(0xED, 0xA0, 0x80)),
    as.raw(c(0xF4, 0x90, 0x80, 0x80)),
    as.raw(c(0xE2, 0x82)),
    as.raw(0xC1),
    as.raw(0xBF),
    as.raw(c(0xE2, 0x28, 0xA1)),
    as.raw(c(0xF5, 0x80, 0x80, 0x80))
  )

  for (bytes in invalid) {
    decoded <- NULL
    expect_no_warning(decoded <- decode_bytes(bytes))
    expect_null(decoded)
  }
})

test_that("new_inputs reads stdin once and rereads named files", {
  calls <- new.env(parent = emptyenv())
  calls$stdin <- 0L
  calls$paths <- character()
  io <- list(
    read_stdin = function() {
      calls$stdin <- calls$stdin + 1L
      charToRaw("<a/>")
    },
    read_file = function(path) {
      calls$paths <- c(calls$paths, path)
      charToRaw("x")
    }
  )
  inputs <- new_inputs(io)

  expect_identical(
    inputs$read("-"),
    list(label = "<stdin>", text = "<a/>")
  )
  expect_identical(calls$stdin, 1L)
  expect_identical(
    inputs$read("a.xml"),
    list(label = "a.xml", text = "x")
  )
  expect_identical(
    inputs$read("a.xml"),
    list(label = "a.xml", text = "x")
  )
  expect_identical(calls$paths, c("a.xml", "a.xml"))
})

test_that("stdin attempts are single-use per input instance", {
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  failing_io <- list(
    read_stdin = function() {
      calls$count <- calls$count + 1L
      stop("read failed")
    },
    read_file = function(path) raw()
  )
  failed_inputs <- new_inputs(failing_io)

  expect_identical(
    failure_of(failed_inputs$read("-")),
    "<stdin>: cannot read the file"
  )
  expect_identical(
    failure_of(failed_inputs$read("-")),
    "standard input is named twice"
  )
  expect_identical(calls$count, 1L)

  good_io <- list(
    read_stdin = function() {
      calls$count <- calls$count + 1L
      charToRaw("ok")
    },
    read_file = function(path) raw()
  )
  other_inputs <- new_inputs(good_io)
  expect_identical(
    other_inputs$read("-"),
    list(label = "<stdin>", text = "ok")
  )
  expect_identical(calls$count, 2L)

  invalid_io <- list(
    read_stdin = function() {
      calls$count <- calls$count + 1L
      as.raw(0xFF)
    },
    read_file = function(path) raw()
  )
  invalid_inputs <- new_inputs(invalid_io)
  expect_identical(
    failure_of(invalid_inputs$read("-")),
    "<stdin>: input is not valid UTF-8"
  )
  expect_identical(
    failure_of(invalid_inputs$read("-")),
    "standard input is named twice"
  )
  expect_identical(calls$count, 3L)
})

test_that("new_inputs reports read errors and sanitized labels", {
  calls <- new.env(parent = emptyenv())
  calls$paths <- character()
  io <- list(
    read_stdin = function() stop("stdin failed"),
    read_file = function(path) {
      calls$paths <- c(calls$paths, path)
      if (identical(path, "missing.xml")) {
        stop("file failed")
      }
      as.raw(0xE9)
    }
  )
  inputs <- new_inputs(io)

  expect_identical(
    failure_of(inputs$read("missing.xml")),
    "missing.xml: cannot read the file"
  )
  expect_identical(
    failure_of(inputs$read("-")),
    "<stdin>: cannot read the file"
  )
  expect_identical(
    failure_of(inputs$read("latin.xml")),
    "latin.xml: input is not valid UTF-8"
  )
  expect_identical(
    failure_of(inputs$read("a\u00e9.xml")),
    "a?.xml: input is not valid UTF-8"
  )
  expect_identical(
    calls$paths,
    c("missing.xml", "latin.xml", "a\u00e9.xml")
  )
})
