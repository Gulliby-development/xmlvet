test_that("faults preserve their classes, fields, and messages", {
  failure <- tryCatch(fail("bad"), xmlvet_failure = function(error) error)
  expect_identical(
    class(failure), c("xmlvet_failure", "error", "condition")
  )
  expect_identical(failure$message, "bad")
  expect_null(failure$call)
  expect_identical(failure_of(fail("bad")), "bad")

  syntax <- tryCatch(
    syntax_fault(3L, "XV001", "bad"),
    xmlvet_syntax = function(error) error
  )
  expect_identical(class(syntax), c("xmlvet_syntax", "error", "condition"))
  expect_identical(syntax$index, 3L)
  expect_identical(syntax$code, "XV001")
  expect_identical(syntax$message, "bad")
  expect_null(syntax$call)
  expect_identical(
    fault_of(syntax_fault(3L, "XV001", "bad")),
    list(index = 3L, code = "XV001")
  )

  path <- tryCatch(
    path_fault("unexpected character", 4L),
    xmlvet_path = function(error) error
  )
  expect_identical(class(path), c("xmlvet_path", "error", "condition"))
  expect_identical(path$pos, 4L)
  expect_identical(path$message, "unexpected character")
  expect_null(path$call)
  expect_identical(
    path_fault_of(path_fault("unexpected character", 4L)),
    list(pos = 4L, message = "unexpected character")
  )
})

test_that("diagnostic text is ASCII-safe and preserves printable quotes", {
  expect_identical(plain_text("A\n\t\r\u00e9\U0001f600Z"), "A?????Z")
  expect_identical(show_text('a"b'), '"a"b"')
  expect_identical(
    utf8ToInt(show_text('a"b')),
    c(34L, 97L, 34L, 98L, 34L)
  )
  expect_identical(show_text(""), '""')
})

test_that("plural formats zero, one, and many", {
  expect_identical(plural(1, "element"), "1 element")
  expect_identical(plural(0, "element"), "0 elements")
  expect_identical(plural(2, "element"), "2 elements")
})

test_that("values escape line controls and preserve other code points", {
  expect_identical(show_value("\\\n\t\r\u00e9"), "\\\\\\n\\t\\r\u00e9")
  expect_identical(quoted_value("x\ny"), '"x\\ny"')
  expect_identical(show_value('a"b\u00e9'), 'a"b\u00e9')
  expect_identical(quoted_value('a"b\u00e9'), '"a"b\u00e9"')
})

test_that("code point ordering is lexical and stable", {
  expect_identical(
    order_codepoints(c("z", "b", "B", "a")),
    c(3L, 4L, 2L, 1L)
  )
  expect_identical(
    order_codepoints(c("\u00e9", "a", "aa", "a", "\U0001f600")),
    c(2L, 4L, 3L, 1L, 5L)
  )
  expect_identical(order_codepoints(character()), integer())
  expect_identical(order_codepoints(c("", "a", "", "aa")), c(1L, 3L, 2L, 4L))
})
