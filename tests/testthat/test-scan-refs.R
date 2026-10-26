test_that("predefined references return their text and next position", {
  for (case in list(
    list(input = "&amp;!", index = 1L, text = "&", pos = 6L),
    list(input = "x&lt;!", index = 2L, text = "<", pos = 6L),
    list(input = "x&gt;!", index = 2L, text = ">", pos = 6L),
    list(input = "x&quot;!", index = 2L, text = "\"", pos = 8L),
    list(input = "x&apos;!", index = 2L, text = "'", pos = 8L)
  )) {
    expect_identical(
      read_reference(make_doc(case$input), case$index),
      list(text = case$text, pos = case$pos)
    )
  }
})

test_that("numeric references decode code points and referenced whitespace", {
  for (case in list(
    list(input = "&#65;!", text = "A", pos = 6L),
    list(input = "&#x41;!", text = "A", pos = 7L),
    list(input = "&#x1f600;!", text = "\U0001F600", pos = 10L),
    list(input = "&#00065;!", text = "A", pos = 9L),
    list(input = "&#9;!", text = "\t", pos = 5L),
    list(input = "&#10;!", text = "\n", pos = 6L),
    list(input = "&#13;!", text = "\r", pos = 6L)
  )) {
    expect_identical(
      read_reference(make_doc(case$input), 1L),
      list(text = case$text, pos = case$pos)
    )
  }
})

test_that("leading zeros and XML character boundaries are accepted", {
  for (case in list(
    list(input = "&#00000000000065;", text = "A", pos = 18L),
    list(input = "&#x00000000000041;", text = "A", pos = 19L),
    list(input = "&#1114111;", text = "\U0010FFFF", pos = 11L),
    list(input = "&#x10FFFF;", text = "\U0010FFFF", pos = 11L),
    list(input = "&#xD7FF;", text = "\uD7FF", pos = 9L),
    list(input = "&#xE000;", text = "\uE000", pos = 9L),
    list(input = "&#xFFFD;", text = "\uFFFD", pos = 9L)
  )) {
    expect_identical(
      read_reference(make_doc(case$input), 1L),
      list(text = case$text, pos = case$pos)
    )
  }
})

test_that("malformed references fault at their ampersand", {
  for (reference in list(
    "&x;",
    "&",
    "&amp",
    "&#0;",
    "&#X41;",
    "&#;",
    "&#x;",
    "&#123456789;",
    "&#x123456789;",
    "&#xD800;",
    "&#x110000;",
    "&#xFFFE;"
  )) {
    expect_identical(
      fault_of(read_reference(make_doc(reference), 1L)),
      list(index = 1L, code = "XV021")
    )
  }

  expect_identical(
    fault_of(read_reference(make_doc("<a b=\"&x;\"/>"), 7L)),
    list(index = 7L, code = "XV021")
  )
})
