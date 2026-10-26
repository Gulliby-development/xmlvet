test_that("start tags retain node fields and the supplied ordinal", {
  tag <- read_start_tag(make_doc("<a b='x'/>"), 1L, 7L)

  expect_equal(
    tag,
    list(
      node = list(
        type = "element",
        name = "a",
        attr_names = "b",
        attr_values = "x",
        kids = list(),
        line = 1L,
        col = 1L,
        ord = 7L
      ),
      pos = 11L,
      empty = TRUE
    )
  )
})

test_that("attribute values normalize only literal whitespace", {
  expect_equal(
    read_attr_value(make_doc("'x'!"), 1L),
    list(value = "x", pos = 4L)
  )
  expect_equal(
    read_attr_value(
      make_doc("\"1\t2\n3&#9;4&#10;5&#13;6\"!"),
      1L
    ),
    list(value = "1 2 3\t4\n5\r6", pos = 25L)
  )
})

test_that("valid names and tag endings are scanned", {
  expect_equal(read_name(make_doc("a-b.c:d!"), 1L), 8L)
  expect_equal(
    read_name(make_doc("\u00C0\u0300!"), 1L),
    3L
  )
  expect_equal(
    read_start_tag(make_doc("<a-b.c:d x:y=\"1\"/>"), 1L, 2L),
    list(
      node = list(
        type = "element",
        name = "a-b.c:d",
        attr_names = "x:y",
        attr_values = "1",
        kids = list(),
        line = 1L,
        col = 1L,
        ord = 2L
      ),
      pos = 19L,
      empty = TRUE
    )
  )
  expect_equal(
    read_start_tag(make_doc("<a />"), 1L, 1L)$empty,
    TRUE
  )
  expect_equal(
    read_start_tag(make_doc("<a>"), 1L, 1L)$empty,
    FALSE
  )
})

test_that("malformed tags report exact fault indices and codes", {
  cases <- list(
    list(text = "<a b=\"1\"c=\"2\"/>", index = 9L, code = "XV015"),
    list(text = "<a b=\"1\" b=\"2\"/>", index = 10L, code = "XV019"),
    list(text = "<a b>", index = 5L, code = "XV020"),
    list(text = "<a b=\"1", index = 6L, code = "XV020"),
    list(text = "<a b=\"<\"/>", index = 7L, code = "XV023"),
    list(text = "<a 1=\"x\"/>", index = 4L, code = "XV015"),
    list(text = "<a/ >", index = 3L, code = "XV015"),
    list(
      text = "<a b=\"1\" b=\"&x;\"/>",
      index = 13L,
      code = "XV021"
    )
  )

  for (case in cases) {
    expect_equal(
      fault_of(read_start_tag(make_doc(case$text), 1L, 1L)),
      list(index = case$index, code = case$code)
    )
  }
})
