test_that("code point ranges and XML character predicates are exact", {
  expect_identical(
    in_ranges(
      c(0L, 1L, 2L, 3L, 4L, 5L),
      c(1L, 4L),
      c(2L, 4L)
    ),
    c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
  )
  expect_identical(
    in_ranges(integer(), integer(), integer()),
    logical()
  )
  expect_identical(
    is_xml_char(
      c(
        0L, 9L, 10L, 13L, 31L, 32L, 55295L, 55296L, 57344L,
        65533L, 65534L, 65535L, 65536L, 1114111L, 1114112L
      )
    ),
    c(
      FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE,
      TRUE, FALSE, FALSE, TRUE, TRUE, FALSE
    )
  )
  expect_identical(
    is_xml_char(
      c(
        8L, 9L, 10L, 11L, 12L, 13L, 14L, 31L, 32L, 55295L,
        55296L, 57343L, 57344L, 65533L, 65534L, 65535L,
        65536L, 1114111L, 1114112L
      )
    ),
    c(
      FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE,
      TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE,
      FALSE
    )
  )
  expect_identical(
    is_space(c(32L, 9L, 10L, 13L, 160L)),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_identical(
    is_space(c(8L, 9L, 10L, 11L, 12L, 13L, 31L, 32L, 33L)),
    c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
  expect_identical(is_xml_char(integer()), logical())
  expect_identical(is_space(integer()), logical())
})

test_that("name start intervals include endpoints and exclude adjacent gaps", {
  cases <- list(
    list(x = c(57L, 58L, 59L), expected = c(FALSE, TRUE, FALSE)),
    list(x = c(64L, 65L, 90L, 91L), expected = c(FALSE, TRUE, TRUE, FALSE)),
    list(x = c(94L, 95L, 96L), expected = c(FALSE, TRUE, FALSE)),
    list(x = c(96L, 97L, 122L, 123L), expected = c(FALSE, TRUE, TRUE, FALSE)),
    list(x = c(191L, 192L, 214L, 215L), expected = c(FALSE, TRUE, TRUE, FALSE)),
    list(x = c(215L, 216L, 246L, 247L), expected = c(FALSE, TRUE, TRUE, FALSE)),
    list(x = c(247L, 248L, 767L, 768L), expected = c(FALSE, TRUE, TRUE, FALSE)),
    list(
      x = c(879L, 880L, 893L, 894L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    ),
    list(
      x = c(894L, 895L, 8191L, 8192L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    ),
    list(
      x = c(8203L, 8204L, 8205L, 8206L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    ),
    list(
      x = c(8303L, 8304L, 8591L, 8592L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    ),
    list(
      x = c(11263L, 11264L, 12271L, 12272L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    ),
    list(
      x = c(12288L, 12289L, 55295L, 55296L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    ),
    list(
      x = c(63743L, 63744L, 64975L, 64976L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    ),
    list(
      x = c(65007L, 65008L, 65533L, 65534L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    ),
    list(
      x = c(65535L, 65536L, 983039L, 983040L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    )
  )
  for (case in cases) {
    expect_identical(
      is_name_start(case$x),
      case$expected,
      info = paste(case$x, collapse = ",")
    )
  }
  expect_identical(
    is_name_start(
      c(
        58L, 65L, 95L, 97L, 45L, 48L, 192L, 215L, 216L, 65536L,
        983039L, 983040L
      )
    ),
    c(
      TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE,
      FALSE
    )
  )
  expect_identical(is_name_start(integer()), logical())
})

test_that("name character additions have exact inclusive boundaries", {
  cases <- list(
    list(x = c(44L, 45L, 46L, 47L), expected = c(FALSE, TRUE, TRUE, FALSE)),
    list(x = c(45L, 46L, 47L), expected = c(TRUE, TRUE, FALSE)),
    list(x = c(47L, 48L, 57L, 58L), expected = c(FALSE, TRUE, TRUE, TRUE)),
    list(x = c(182L, 183L, 184L), expected = c(FALSE, TRUE, FALSE)),
    list(x = c(767L, 768L, 879L, 880L), expected = c(TRUE, TRUE, TRUE, TRUE)),
    list(
      x = c(8254L, 8255L, 8256L, 8257L),
      expected = c(FALSE, TRUE, TRUE, FALSE)
    )
  )
  for (case in cases) {
    expect_identical(is_name_char(case$x), case$expected)
  }
  expect_identical(
    is_name_char(c(45L, 46L, 48L, 57L, 183L, 768L, 879L, 8255L, 8256L, 47L)),
    c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_identical(is_name_char(integer()), logical())
  expect_identical(
    is_name_char(c(44L, 47L, 58L, 91L, 96L, 123L, 880L, 983040L)),
    c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )
})

test_that("node constructors preserve fields and append text in order", {
  element <- new_element(
    "a", c("z", "b"), c("1", "2"), list(new_text("child", 3L)),
    2L, 3L, 1L
  )
  expect_identical(
    element,
    list(
      type = "element",
      name = "a",
      attr_names = c("z", "b"),
      attr_values = c("1", "2"),
      kids = list(new_text("child", 3L)),
      line = 2L,
      col = 3L,
      ord = 1L
    )
  )
  expect_identical(
    new_text("x", 2L),
    list(type = "text", value = "x", ord = 2L)
  )
  expect_identical(new_comment("c"), list(type = "comment", value = "c"))
  expect_identical(
    new_pi("p", "d"),
    list(type = "pi", target = "p", data = "d")
  )
  expect_identical(append_text(list(), "", 1L), list())
  expect_identical(
    append_text(list(), "x", 1L),
    list(new_text("x", 1L))
  )
  kids <- list(new_text("x", 2L))
  expect_identical(append_text(kids, "", 3L), kids)
  expect_identical(
    append_text(kids, "y", 3L),
    list(new_text("xy", 2L))
  )
  expect_identical(
    append_text(list(new_comment("c")), "y", 3L),
    list(new_comment("c"), new_text("y", 3L))
  )
  expect_identical(
    append_text(list(new_pi("p", "d")), "y", 3L),
    list(new_pi("p", "d"), new_text("y", 3L))
  )
})

test_that("string values traverse descendants in document order", {
  exact <- new_element(
    "a",
    character(),
    character(),
    list(
      new_text("x", 1L),
      new_element(
        "b",
        character(),
        character(),
        list(new_text("y", 3L)),
        1L,
        1L,
        2L
      ),
      new_comment("ignored"),
      new_pi("p", "ignored"),
      new_text("z", 5L)
    ),
    1L,
    1L,
    1L
  )
  expect_identical(string_value(exact), "xyz")

  leaf <- new_element(
    "leaf",
    character(),
    character(),
    list(new_text("z", 4L)),
    3L,
    1L,
    3L
  )
  branch <- new_element(
    "branch",
    character(),
    character(),
    list(
      new_text("y", 2L),
      leaf,
      new_comment("ignored"),
      new_pi("p", "ignored"),
      new_text("w", 5L)
    ),
    2L,
    1L,
    2L
  )
  root <- new_element(
    "root",
    character(),
    character(),
    list(
      new_text("x", 1L),
      branch,
      new_comment("ignored"),
      new_pi("q", "ignored"),
      new_text("v", 6L)
    ),
    1L,
    1L,
    1L
  )
  expect_identical(string_value(root), "xyzwv")

  empty <- new_element(
    "empty",
    character(),
    character(),
    list(
      new_element(
        "child",
        character(),
        character(),
        list(new_comment("ignored"), new_pi("p", "ignored")),
        1L,
        1L,
        2L
      ),
      new_comment("ignored"),
      new_pi("q", "ignored")
    ),
    1L,
    1L,
    1L
  )
  expect_identical(string_value(empty), "")
})
