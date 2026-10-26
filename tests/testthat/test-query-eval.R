test_that("evaluation handles virtual roots and candidate positions", {
  root <- new_element(
    "a", character(), character(),
    list(
      new_element(
        "b", character(), character(),
        list(new_text("1", 3L)), 1L, 1L, 2L
      ),
      new_element(
        "c", character(), character(),
        list(
          new_element(
            "b", character(), character(),
            list(new_text("2", 6L)), 1L, 1L, 5L
          ),
          new_element(
            "b", character(), character(),
            list(new_text("3", 8L)), 1L, 1L, 7L
          )
        ), 1L, 1L, 4L
      ),
      new_element(
        "b", character(), character(),
        list(new_text("4", 10L)), 1L, 1L, 9L
      ),
      new_element(
        "d", character(), character(),
        list(
          new_element(
            "e", character(), character(),
            list(
              new_element(
                "b", character(), character(),
                list(new_text("5", 14L)), 1L, 1L, 13L
              )
            ), 1L, 1L, 12L
          )
        ), 1L, 1L, 11L
      )
    ), 1L, 1L, 1L
  )
  cases <- list(
    list(path = "/a/b", expected = c("1", "4")),
    list(path = "//b", expected = c("1", "2", "3", "4", "5")),
    list(path = "//b[1]", expected = c("1", "2", "5")),
    list(path = "//b[2]", expected = c("3", "4")),
    list(path = "//*[2]", expected = c("23", "3")),
    list(path = "/a", expected = "12345"),
    list(path = "/b", expected = character(0))
  )

  for (case in cases) {
    expect_identical(
      evaluate_path(root, parse_path(case$path)),
      case$expected
    )
  }
})

test_that("predicates filter sequentially within each candidate", {
  root <- new_element(
    "r", character(), character(),
    list(
      new_element(
        "i", "k", "1", list(new_text("a", 3L)), 1L, 1L, 2L
      ),
      new_element(
        "i", "k", "2", list(new_text("b", 5L)), 1L, 1L, 4L
      ),
      new_element(
        "i", "k", "1", list(new_text("c", 7L)), 1L, 1L, 6L
      ),
      new_element(
        "i", character(), character(),
        list(new_text("d", 9L)), 1L, 1L, 8L
      ),
      new_element(
        "j", "k", "1",
        list(
          new_element(
            "i", "k", "1", list(new_text("e", 12L)), 1L, 1L, 11L
          )
        ), 1L, 1L, 10L
      )
    ), 1L, 1L, 1L
  )
  cases <- list(
    list(path = "/r/i[@k='1'][2]", expected = "c"),
    list(path = "/r/i[2][@k='2']", expected = "b"),
    list(path = "/r/i[@k][2]", expected = "b"),
    list(path = "/r/i[2][@k='1']", expected = character(0)),
    list(path = "//*[@k]/@k", expected = c("1", "2", "1", "1", "1")),
    list(path = "/r/i[999999]", expected = character(0))
  )

  for (case in cases) {
    expect_identical(
      evaluate_path(root, parse_path(case$path)),
      case$expected
    )
  }
})

test_that("text selection preserves mixed-content node boundaries", {
  root <- new_element(
    "a", character(), character(),
    list(
      new_text("x", 2L),
      new_element(
        "b", character(), character(),
        list(new_text("y", 4L)), 1L, 1L, 3L
      ),
      new_text("z", 5L),
      new_comment("c"),
      new_text("w", 6L),
      new_element("b", character(), character(), list(), 1L, 1L, 7L)
    ), 1L, 1L, 1L
  )
  pi_root <- new_element(
    "a", character(), character(),
    list(new_text("x", 2L), new_pi("p", ""), new_text("y", 3L)),
    1L, 1L, 1L
  )
  cases <- list(
    list(path = "/a/text()", expected = c("x", "z", "w")),
    list(path = "/a/b", expected = c("y", "")),
    list(path = "//text()", expected = c("x", "y", "z", "w")),
    list(path = "/a", expected = "xyzw"),
    list(path = "/a/@missing", expected = character(0))
  )

  for (case in cases) {
    expect_identical(
      evaluate_path(root, parse_path(case$path)),
      case$expected
    )
  }
  expect_identical(
    evaluate_path(pi_root, parse_path("/a/text()")),
    c("x", "y")
  )
})

test_that("overlapping descendant paths retain distinct node identities", {
  root <- new_element(
    "a", character(), character(),
    list(
      new_element(
        "a", character(), character(),
        list(
          new_element("a", character(), character(), list(), 1L, 1L, 3L),
          new_element("b", character(), character(), list(), 1L, 1L, 4L)
        ), 1L, 1L, 2L
      ),
      new_element(
        "b", character(), character(),
        list(
          new_element("a", character(), character(), list(), 1L, 1L, 6L)
        ), 1L, 1L, 5L
      )
    ), 1L, 1L, 1L
  )
  cases <- list(
    list(path = "//a", expected = c("", "", "", "")),
    list(path = "//a/a", expected = c("", "")),
    list(path = "//a//a", expected = c("", "", "")),
    list(path = "//*", expected = c("", "", "", "", "", "")),
    list(path = "//a[1]", expected = c("", "", "", ""))
  )

  for (case in cases) {
    expect_identical(
      evaluate_path(root, parse_path(case$path)),
      case$expected
    )
  }
})

test_that("attribute selection preserves owners and raw values", {
  root <- new_element(
    "a", c("z", "id", "a"), c("Z", "root", "A"),
    list(
      new_element(
        "b", "id", "same", list(), 1L, 1L, 2L
      ),
      new_element(
        "b", "id", "same", list(), 1L, 1L, 3L
      )
    ), 1L, 1L, 1L
  )
  empty_root <- new_element(
    "a", "id", "", list(), 1L, 1L, 1L
  )

  expect_identical(
    evaluate_path(root, parse_path("//@id")),
    c("root", "same", "same")
  )
  expect_identical(
    evaluate_path(root, parse_path("/a/@id")),
    "root"
  )
  expect_identical(
    evaluate_path(empty_root, parse_path("/a[@id='']/@id")),
    ""
  )
})

test_that("text results retain whitespace, backslash, and Unicode", {
  value <- "\n\t\r\\\u00E9"
  root <- new_element(
    "a", character(), character(),
    list(new_text(value, 2L)), 1L, 1L, 1L
  )

  expect_identical(
    evaluate_path(root, parse_path("/a/text()")),
    value
  )
})
