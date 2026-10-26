test_that("listings use preorder paths and local sibling indices", {
  root <- new_element(
    "root", character(), character(),
    list(
      new_element("item", character(), character(), list(), 1L, 1L, 2L),
      new_element(
        "section", character(), character(),
        list(
          new_element("item", character(), character(), list(), 1L, 1L, 4L),
          new_element("note", character(), character(), list(), 1L, 1L, 5L),
          new_element("item", character(), character(), list(), 1L, 1L, 6L)
        ),
        1L, 1L, 3L
      ),
      new_element("note", character(), character(), list(), 1L, 1L, 7L),
      new_element("item", character(), character(), list(), 1L, 1L, 8L),
      new_element("item", character(), character(), list(), 1L, 1L, 9L)
    ),
    1L, 1L, 1L
  )
  doc <- list(prolog = list(), root = root, epilog = list())
  original <- doc

  expect_identical(
    list_document(doc, FALSE),
    c(
      "/root[1]",
      "/root[1]/item[1]",
      "/root[1]/section[1]",
      "/root[1]/section[1]/item[1]",
      "/root[1]/section[1]/note[1]",
      "/root[1]/section[1]/item[2]",
      "/root[1]/note[1]",
      "/root[1]/item[2]",
      "/root[1]/item[3]"
    )
  )
  expect_identical(doc, original)
})

test_that("attributes sort by code point independent of input order", {
  names <- c("p:z", "a", "B", "a:b", "\u00e9", "empty", "Z")
  values <- c("p", "lower", "upper", "prefixed", "unicode", "", "z")
  root <- new_element("r", names, values, list(), 1L, 1L, 1L)
  doc <- list(prolog = list(), root = root, epilog = list())
  reordered <- new_element(
    "r",
    c("Z", "empty", "\u00e9", "a:b", "B", "p:z", "a"),
    c("z", "", "unicode", "prefixed", "upper", "p", "lower"),
    list(),
    1L,
    1L,
    1L
  )
  reordered_doc <- list(prolog = list(), root = reordered, epilog = list())
  expected <- c(
    "/r[1]",
    "/r[1]/@B=\"upper\"",
    "/r[1]/@Z=\"z\"",
    "/r[1]/@a=\"lower\"",
    "/r[1]/@a:b=\"prefixed\"",
    "/r[1]/@empty=\"\"",
    "/r[1]/@p:z=\"p\"",
    "/r[1]/@\u00e9=\"unicode\""
  )

  expect_identical(list_document(doc, FALSE), expected)
  expect_identical(list_document(reordered_doc, FALSE), expected)
})

test_that("comments and processing instructions join text but not elements", {
  root <- new_element(
    "r", character(), character(),
    list(
      new_text("before", 2L),
      new_comment("between"),
      new_pi("inside", "ignored"),
      new_text("middle", 3L),
      new_element(
        "branch",
        character(),
        character(),
        list(new_text("inside", 5L)),
        1L,
        1L,
        4L
      ),
      new_text("after", 6L),
      new_pi("next", ""),
      new_comment("also between"),
      new_text("tail", 7L)
    ),
    1L, 1L, 1L
  )
  doc <- list(
    prolog = list(new_comment("prolog"), new_pi("head", "data")),
    root = root,
    epilog = list(new_pi("foot", "data"), new_comment("epilog"))
  )
  original <- doc

  expect_identical(
    list_document(doc, FALSE),
    c(
      "/r[1]",
      "/r[1]/text()[1]=\"beforemiddle\"",
      "/r[1]/branch[1]",
      "/r[1]/branch[1]/text()[1]=\"inside\"",
      "/r[1]/text()[2]=\"aftertail\""
    )
  )
  expect_identical(doc, original)
})

test_that("text numbering and quoted literal values are preserved", {
  root <- new_element(
    "r", character(), character(),
    list(
      new_text("start", 2L),
      new_element("empty", character(), character(), list(), 1L, 1L, 3L),
      new_text('slash\\quote" line\n\t\r\u00e9', 4L)
    ),
    1L, 1L, 1L
  )
  doc <- list(prolog = list(), root = root, epilog = list())

  expect_identical(
    list_document(doc, FALSE),
    c(
      "/r[1]",
      "/r[1]/text()[1]=\"start\"",
      "/r[1]/empty[1]",
      '/r[1]/text()[2]="slash\\\\quote" line\\n\\t\\r\u00e9"'
    )
  )
})

test_that("ignore space trims coalesced edges and retains interior text", {
  root <- new_element(
    "r",
    "keep",
    " \tunchanged\n",
    list(
      new_text(" \t", 2L),
      new_comment("joins whitespace"),
      new_text("\n", 3L),
      new_element("empty", character(), character(), list(), 1L, 1L, 4L),
      new_text(" \tA \tB \n", 5L),
      new_pi("join", ""),
      new_text("\t", 6L),
      new_element("mid", character(), character(), list(), 1L, 1L, 7L),
      new_text("\r", 8L),
      new_text("\u00a0", 9L)
    ),
    1L, 1L, 1L
  )
  doc <- list(prolog = list(), root = root, epilog = list())
  original <- doc

  expect_identical(
    list_document(doc, FALSE),
    c(
      "/r[1]",
      '/r[1]/@keep=" \\tunchanged\\n"',
      '/r[1]/text()[1]=" \\t\\n"',
      "/r[1]/empty[1]",
      '/r[1]/text()[2]=" \\tA \\tB \\n\\t"',
      "/r[1]/mid[1]",
      '/r[1]/text()[3]="\\r\u00a0"'
    )
  )
  expect_identical(
    list_document(doc, TRUE),
    c(
      "/r[1]",
      '/r[1]/@keep=" \\tunchanged\\n"',
      "/r[1]/empty[1]",
      '/r[1]/text()[1]="A \\tB"',
      "/r[1]/mid[1]",
      '/r[1]/text()[2]="\\r\u00a0"'
    )
  )
  expect_identical(doc, original)
})

test_that("first difference reports only the earliest differing line", {
  cases <- list(
    list(
      first = c("/root[1]", "/root[1]/@a=\"1\"", "/root[1]/@z=\"2\""),
      second = c("/root[1]", "/root[1]/@a=\"9\"", "/root[1]/@z=\"8\""),
      expected = "- /root[1]/@a=\"1\"\n+ /root[1]/@a=\"9\"\n"
    ),
    list(
      first = c(
        "/root[1]",
        "/root[1]/text()[1]=\"x\"",
        "/root[1]/text()[2]=\"later\""
      ),
      second = c(
        "/root[1]",
        "/root[1]/text()[1]=\"y\"",
        "/root[1]/text()[2]=\"changed\""
      ),
      expected = "- /root[1]/text()[1]=\"x\"\n+ /root[1]/text()[1]=\"y\"\n"
    ),
    list(
      first = c("/root[1]", "/root[1]/left[1]", "/root[1]/tail[1]"),
      second = c("/root[1]", "/root[1]/right[1]", "/root[1]/tail[1]"),
      expected = "- /root[1]/left[1]\n+ /root[1]/right[1]\n"
    )
  )

  for (case in cases) {
    expect_identical(
      first_difference(case$first, case$second),
      case$expected
    )
  }
})

test_that("equal vectors and both exhaustion directions are handled", {
  expect_null(first_difference(character(), character()))
  expect_null(first_difference(c("/r[1]"), c("/r[1]")))
  expect_identical(
    first_difference(c("one"), c("one", "two")),
    "- (end)\n+ two\n"
  )
  expect_identical(
    first_difference(c("one", "two"), c("one")),
    "- two\n+ (end)\n"
  )
  expect_identical(
    first_difference(character(), c("added")),
    "- (end)\n+ added\n"
  )
  expect_identical(
    first_difference(c("removed"), character()),
    "- removed\n+ (end)\n"
  )
})
