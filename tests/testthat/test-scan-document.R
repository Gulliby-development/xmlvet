test_that("document assembly preserves positions and node ordinals", {
  parsed <- parse_document(
    paste0(
      "<!--p--><?p x?><a z='1' b='2'>x&amp;",
      "<![CDATA[y]]><b/>z<!--c-->w<?q?>v</a><?e?><!--e-->"
    )
  )

  expect_true(parsed$ok)
  expect_identical(
    parsed$doc$prolog,
    list(new_comment("p"), new_pi("p", "x"))
  )
  expect_identical(
    parsed$doc$epilog,
    list(new_pi("e", ""), new_comment("e"))
  )

  root <- parsed$doc$root
  expect_identical(root$name, "a")
  expect_identical(root$line, 1L)
  expect_identical(root$col, 16L)
  expect_identical(paste0(root$line, ":", root$col), "1:16")
  expect_identical(root$ord, 1L)
  expect_identical(root$attr_names, c("z", "b"))
  expect_identical(root$attr_values, c("1", "2"))
  expect_identical(
    root$kids[[1L]],
    new_text("x&y", 2L)
  )
  expect_identical(
    root$kids[[2L]],
    new_element("b", character(), character(), list(), 1L, 50L, 3L)
  )
  expect_identical(root$kids[[3L]], new_text("z", 4L))
  expect_identical(root$kids[[4L]], new_comment("c"))
  expect_identical(root$kids[[5L]], new_text("w", 5L))
  expect_identical(root$kids[[6L]], new_pi("q", ""))
  expect_identical(root$kids[[7L]], new_text("v", 6L))
})

test_that("valid full documents retain references and markup values", {
  examples <- list(
    list(
      text = "<a>&amp;&lt;&gt;&quot;&apos;</a>",
      value = "&<>\"'",
      attr_names = character(),
      attr_values = character()
    ),
    list(
      text = "<a>&#65;&#x41;&#x1f600;&#00065;</a>",
      value = "AA\U0001F600A",
      attr_names = character(),
      attr_values = character()
    ),
    list(
      text = "<a  b = \"x y\"  ></a>",
      value = "",
      attr_names = "b",
      attr_values = "x y"
    ),
    list(
      text = "<a b=\"1&#9;2&#10;3&#13;4\"/>",
      value = "",
      attr_names = "b",
      attr_values = "1\t2\n3\r4"
    ),
    list(
      text = "<a><![CDATA[]]]]></a>",
      value = "]]",
      attr_names = character(),
      attr_values = character()
    ),
    list(
      text = "<?xml  version = \"1.0\"  encoding = \"UTF-8\"  ?><a/>",
      value = "",
      attr_names = character(),
      attr_values = character()
    ),
    list(
      text = "<!DOCTYPE a SYSTEM 'x'><a/>",
      value = "",
      attr_names = character(),
      attr_values = character()
    ),
    list(
      text = "<a>]]</a>",
      value = "]]",
      attr_names = character(),
      attr_values = character()
    )
  )

  for (example in examples) {
    parsed <- parse_document(example$text)
    expect_true(parsed$ok, info = example$text)
    expect_identical(string_value(parsed$doc$root), example$value)
    expect_identical(parsed$doc$root$attr_names, example$attr_names)
    expect_identical(parsed$doc$root$attr_values, example$attr_values)
  }

  tag <- read_start_tag(make_doc("<a b='x'/>"), 1L, 7L)
  expect_identical(
    tag,
    list(
      node = new_element("a", "b", "x", list(), 1L, 1L, 7L),
      pos = 11L,
      empty = TRUE
    )
  )

  parsed <- parse_document("<a><![CDATA[]]><b/><![CDATA[]]></a>")
  expect_true(parsed$ok)
  expect_identical(
    parsed$doc$root$kids,
    list(new_element("b", character(), character(), list(), 1L, 16L, 2L))
  )
})

test_that("one hundred nested elements are accepted with document ordinals", {
  opening <- paste0(
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>"
  )
  closing <- paste0(
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>"
  )
  parsed <- parse_document(paste0(opening, closing))

  expect_true(parsed$ok)
  node <- parsed$doc$root
  ordinals <- integer()
  for (i in seq_len(100L)) {
    ordinals <- c(ordinals, node$ord)
    if (i < 100L) {
      expect_length(node$kids, 1L)
      node <- node$kids[[1L]]
    }
  }
  expect_identical(
    ordinals,
    c(
      1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L,
      11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L,
      21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L,
      31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L,
      41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L,
      51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L,
      61L, 62L, 63L, 64L, 65L, 66L, 67L, 68L, 69L, 70L,
      71L, 72L, 73L, 74L, 75L, 76L, 77L, 78L, 79L, 80L,
      81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L,
      91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L
    )
  )
  expect_length(node$kids, 0L)
})

test_that("the 101st start and incomplete 101st start are rejected safely", {
  opening <- paste0(
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>",
    "<a><a><a><a><a><a><a><a><a><a>"
  )
  closing <- paste0(
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a></a></a></a></a></a></a></a></a></a>",
    "</a>"
  )
  expected <- list(
    line = 1L,
    col = 301L,
    code = "XV024",
    message = "nesting is deeper than 100 levels"
  )

  full_problem <- problem_of(paste0(opening, "<a>", closing))
  expect_identical(full_problem, expected)
  expect_identical(
    paste0(
      full_problem$line,
      ":",
      full_problem$col,
      " ",
      full_problem$code,
      " ",
      full_problem$message
    ),
    "1:301 XV024 nesting is deeper than 100 levels"
  )
  expect_identical(
    problem_of(paste0(opening, "<a")),
    expected
  )
})
