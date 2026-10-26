test_that("attribute and tag faults keep content precedence", {
  cases <- list(
    list(
      text = "<a b=\"1\"c=\"2\"/>",
      line = 1L, col = 9L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a b=\"1\" b=\"2\"/>",
      line = 1L, col = 10L, code = "XV019",
      message = "duplicate attribute \"b\""
    ),
    list(
      text = "<a b>", line = 1L, col = 5L, code = "XV020",
      message = "malformed attribute"
    ),
    list(
      text = "<a b=\"1", line = 1L, col = 6L, code = "XV020",
      message = "malformed attribute"
    ),
    list(
      text = "<a b=\"<\"/>",
      line = 1L, col = 7L, code = "XV023",
      message = "\"<\" is not allowed in an attribute value"
    ),
    list(
      text = "<a 1=\"x\"/>",
      line = 1L, col = 4L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a/ >",
      line = 1L, col = 3L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a\n  b=\"1\"\n  b=\"2\"/>",
      line = 3L, col = 3L, code = "XV019",
      message = "duplicate attribute \"b\""
    ),
    list(
      text = "<a b=\"1\" b=\"&x;\"/>",
      line = 1L, col = 13L, code = "XV021",
      message = "invalid reference"
    )
  )

  diagnostics <- c(
    "1:9 XV015 malformed start tag",
    "1:10 XV019 duplicate attribute \"b\"",
    "1:5 XV020 malformed attribute",
    "1:6 XV020 malformed attribute",
    "1:7 XV023 \"<\" is not allowed in an attribute value",
    "1:4 XV015 malformed start tag",
    "1:3 XV015 malformed start tag",
    "3:3 XV019 duplicate attribute \"b\"",
    "1:13 XV021 invalid reference"
  )
  diagnostic_index <- 0L
  for (case in cases) {
    problem <- problem_of(case$text)
    diagnostic_index <- diagnostic_index + 1L
    expect_identical(
      problem,
      case[c("line", "col", "code", "message")]
    )
    expect_identical(
      paste0(
        problem$line, ":", problem$col, " ", problem$code, " ",
        problem$message
      ),
      diagnostics[[diagnostic_index]]
    )
  }

  expect_null(problem_of("<a  b = \"x y\"  ></a>"))
  expect_null(problem_of("<a b='&amp;'/>"))
})

test_that("references reject malformed and non-XML values", {
  cases <- list(
    list(
      text = "<a>&x;</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&amp</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&#0;</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&#X41;</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&#;</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&#x;</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&#123456789;</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&#xD800;</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a>&#x110000;</a>",
      line = 1L, col = 4L, code = "XV021",
      message = "invalid reference"
    ),
    list(
      text = "<a b=\"&x;\"/>",
      line = 1L, col = 7L, code = "XV021",
      message = "invalid reference"
    )
  )

  diagnostics <- c(
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:4 XV021 invalid reference",
    "1:7 XV021 invalid reference"
  )
  diagnostic_index <- 0L
  for (case in cases) {
    problem <- problem_of(case$text)
    diagnostic_index <- diagnostic_index + 1L
    expect_identical(
      problem,
      case[c("line", "col", "code", "message")]
    )
    expect_identical(
      paste0(
        problem$line, ":", problem$col, " ", problem$code, " ",
        problem$message
      ),
      diagnostics[[diagnostic_index]]
    )
  }

  expect_null(problem_of("<a>&amp;&lt;&gt;&quot;&apos;</a>"))
  expect_null(problem_of("<a>&#65;&#x41;&#x1f600;&#00065;</a>"))
  expect_identical(
    string_value(parse_document("<a>&#65;&#x41;</a>")$doc$root),
    "AA"
  )
  expect_null(
    problem_of("<a>&#00000000000065;&#x000000000041;</a>")
  )
})

test_that("markup readers report malformed content boundaries", {
  cases <- list(
    list(
      text = "<a><!-- -- --></a>",
      line = 1L, col = 9L, code = "XV013",
      message = "malformed comment"
    ),
    list(
      text = "<a><!--></a>",
      line = 1L, col = 4L, code = "XV013",
      message = "malformed comment"
    ),
    list(
      text = "<a><!-- x --->",
      line = 1L, col = 11L, code = "XV013",
      message = "malformed comment"
    ),
    list(
      text = "<!--->",
      line = 1L, col = 1L, code = "XV013",
      message = "malformed comment"
    ),
    list(
      text = "<a><![CDATA[x</a>",
      line = 1L, col = 4L, code = "XV014",
      message = "unterminated CDATA section"
    ),
    list(
      text = "<a><? p?></a>",
      line = 1L, col = 4L, code = "XV012",
      message = "malformed processing instruction"
    ),
    list(
      text = "<a><?p\"?></a>",
      line = 1L, col = 4L, code = "XV012",
      message = "malformed processing instruction"
    ),
    list(
      text = "<a><?xml?></a>",
      line = 1L, col = 4L, code = "XV005",
      message = "misplaced XML declaration"
    ),
    list(
      text = " <?xml version=\"1.0\"?><a/>",
      line = 1L, col = 2L, code = "XV005",
      message = "misplaced XML declaration"
    ),
    list(
      text = "<?XML x?><a/>",
      line = 1L, col = 1L, code = "XV011",
      message = "reserved processing instruction target \"XML\""
    ),
    list(
      text = "<a>]]></a>",
      line = 1L, col = 4L, code = "XV022",
      message = "\"]]>\" is not allowed in text"
    ),
    list(
      text = "",
      line = 1L, col = 1L, code = "XV002",
      message = "missing root element"
    ),
    list(
      text = " ",
      line = 1L, col = 2L, code = "XV002",
      message = "missing root element"
    ),
    list(
      text = "<a/>text",
      line = 1L, col = 5L, code = "XV003",
      message = "text outside the root element"
    ),
    list(
      text = "<a/><b/>",
      line = 1L, col = 5L, code = "XV004",
      message = "more than one root element"
    ),
    list(
      text = "</a>",
      line = 1L, col = 1L, code = "XV016",
      message = "malformed end tag"
    ),
    list(
      text = "<![CDATA[x]]><a/>",
      line = 1L, col = 1L, code = "XV003",
      message = "text outside the root element"
    ),
    list(
      text = "<>",
      line = 1L, col = 2L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a/><",
      line = 1L, col = 6L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a><",
      line = 1L, col = 5L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a><!x></a>",
      line = 1L, col = 4L, code = "XV015",
      message = "malformed start tag"
    )
  )

  diagnostics <- c(
    "1:9 XV013 malformed comment",
    "1:4 XV013 malformed comment",
    "1:11 XV013 malformed comment",
    "1:1 XV013 malformed comment",
    "1:4 XV014 unterminated CDATA section",
    "1:4 XV012 malformed processing instruction",
    "1:4 XV012 malformed processing instruction",
    "1:4 XV005 misplaced XML declaration",
    "1:2 XV005 misplaced XML declaration",
    "1:1 XV011 reserved processing instruction target \"XML\"",
    "1:4 XV022 \"]]>\" is not allowed in text",
    "1:1 XV002 missing root element",
    "1:2 XV002 missing root element",
    "1:5 XV003 text outside the root element",
    "1:5 XV004 more than one root element",
    "1:1 XV016 malformed end tag",
    "1:1 XV003 text outside the root element",
    "1:2 XV015 malformed start tag",
    "1:6 XV015 malformed start tag",
    "1:5 XV015 malformed start tag",
    "1:4 XV015 malformed start tag"
  )
  diagnostic_index <- 0L
  for (case in cases) {
    problem <- problem_of(case$text)
    diagnostic_index <- diagnostic_index + 1L
    expect_identical(
      problem,
      case[c("line", "col", "code", "message")]
    )
    expect_identical(
      paste0(
        problem$line, ":", problem$col, " ", problem$code, " ",
        problem$message
      ),
      diagnostics[[diagnostic_index]]
    )
  }

  expect_null(problem_of("<a><!----></a>"))
  expect_null(problem_of("<a><![CDATA[]]]]></a>"))
  expect_null(problem_of("<a><?p?><?q d?b?></a>"))
  expect_identical(read_cdata(make_doc("<![CDATA[x]]>"), 1L)$pos, 14L)
  expect_identical(read_pi(make_doc("<?p   d  e  ?>"), 1L)$pos, 15L)
})

test_that("invalid characters arbitrate against syntax stops", {
  cases <- list(
    list(
      text = "<a>\u0001</a>",
      line = 1L, col = 4L, code = "XV001",
      message = "invalid character U+0001"
    ),
    list(
      text = "\u0001<a",
      line = 1L, col = 1L, code = "XV001",
      message = "invalid character U+0001"
    ),
    list(
      text = "<!-- \u0001 -->",
      line = 1L, col = 6L, code = "XV001",
      message = "invalid character U+0001"
    ),
    list(
      text = "<a>\uFFFE</a>",
      line = 1L, col = 4L, code = "XV001",
      message = "invalid character U+FFFE"
    ),
    list(
      text = "<a></b>\u0001",
      line = 1L, col = 4L, code = "XV017",
      message = paste0(
        "end tag \"b\" does not match start tag \"a\" opened at 1:1"
      )
    ),
    list(
      text = "<a b=\"1\"c=\"2\">\u0001",
      line = 1L, col = 9L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<\u0001",
      line = 1L, col = 2L, code = "XV001",
      message = "invalid character U+0001"
    ),
    list(
      text = "<?xml version='1.1'?>\u0001",
      line = 1L, col = 1L, code = "XV007",
      message = "unsupported XML version \"1.1\""
    )
  )

  diagnostics <- c(
    "1:4 XV001 invalid character U+0001",
    "1:1 XV001 invalid character U+0001",
    "1:6 XV001 invalid character U+0001",
    "1:4 XV001 invalid character U+FFFE",
    "1:4 XV017 end tag \"b\" does not match start tag \"a\" opened at 1:1",
    "1:9 XV015 malformed start tag",
    "1:2 XV001 invalid character U+0001",
    "1:1 XV007 unsupported XML version \"1.1\""
  )
  diagnostic_index <- 0L
  for (case in cases) {
    problem <- problem_of(case$text)
    diagnostic_index <- diagnostic_index + 1L
    expect_identical(
      problem,
      case[c("line", "col", "code", "message")]
    )
    expect_identical(
      paste0(
        problem$line, ":", problem$col, " ", problem$code, " ",
        problem$message
      ),
      diagnostics[[diagnostic_index]]
    )
  }

  expect_null(problem_of("<a>\u007F\u0085</a>"))
  expect_null(problem_of("<a>\uFEFF</a>"))
  expect_null(problem_of("<a>\uD7FF\uE000\uFFD0\U00010000</a>"))
})
