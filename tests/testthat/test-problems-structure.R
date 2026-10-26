test_that("outside-root and markup dispatch problems are literal", {
  cases <- list(
    list(
      text = "", line = 1L, col = 1L, code = "XV002",
      message = "missing root element"
    ),
    list(
      text = " ", line = 1L, col = 2L, code = "XV002",
      message = "missing root element"
    ),
    list(
      text = "<a/>text", line = 1L, col = 5L, code = "XV003",
      message = "text outside the root element"
    ),
    list(
      text = "<a/><b/>", line = 1L, col = 5L, code = "XV004",
      message = "more than one root element"
    ),
    list(
      text = "</a>", line = 1L, col = 1L, code = "XV016",
      message = "malformed end tag"
    ),
    list(
      text = "<![CDATA[x]]><a/>", line = 1L, col = 1L, code = "XV003",
      message = "text outside the root element"
    ),
    list(
      text = "<>", line = 1L, col = 2L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a/><", line = 1L, col = 6L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a><", line = 1L, col = 5L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a><!x></a>", line = 1L, col = 4L, code = "XV015",
      message = "malformed start tag"
    ),
    list(
      text = "<a>]]></a>", line = 1L, col = 4L, code = "XV022",
      message = "\"]]>\" is not allowed in text"
    )
  )
  for (case in cases) {
    expect_identical(
      problem_of(case$text),
      list(
        line = case$line,
        col = case$col,
        code = case$code,
        message = case$message
      )
    )
  }
  expect_null(problem_of("<a>]]</a>"))
})

test_that("end-tag and prepared-position problems are literal", {
  cases <- list(
    list(
      text = "<a>", line = 1L, col = 4L, code = "XV018",
      message = "unclosed element \"a\" opened at 1:1"
    ),
    list(
      text = "<a><b></a>", line = 1L, col = 7L, code = "XV017",
      message = paste0(
        "end tag \"a\" does not match start tag ",
        "\"b\" opened at 1:4"
      )
    ),
    list(
      text = "<a></b>", line = 1L, col = 4L, code = "XV017",
      message = paste0(
        "end tag \"b\" does not match start tag ",
        "\"a\" opened at 1:1"
      )
    ),
    list(
      text = "<a>\n<b>\n</a>", line = 3L, col = 1L, code = "XV017",
      message = paste0(
        "end tag \"a\" does not match start tag ",
        "\"b\" opened at 2:1"
      )
    ),
    list(
      text = "<a>\r\n<b>\r\n</a>", line = 3L, col = 1L, code = "XV017",
      message = paste0(
        "end tag \"a\" does not match start tag ",
        "\"b\" opened at 2:1"
      )
    ),
    list(
      text = "\n\n  <a>\t<b></c></a>", line = 3L, col = 10L,
      code = "XV017",
      message = paste0(
        "end tag \"c\" does not match start tag ",
        "\"b\" opened at 3:7"
      ),
      display = paste0(
        "3:10 XV017 end tag \"c\" does not match start tag ",
        "\"b\" opened at 3:7"
      )
    ),
    list(
      text = "\uFEFF<a></b>", line = 1L, col = 4L, code = "XV017",
      message = paste0(
        "end tag \"b\" does not match start tag ",
        "\"a\" opened at 1:1"
      )
    ),
    list(
      text = "<a></a b>", line = 1L, col = 8L, code = "XV016",
      message = "malformed end tag",
      display = "1:8 XV016 malformed end tag"
    ),
    list(
      text = "<a></>", line = 1L, col = 6L, code = "XV016",
      message = "malformed end tag",
      display = "1:6 XV016 malformed end tag"
    ),
    list(
      text = "<a></a", line = 1L, col = 7L, code = "XV016",
      message = "malformed end tag"
    )
  )
  for (case in cases) {
    expect_identical(
      problem_of(case$text),
      list(
        line = case$line,
        col = case$col,
        code = case$code,
        message = case$message
      )
    )
    if (!is.null(case$display)) {
      expect_identical(
        paste0(
          case$line, ":", case$col, " ", case$code, " ", case$message
        ),
        case$display
      )
    }
  }
})

test_that("declaration and processing-instruction problems are literal", {
  cases <- list(
    list(
      text = "<a><?xml?></a>", line = 1L, col = 4L, code = "XV005",
      message = "misplaced XML declaration"
    ),
    list(
      text = " <?xml version=\"1.0\"?><a/>", line = 1L, col = 2L,
      code = "XV005", message = "misplaced XML declaration"
    ),
    list(
      text = "<?XML x?><a/>", line = 1L, col = 1L, code = "XV011",
      message = "reserved processing instruction target \"XML\""
    ),
    list(
      text = "<a><? p?></a>", line = 1L, col = 4L, code = "XV012",
      message = "malformed processing instruction"
    ),
    list(
      text = "<a><?p\"?></a>", line = 1L, col = 4L, code = "XV012",
      message = "malformed processing instruction"
    ),
    list(
      text = "<?xml?><a/>", line = 1L, col = 1L, code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version=\"1.0\"encoding=\"UTF-8\"?><a/>",
      line = 1L, col = 1L, code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml encoding=\"UTF-8\" version=\"1.0\"?><a/>",
      line = 1L, col = 1L, code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version=\"1.0\" ", line = 1L, col = 1L,
      code = "XV006", message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.0' version='1.0'?><a/>", line = 1L,
      col = 1L, code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.0' unknown='x'?><a/>", line = 1L,
      col = 1L, code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.0' standalone='maybe'?><a/>", line = 1L,
      col = 1L, code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.0' encoding='8UTF'?><a/>", line = 1L,
      col = 1L, code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version=\"1.1\"?><a/>", line = 1L, col = 1L,
      code = "XV007", message = "unsupported XML version \"1.1\""
    ),
    list(
      text = "<?xml version='1.1' bad", line = 1L, col = 1L,
      code = "XV007", message = "unsupported XML version \"1.1\""
    ),
    list(
      text = "<?xml version=\"2.0\"?><a/>", line = 1L, col = 1L,
      code = "XV006", message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version=\"1.0\" encoding=\"latin1\"?><a/>",
      line = 1L, col = 1L, code = "XV008",
      message = "unsupported encoding \"latin1\""
    ),
    list(
      text = "<?xml version='1.0' encoding='latin1' bad", line = 1L,
      col = 1L, code = "XV008",
      message = "unsupported encoding \"latin1\""
    )
  )
  for (case in cases) {
    expect_identical(
      problem_of(case$text),
      list(
        line = case$line,
        col = case$col,
        code = case$code,
        message = case$message
      )
    )
  }
  expect_identical(
    read_declaration(make_doc("<?xml version='1.0'?><a/>")),
    22L
  )
  expect_null(
    problem_of("<?xml version='1.0' encoding='Utf-8' standalone='no'?><a/>")
  )
  expect_null(
    problem_of("<?xml  version = \"1.0\"  encoding = \"UTF-8\"  ?><a/>")
  )
  expect_null(problem_of("<?xml version='1.0' standalone='yes'?><a/>"))
  expect_null(problem_of("<?xml-stylesheet href=\"s\"?><a/>"))
  expect_null(problem_of("<a><?p?><?q d?b?></a>"))
})

test_that("DOCTYPE syntax and placement problems are literal", {
  cases <- list(
    list(
      text = "<!DOCTYPE a><!DOCTYPE a><a/>", line = 1L, col = 13L,
      code = "XV009", message = "malformed DOCTYPE",
      display = "1:13 XV009 malformed DOCTYPE"
    ),
    list(
      text = "<a/><!DOCTYPE a>", line = 1L, col = 5L, code = "XV009",
      message = "malformed DOCTYPE",
      display = "1:5 XV009 malformed DOCTYPE"
    ),
    list(
      text = "<a><!DOCTYPE a></a>", line = 1L, col = 4L, code = "XV009",
      message = "malformed DOCTYPE"
    ),
    list(
      text = "<!doctype a><a/>", line = 1L, col = 1L, code = "XV009",
      message = "malformed DOCTYPE"
    ),
    list(
      text = "<!DOCTYPE a PUBLIC \"p\" \"x\"><a/>", line = 1L, col = 1L,
      code = "XV009", message = "malformed DOCTYPE"
    ),
    list(
      text = "<!DOCTYPE a [ ]><a/>", line = 1L, col = 13L, code = "XV010",
      message = "DOCTYPE internal subset is not supported",
      display = "1:13 XV010 DOCTYPE internal subset is not supported"
    )
  )
  for (case in cases) {
    expect_identical(
      problem_of(case$text),
      list(
        line = case$line,
        col = case$col,
        code = case$code,
        message = case$message
      )
    )
    if (!is.null(case$display)) {
      expect_identical(
        paste0(
          case$line, ":", case$col, " ", case$code, " ", case$message
        ),
        case$display
      )
    }
  }
  expect_identical(
    read_doctype(make_doc("<!DOCTYPE a><a/>"), 1L),
    13L
  )
  expect_null(problem_of("<!DOCTYPE  a  ><a/>"))
  expect_null(problem_of("<!DOCTYPE a SYSTEM \"x.dtd\"><a/>"))
  expect_null(problem_of("<!DOCTYPE a SYSTEM 'x'><a/>"))
  expect_null(problem_of("<!DOCTYPE b><a/>"))
})
