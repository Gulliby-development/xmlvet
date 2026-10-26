test_that("comments return content and report malformed delimiters", {
  expect_identical(
    read_comment(make_doc("<!---->"), 1L),
    list(text = "", pos = 8L)
  )
  expect_identical(
    read_comment(make_doc("<!-- x -->"), 1L),
    list(text = " x ", pos = 11L)
  )
  for (case in list(
    list(text = "<!-- x", index = 1L),
    list(text = "<!--->", index = 1L),
    list(text = "<!-- x --", index = 8L),
    list(text = "<!-- x --->", index = 8L)
  )) {
    problem <- tryCatch(
      read_comment(make_doc(case$text), 1L),
      xmlvet_syntax = function(error) error
    )
    expect_s3_class(problem, "xmlvet_syntax")
    expect_identical(problem$index, case$index)
    expect_identical(problem$code, "XV013")
    expect_identical(conditionMessage(problem), "malformed comment")
  }
})

test_that("CDATA returns content through its first terminator", {
  expect_identical(
    read_cdata(make_doc("<![CDATA[x]]>"), 1L),
    list(text = "x", pos = 14L)
  )
  expect_identical(
    read_cdata(make_doc("<![CDATA[]]>"), 1L),
    list(text = "", pos = 13L)
  )
  problem <- tryCatch(
    read_cdata(make_doc("<![CDATA[x"), 1L),
    xmlvet_syntax = function(error) error
  )
  expect_s3_class(problem, "xmlvet_syntax")
  expect_identical(problem$index, 1L)
  expect_identical(problem$code, "XV014")
  expect_identical(
    conditionMessage(problem),
    "unterminated CDATA section"
  )
})

test_that("processing instructions retain targets and data", {
  expect_identical(
    read_pi(make_doc("<?p?>"), 1L),
    list(target = "p", data = "", pos = 6L)
  )
  expect_identical(
    read_pi(make_doc("<?p   d  e  ?>"), 1L),
    list(target = "p", data = "d  e  ", pos = 15L)
  )
  for (case in list(
    list(
      text = "<??>", code = "XV012",
      message = "malformed processing instruction"
    ),
    list(
      text = "<?p\"?>", code = "XV012",
      message = "malformed processing instruction"
    ),
    list(
      text = "<?p data", code = "XV012",
      message = "malformed processing instruction"
    ),
    list(
      text = "<?xml?>", code = "XV005",
      message = "misplaced XML declaration"
    ),
    list(
      text = "<?XML x?>", code = "XV011",
      message = "reserved processing instruction target \"XML\""
    )
  )) {
    problem <- tryCatch(
      read_pi(make_doc(case$text), 1L),
      xmlvet_syntax = function(error) error
    )
    expect_s3_class(problem, "xmlvet_syntax")
    expect_identical(problem$index, 1L)
    expect_identical(problem$code, case$code)
    expect_identical(conditionMessage(problem), case$message)
  }
})

test_that("XML declarations accept ordered optional fields", {
  expect_identical(
    read_declaration(make_doc("<?xml version='1.0'?><a/>")),
    22L
  )
  expect_true(
    parse_document(
      "<?xml version='1.0' encoding='Utf-8' standalone='no'?><a/>"
    )$ok
  )
  expect_true(
    parse_document(
      '<?xml  version = "1.0"  encoding = "UTF-8"  ?><a/>'
    )$ok
  )
  expect_true(
    parse_document("<?xml version='1.0' standalone='yes'?><a/>")$ok
  )
  expect_true(parse_document("<?xml-stylesheet href=\"s\"?><a/>")$ok)
})

test_that("XML declaration faults preserve grammar and value precedence", {
  for (case in list(
    list(
      text = "<?xml?><a/>", code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = '<?xml version="1.0"encoding="UTF-8"?><a/>',
      code = "XV006", message = "malformed XML declaration"
    ),
    list(
      text = '<?xml encoding="UTF-8" version="1.0"?><a/>',
      code = "XV006", message = "malformed XML declaration"
    ),
    list(
      text = '<?xml version="1.0" ', code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.0' version='1.0'?><a/>",
      code = "XV006", message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.0' unknown='x'?><a/>",
      code = "XV006", message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.0' standalone='maybe'?><a/>",
      code = "XV006", message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='2.0'?><a/>", code = "XV006",
      message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.0' encoding='8UTF'?><a/>",
      code = "XV006", message = "malformed XML declaration"
    ),
    list(
      text = "<?xml version='1.1' bad", code = "XV007",
      message = 'unsupported XML version "1.1"'
    ),
    list(
      text = "<?xml version='1.0' encoding='latin1' bad",
      code = "XV008", message = 'unsupported encoding "latin1"'
    )
  )) {
    problem <- tryCatch(
      read_declaration(make_doc(case$text)),
      xmlvet_syntax = function(error) error
    )
    expect_s3_class(problem, "xmlvet_syntax")
    expect_identical(problem$index, 1L)
    expect_identical(problem$code, case$code)
    expect_identical(conditionMessage(problem), case$message)
  }
})

test_that("DOCTYPE readers enforce the supported grammar", {
  expect_identical(
    read_doctype(make_doc("<!DOCTYPE a><a/>"), 1L),
    13L
  )
  expect_identical(read_doctype(make_doc("<!DOCTYPE  a  ><a/>"), 1L), 16L)
  expect_identical(
    read_doctype(make_doc('<!DOCTYPE a SYSTEM "x.dtd"><a/>'), 1L),
    28L
  )
  expect_identical(
    read_doctype(make_doc("<!DOCTYPE a SYSTEM 'x'><a/>"), 1L),
    24L
  )
  expect_true(parse_document("<!DOCTYPE b><a/>")$ok)
  for (case in list(
    list(
      text = "<!doctype a>", index = 1L, code = "XV009",
      message = "malformed DOCTYPE"
    ),
    list(
      text = "<!DOCTYPE a PUBLIC \"p\" \"x\">", index = 1L,
      code = "XV009", message = "malformed DOCTYPE"
    ),
    list(
      text = "<!DOCTYPE a", index = 1L, code = "XV009",
      message = "malformed DOCTYPE"
    ),
    list(
      text = "<!DOCTYPE a SYSTEM x>", index = 1L,
      code = "XV009", message = "malformed DOCTYPE"
    ),
    list(
      text = "<!DOCTYPE a [ ]>", index = 13L, code = "XV010",
      message = "DOCTYPE internal subset is not supported"
    )
  )) {
    problem <- tryCatch(
      read_doctype(make_doc(case$text), 1L),
      xmlvet_syntax = function(error) error
    )
    expect_s3_class(problem, "xmlvet_syntax")
    expect_identical(problem$index, case$index)
    expect_identical(problem$code, case$code)
    expect_identical(conditionMessage(problem), case$message)
  }
})
