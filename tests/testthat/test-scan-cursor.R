test_that("preparation drops U+FEFF and uses NA for no invalid character", {
  cases <- list(
    list(
      input = "\uFEFFa\r\nb\rc\t",
      expected = list(
        x = c(97L, 10L, 98L, 10L, 99L, 9L),
        n = 6L,
        line = c(1L, 1L, 2L, 2L, 3L, 3L, 3L),
        col = c(1L, 2L, 1L, 2L, 1L, 2L, 3L),
        name_start = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
        name_char = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
        first_bad = NA_integer_
      )
    ),
    list(
      input = "",
      expected = list(
        x = integer(0),
        n = 0L,
        line = 1L,
        col = 1L,
        name_start = logical(0),
        name_char = logical(0),
        first_bad = NA_integer_
      )
    ),
    list(
      input = "\uFEFF\uFEFFa",
      expected = list(
        x = c(65279L, 97L),
        n = 2L,
        line = c(1L, 1L, 1L),
        col = c(1L, 2L, 3L),
        name_start = c(TRUE, TRUE),
        name_char = c(TRUE, TRUE),
        first_bad = NA_integer_
      )
    ),
    list(
      input = "a\u0001",
      expected = list(
        x = c(97L, 1L),
        n = 2L,
        line = c(1L, 1L, 1L),
        col = c(1L, 2L, 3L),
        name_start = c(TRUE, FALSE),
        name_char = c(TRUE, FALSE),
        first_bad = 2L
      )
    )
  )
  for (case in cases) {
    expect_equal(make_doc(case$input), case$expected)
  }
})

test_that("cursor access, slicing, prefixes and search use code points", {
  doc <- make_doc("abc")
  expect_equal(doc_at(make_doc("ab"), 0), -1L)
  expect_equal(doc_at(make_doc("ab"), 3), -1L)
  expect_equal(doc_at(doc, 2), 98L)
  expect_equal(doc_text(doc, 2, 3), "bc")
  expect_equal(doc_text(doc, 3, 2), "")
  expect_equal(doc_text(doc, 0, 1), "a")
  expect_equal(doc_text(doc, 3, 9), "c")
  expect_true(doc_starts(doc, 2, "bc"))
  expect_false(doc_starts(doc, 2, "bd"))
  expect_true(doc_starts(doc, 4, ""))
  expect_false(doc_starts(doc, 4, "a"))
  expect_equal(doc_find(make_doc("ababa"), 2, "aba"), 3L)
  expect_true(is.na(doc_find(make_doc("ababa"), 2, "z")))
  expect_equal(doc_find(doc, 4, ""), 4L)
})

test_that("ASCII word matching and whitespace scanning are exact", {
  expect_true(doc_word_at(make_doc("DoCtYpE"), 1, "doctype"))
  expect_true(doc_word_at(make_doc("doctype"), 1, "DOCTYPE"))
  expect_true(doc_word_at(make_doc("doctypex"), 1, "doctype"))
  expect_false(doc_word_at(make_doc("doct"), 1, "doctype"))
  expect_true(doc_word_at(make_doc("abc"), 4, ""))
  expect_equal(skip_space(make_doc(" \t\na"), 1), 4L)
  expect_equal(skip_space(make_doc("a \tb"), 1), 1L)
  expect_equal(skip_space(make_doc(" \t\n"), 1), 4L)
  expect_equal(skip_space(make_doc(""), 1), 1L)
})

test_that("XML names test U+0300 (\\u0300) range boundaries", {
  expect_true(all(is_name_start(c(
    58L, 65L, 90L, 95L, 97L, 122L,
    192L, 214L, 216L, 246L, 248L, 767L,
    880L, 893L, 895L, 8191L, 8204L, 8205L,
    8304L, 8591L, 11264L, 12271L, 12289L, 55295L,
    63744L, 64975L, 65008L, 65533L, 65536L, 983039L
  ))))
  expect_true(all(is_name_char(c(
    58L, 65L, 90L, 95L, 97L, 122L,
    192L, 214L, 216L, 246L, 248L, 767L,
    880L, 893L, 895L, 8191L, 8204L, 8205L,
    8304L, 8591L, 11264L, 12271L, 12289L, 55295L,
    63744L, 64975L, 65008L, 65533L, 65536L, 983039L
  ))))
  expect_false(any(is_name_start(c(
    57L, 64L, 91L, 94L, 96L, 123L,
    191L, 215L, 247L, 768L, 879L, 894L, 894L,
    8192L, 8203L, 8206L, 8303L, 8592L, 11263L,
    12272L, 12288L, 55296L, 63743L, 64976L,
    65007L, 65534L, 983040L
  ))))
  expect_true(all(is_name_char(c(
    45L, 46L, 48L, 57L, 183L, 768L, 879L, 8255L, 8256L
  ))))
  expect_false(any(is_name_char(c(
    44L, 47L, 59L, 182L, 184L, 8254L, 8257L
  ))))
  expect_equal(read_name(make_doc("a-b.c:d!"), 1), 8L)
  expect_equal(read_name(make_doc("1a"), 1), 1L)
  expect_equal(read_name(make_doc("\u00C0!"), 1), 2L)
  expect_equal(read_name(make_doc("\U00010000!"), 1), 2L)
  expect_equal(read_name(make_doc("\u0300a"), 1), 1L)
  expect_equal(read_name(make_doc("a\u0300!"), 1), 3L)
  expect_equal(make_doc("\u0300")$name_start, FALSE)
})
