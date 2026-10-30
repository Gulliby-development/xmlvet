catalog_xml <- paste0(
  "<?xml version=\"1.0\"?>\n",
  "<catalog>\n",
  "  <book id=\"b2\" lang=\"en\">Tom &amp; Jerry</book>\n",
  "  <book lang=\"fr\" id=\"b1\"><title>Plain</title></book>\n",
  "</catalog>\n"
)
bad_xml <- "<a>\n  <b>\n</a>"

get_count_rows <- list(
  list(
    name = "counts the selected attributes",
    argv = c("get", "--count", "//book/@id", "catalog.xml"),
    stdin = FALSE,
    code = 0L,
    out = "2\n",
    err = ""
  ),
  list(
    name = "the option may follow the path",
    argv = c("get", "//book/@id", "--count", "catalog.xml"),
    stdin = FALSE,
    code = 0L,
    out = "2\n",
    err = ""
  ),
  list(
    name = "the option may come last",
    argv = c("get", "//book", "catalog.xml", "--count"),
    stdin = FALSE,
    code = 0L,
    out = "2\n",
    err = ""
  ),
  list(
    name = "repeating the option changes nothing",
    argv = c("get", "--count", "--count", "//book", "catalog.xml"),
    stdin = FALSE,
    code = 0L,
    out = "2\n",
    err = ""
  ),
  list(
    name = "counts one element",
    argv = c("get", "--count", "//title", "catalog.xml"),
    stdin = FALSE,
    code = 0L,
    out = "1\n",
    err = ""
  ),
  list(
    name = "counts every element",
    argv = c("get", "--count", "//*", "catalog.xml"),
    stdin = FALSE,
    code = 0L,
    out = "4\n",
    err = ""
  ),
  list(
    name = "no value prints zero and exits 1",
    argv = c("get", "--count", "//missing", "catalog.xml"),
    stdin = FALSE,
    code = 1L,
    out = "0\n",
    err = ""
  ),
  list(
    name = "a document on standard input",
    argv = c("get", "--count", "//book", "-"),
    stdin = TRUE,
    code = 0L,
    out = "2\n",
    err = ""
  ),
  list(
    name = "without the option the values are printed",
    argv = c("get", "//book/@id", "catalog.xml"),
    stdin = FALSE,
    code = 0L,
    out = "b2\nb1\n",
    err = ""
  ),
  list(
    name = "without the option no value exits 1 with no output",
    argv = c("get", "//missing", "catalog.xml"),
    stdin = FALSE,
    code = 1L,
    out = "",
    err = ""
  ),
  list(
    name = "a malformed document is an error",
    argv = c("get", "--count", "//a", "bad.xml"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = paste0(
      "xmlvet: bad.xml:3:1: XV017 end tag \"a\" does not match start tag \"b",
      "\" opened at 2:3\n"
    )
  ),
  list(
    name = "an invalid path is an error",
    argv = c("get", "--count", "//book[", "catalog.xml"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = paste0(
      "xmlvet: invalid path \"//book[\": a position or an attribute test is ",
      "missing at character 8\n"
    )
  ),
  list(
    name = "a missing file is an error",
    argv = c("get", "--count", "//book", "nofile.xml"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = "xmlvet: nofile.xml: cannot read the file\n"
  ),
  list(
    name = "missing path",
    argv = c("get", "--count"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = "xmlvet: missing PATH\n"
  ),
  list(
    name = "missing file",
    argv = c("get", "--count", "//book"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = "xmlvet: missing FILE\n"
  ),
  list(
    name = "too many arguments",
    argv = c("get", "--count", "//book", "catalog.xml", "extra"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = "xmlvet: too many arguments\n"
  ),
  list(
    name = "a value after the option name is an unknown option",
    argv = c("get", "--count=1", "//book", "catalog.xml"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = "xmlvet: unknown option \"--count=1\"\n"
  ),
  list(
    name = "check rejects the option",
    argv = c("check", "--count", "catalog.xml"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = "xmlvet: unknown option \"--count\"\n"
  ),
  list(
    name = "diff rejects the option",
    argv = c("diff", "--count", "catalog.xml", "catalog.xml"),
    stdin = FALSE,
    code = 2L,
    out = "",
    err = "xmlvet: unknown option \"--count\"\n"
  )
)

test_that("get --count prints how many values the path selects", {
  files <- list("catalog.xml" = catalog_xml, "bad.xml" = bad_xml)
  for (row in get_count_rows) {
    stdin <- if (row$stdin) catalog_xml else NULL
    result <- run_main(row$argv, files = files, stdin = stdin)
    expect_equal(result$code, row$code, info = row$name)
    expect_identical(result$out, row$out, info = row$name)
    expect_identical(result$err, row$err, info = row$name)
  }
})
