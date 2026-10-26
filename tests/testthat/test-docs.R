test_that("README explains purpose, installation, usage, and status codes", {
  readme <- paste(
    readLines(test_path("..", "..", "README.md"), warn = FALSE),
    collapse = "\n"
  )

  expect_true(
    grepl(
      paste0(
        "xmlvet is an offline command-line tool for people who write ",
        "and review XML by\nhand."
      ),
      readme,
      fixed = TRUE
    )
  )
  expect_true(grepl("Use R 4.1 or newer.", readme, fixed = TRUE))
  expect_true(
    grepl(
      "Nothing needs to be installed: run the script from the\nproject folder",
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "`xmlvet` stands for `Rscript exec/xmlvet.R`",
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      paste0(
        "usage: xmlvet check [--quiet] FILE...\n",
        "       xmlvet canon [--comments] FILE\n",
        "       xmlvet outline [--depth N] FILE\n",
        "       xmlvet get PATH FILE\n",
        "       xmlvet diff [--ignore-space] FILE FILE"
      ),
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      paste0(
        "Exit status is 0 for success, 1 when `check` finds a problem, `get` ",
        "selects no\nvalues, or `diff` finds a difference, and 2 for an error."
      ),
      readme,
      fixed = TRUE
    )
  )
})

test_that("README fixtures and command examples match main output", {
  readme <- paste(
    readLines(test_path("..", "..", "README.md"), warn = FALSE),
    collapse = "\n"
  )
  files <- list(
    "small.xml" = "<a x=\"1\"><b>t</b><b/></a>",
    "other.xml" = "<a x=\"2\"><b>t</b><b/></a>",
    "bad.xml" = "<a>\n  <b>\n</a>",
    "catalog.xml" = paste0(
      "<?xml version=\"1.0\"?>\n",
      "<catalog>\n",
      "  <book id=\"b2\" lang=\"en\">Tom &amp; Jerry</book>\n",
      "  <book lang=\"fr\" id=\"b1\"><title>Plain</title></book>\n",
      "</catalog>\n"
    )
  )

  expect_true(
    grepl(
      "printf '%s' '<a x=\"1\"><b>t</b><b/></a>' > small.xml",
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "printf '%s' '<a x=\"2\"><b>t</b><b/></a>' > other.xml",
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl("printf '<a>\\n  <b>\\n</a>' > bad.xml", readme, fixed = TRUE)
  )
  expect_true(
    grepl(
      "printf '%s\\n' '<?xml version=\"1.0\"?>'",
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl("xmlvet check catalog.xml bad.xml small.xml", readme, fixed = TRUE)
  )
  expect_true(grepl("xmlvet canon catalog.xml", readme, fixed = TRUE))
  expect_true(
    grepl("xmlvet outline --depth 1 catalog.xml", readme, fixed = TRUE)
  )
  expect_true(grepl("xmlvet get //book/@id catalog.xml", readme, fixed = TRUE))
  expect_true(grepl("xmlvet diff small.xml other.xml", readme, fixed = TRUE))

  expect_true(
    grepl(
      paste0(
        "catalog.xml: ok, 4 elements\n",
        "bad.xml:3:1: XV017 end tag \"a\" does not match start tag \"b\" ",
        "opened at 2:3\n",
        "small.xml: ok, 3 elements"
      ),
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      paste0(
        "<catalog>\n",
        "  <book id=\"b2\" lang=\"en\">Tom &amp; Jerry</book>\n",
        "  <book id=\"b1\" lang=\"fr\"><title>Plain</title></book>\n",
        "</catalog>"
      ),
      readme,
      fixed = TRUE
    )
  )
  expect_true(grepl("/catalog 1", readme, fixed = TRUE))
  expect_true(grepl("b2\nb1", readme, fixed = TRUE))
  expect_true(
    grepl(
      "- /a[1]/@x=\"1\"\n+ /a[1]/@x=\"2\"",
      readme,
      fixed = TRUE
    )
  )

  expect_identical(
    run_main(c("check", "catalog.xml", "bad.xml", "small.xml"), files),
    list(
      code = 1L,
      out = paste0(
        "catalog.xml: ok, 4 elements\n",
        "bad.xml:3:1: XV017 end tag \"a\" does not match start tag \"b\" ",
        "opened at 2:3\n",
        "small.xml: ok, 3 elements\n"
      ),
      err = ""
    )
  )
  expect_identical(
    run_main(c("canon", "catalog.xml"), files),
    list(
      code = 0,
      out = paste0(
        "<catalog>\n",
        "  <book id=\"b2\" lang=\"en\">Tom &amp; Jerry</book>\n",
        "  <book id=\"b1\" lang=\"fr\"><title>Plain</title></book>\n",
        "</catalog>\n"
      ),
      err = ""
    )
  )
  expect_identical(
    run_main(c("outline", "--depth", "1", "catalog.xml"), files),
    list(code = 0, out = "/catalog 1\n", err = "")
  )
  expect_identical(
    run_main(c("get", "//book/@id", "catalog.xml"), files),
    list(code = 0, out = "b2\nb1\n", err = "")
  )
  expect_identical(
    run_main(c("diff", "small.xml", "other.xml"), files),
    list(
      code = 1L,
      out = "- /a[1]/@x=\"1\"\n+ /a[1]/@x=\"2\"\n",
      err = ""
    )
  )
})

test_that("README states XML limits; LICENSE is MIT", {
  readme <- paste(
    readLines(test_path("..", "..", "README.md"), warn = FALSE),
    collapse = "\n"
  )
  license <- readLines(test_path("..", "..", "LICENSE"), warn = FALSE)

  expect_true(
    grepl(
      "UTF-8 XML 1.0 and the five predefined XML entities",
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl(
      "names such as `a:b` are treated literally",
      readme,
      fixed = TRUE
    )
  )
  expect_true(
    grepl("A\nDOCTYPE is checked and skipped", readme, fixed = TRUE)
  )
  expect_true(
    grepl("an internal subset is not supported", readme, fixed = TRUE)
  )
  expect_true(
    grepl("maximum element nesting depth is 100", readme, fixed = TRUE)
  )
  expect_true(
    grepl(
      paste0(
        "The program writes LF line endings. On Windows, R may ",
        "translate output LF to\nCR LF."
      ),
      readme,
      fixed = TRUE
    )
  )
  expect_true(grepl("testthat::test_local", readme, fixed = TRUE))
  expect_true(grepl("lintr::lint_package()", readme, fixed = TRUE))
  expect_identical(
    license,
    c(
      "MIT License",
      "",
      "Copyright (c) 2026 Gulliby-development",
      "",
      paste0(
        "Permission is hereby granted, free of charge, to any person ",
        "obtaining a copy"
      ),
      paste0(
        "of this software and associated documentation files (the ",
        "\"Software\"), to deal"
      ),
      paste0(
        "in the Software without restriction, including without limitation ",
        "the rights"
      ),
      paste0(
        "to use, copy, modify, merge, publish, distribute, sublicense, and/or ",
        "sell"
      ),
      "copies of the Software, and to permit persons to whom the Software is",
      paste0(
        "furnished to do so, subject to the following ",
        "conditions:"
      ),
      "",
      paste0(
        "The above copyright notice and this permission notice shall be ",
        "included in all"
      ),
      "copies or substantial portions of the Software.",
      "",
      paste0(
        "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, ",
        "EXPRESS OR"
      ),
      paste0(
        "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF ",
        "MERCHANTABILITY,"
      ),
      paste0(
        "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO ",
        "EVENT SHALL THE"
      ),
      paste0(
        "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES ",
        "OR OTHER"
      ),
      paste0(
        "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ",
        "ARISING FROM,"
      ),
      paste0(
        "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER ",
        "DEALINGS IN THE"
      ),
      "SOFTWARE."
    )
  )
})
