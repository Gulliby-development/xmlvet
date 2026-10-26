test_that("run_outline renders paths and validates requested depths", {
  xml <- "<r><a z=\"1\" b=\"2\" a=\"3\"><x/></a><a c=\"4\" b=\"5\"/></r>"
  all_paths <- paste0(
    "/r 1\n",
    "/r/a 2 @a @b @c @z\n",
    "/r/a/x 1\n"
  )
  inputs <- new_inputs(make_io(files = list("tree.xml" = xml)))

  expect_identical(
    run_outline("tree.xml", character(), list(), inputs),
    list(out = all_paths, code = 0)
  )
  expect_identical(
    run_outline("tree.xml", character(), list("--depth" = "1"), inputs),
    list(out = "/r 1\n", code = 0)
  )
  expect_identical(
    run_outline(
      "tree.xml",
      character(),
      list("--depth" = "2"),
      inputs
    ),
    list(
      out = paste0("/r 1\n", "/r/a 2 @a @b @c @z\n"),
      code = 0
    )
  )
  expect_identical(
    run_outline("tree.xml", character(), list("--depth" = "99"), inputs),
    list(out = all_paths, code = 0)
  )

  stdin_inputs <- new_inputs(make_io(stdin = "<s><i/><i/></s>"))
  expect_identical(
    run_outline("-", character(), list(), stdin_inputs),
    list(out = "/s 1\n/s/i 2\n", code = 0)
  )
})

test_that("run_outline checks arity and depth before reading input", {
  long_depth <- paste0(
    "99999999999999999999",
    "99999999999999999999",
    "99999999999999999999",
    "99999999999999999999",
    "99999999999999999999"
  )
  long_message <- paste0(
    "invalid --depth \"",
    "99999999999999999999",
    "99999999999999999999",
    "99999999999999999999",
    "99999999999999999999",
    "99999999999999999999",
    "\" (expected a whole number from 1 to 99)"
  )
  cases <- list(
    list(args = character(), depth = "0", expected = "missing FILE"),
    list(
      args = c("one.xml", "two.xml"),
      depth = "0",
      expected = "too many arguments"
    ),
    list(
      args = "missing.xml",
      depth = "",
      expected = paste0(
        "invalid --depth \"\" (expected a whole number from 1 to 99)"
      )
    ),
    list(
      args = "missing.xml",
      depth = "0",
      expected = "invalid --depth \"0\" (expected a whole number from 1 to 99)"
    ),
    list(
      args = "missing.xml",
      depth = "00",
      expected = "invalid --depth \"00\" (expected a whole number from 1 to 99)"
    ),
    list(
      args = "missing.xml",
      depth = "01",
      expected = "invalid --depth \"01\" (expected a whole number from 1 to 99)"
    ),
    list(
      args = "missing.xml",
      depth = "100",
      expected = paste0(
        "invalid --depth \"100\" ",
        "(expected a whole number from 1 to 99)"
      )
    ),
    list(
      args = "missing.xml",
      depth = "+1",
      expected = "invalid --depth \"+1\" (expected a whole number from 1 to 99)"
    ),
    list(
      args = "missing.xml",
      depth = "-1",
      expected = "invalid --depth \"-1\" (expected a whole number from 1 to 99)"
    ),
    list(
      args = "missing.xml",
      depth = " 1",
      expected = "invalid --depth \" 1\" (expected a whole number from 1 to 99)"
    ),
    list(
      args = "missing.xml",
      depth = "1 ",
      expected = "invalid --depth \"1 \" (expected a whole number from 1 to 99)"
    ),
    list(
      args = "missing.xml",
      depth = "1.0",
      expected = paste0(
        "invalid --depth \"1.0\" ",
        "(expected a whole number from 1 to 99)"
      )
    ),
    list(
      args = "missing.xml",
      depth = "1e1",
      expected = paste0(
        "invalid --depth \"1e1\" ",
        "(expected a whole number from 1 to 99)"
      )
    ),
    list(
      args = "missing.xml",
      depth = "\u0661",
      expected = "invalid --depth \"?\" (expected a whole number from 1 to 99)"
    ),
    list(args = "missing.xml", depth = long_depth, expected = long_message)
  )

  for (case in cases) {
    inputs <- new_inputs(make_io())
    expect_identical(
      failure_of(
        run_outline(
          case$args,
          character(),
          list("--depth" = case$depth),
          inputs
        )
      ),
      case$expected
    )
  }

  inputs <- new_inputs(make_io())
  expect_identical(
    failure_of(
      run_outline("missing.xml", character(), list(), inputs)
    ),
    "missing.xml: cannot read the file"
  )

  bad_inputs <- new_inputs(
    make_io(files = list("bad.xml" = "<a>\n  <b>\n</a>"))
  )
  expect_identical(
    failure_of(
      run_outline("bad.xml", character(), list(), bad_inputs)
    ),
    paste0(
      "bad.xml:3:1: XV017 end tag \"a\" does not match start tag \"b\" ",
      "opened at 2:3"
    )
  )

  stdin_inputs <- new_inputs(make_io(stdin = "<r/>"))
  expect_identical(
    failure_of(
      run_outline(
        "-",
        character(),
        list("--depth" = "00"),
        stdin_inputs
      )
    ),
    "invalid --depth \"00\" (expected a whole number from 1 to 99)"
  )
  expect_identical(
    run_outline("-", character(), list(), stdin_inputs),
    list(out = "/r 1\n", code = 0)
  )
})

test_that("run_get validates arity and path before reading input", {
  cases <- list(
    list(args = character(), expected = "missing PATH"),
    list(args = "/r", expected = "missing FILE"),
    list(
      args = c("/r", "one.xml", "two.xml"),
      expected = "too many arguments"
    ),
    list(
      args = c("/a[0]", "missing.xml"),
      expected = paste0(
        "invalid path \"/a[0]\": ",
        "a position or an attribute test is missing at character 4"
      )
    ),
    list(
      args = c("/a\u00E9|", "missing.xml"),
      expected = paste0(
        "invalid path \"/a?|\": unexpected character at character 4"
      )
    )
  )

  for (case in cases) {
    inputs <- new_inputs(make_io(stdin = "<r/>"))
    expect_identical(
      failure_of(run_get(case$args, character(), list(), inputs)),
      case$expected
    )
  }

  inputs <- new_inputs(make_io(stdin = "<r/>"))
  expect_identical(
    failure_of(
      run_get(c("/r[0]", "-"), character(), list(), inputs)
    ),
    paste0(
      "invalid path \"/r[0]\": ",
      "a position or an attribute test is missing at character 4"
    )
  )
  expect_identical(
    run_get(c("/r", "-"), character(), list(), inputs),
    list(out = "\n", code = 0)
  )
})

test_that("run_get propagates input and document failures", {
  cases <- list(
    list(
      args = c("/a", "missing.xml"),
      files = list(),
      expected = "missing.xml: cannot read the file"
    ),
    list(
      args = c("/a", "bad.xml"),
      files = list(
        "bad.xml" = as.raw(c(60L, 97L, 62L, 255L, 60L, 47L, 97L, 62L))
      ),
      expected = "bad.xml: input is not valid UTF-8"
    ),
    list(
      args = c("/absent", "bad.xml"),
      files = list("bad.xml" = "<a>\n  <b>\n</a>"),
      expected = paste0(
        "bad.xml:3:1: XV017 end tag \"a\" does not match start tag \"b\" ",
        "opened at 2:3"
      )
    )
  )

  for (case in cases) {
    inputs <- new_inputs(make_io(files = case$files))
    expect_identical(
      failure_of(run_get(case$args, character(), list(), inputs)),
      case$expected
    )
  }
})

test_that("run_get returns ordered root, descendant, and step selections", {
  xml <- paste0(
    "<r>root<i id=\"1\" k=\"a\">one</i>tail",
    "<i id=\"2\" k=\"b\">two</i>",
    "<group><i id=\"3\" k=\"a\">three</i></group>",
    "<i id=\"4\" k=\"a\">four</i></r>"
  )
  inputs <- new_inputs(make_io(files = list("tree.xml" = xml)))
  cases <- list(
    list(
      path = "/r",
      expected = list(out = "rootonetailtwothreefour\n", code = 0)
    ),
    list(
      path = "//i",
      expected = list(out = "one\ntwo\nthree\nfour\n", code = 0)
    ),
    list(
      path = "/r/i[@k='a'][2]",
      expected = list(out = "four\n", code = 0)
    ),
    list(
      path = "/r/i[2][@k='b']",
      expected = list(out = "two\n", code = 0)
    ),
    list(
      path = "//i/@id",
      expected = list(out = "1\n2\n3\n4\n", code = 0)
    ),
    list(
      path = "/r/text()",
      expected = list(out = "root\ntail\n", code = 0)
    )
  )

  for (case in cases) {
    expect_identical(
      run_get(c(case$path, "tree.xml"), character(), list(), inputs),
      case$expected
    )
  }
})

test_that("run_get distinguishes empty values and escapes selected text", {
  xml <- paste0(
    "<r><empty/><item value=\"\"/>",
    "<value>\\\\&#10;&#9;&#13;\u00E9</value>",
    "<item value=\"\\\\&#10;&#9;&#13;\u00E9\"/></r>"
  )
  escaped <- paste0("\\\\\\\\", "\\n", "\\t", "\\r", "\u00E9", "\n")
  inputs <- new_inputs(make_io(files = list("values.xml" = xml)))
  cases <- list(
    list(
      path = "/r/missing",
      expected = list(out = "", code = 1)
    ),
    list(
      path = "/r/empty",
      expected = list(out = "\n", code = 0)
    ),
    list(
      path = "/r/item[1]/@value",
      expected = list(out = "\n", code = 0)
    ),
    list(
      path = "/r/value/text()",
      expected = list(out = escaped, code = 0)
    ),
    list(
      path = "/r/item[2]/@value",
      expected = list(out = escaped, code = 0)
    )
  )

  for (case in cases) {
    expect_identical(
      run_get(c(case$path, "values.xml"), character(), list(), inputs),
      case$expected
    )
  }

  stdin_inputs <- new_inputs(make_io(stdin = "<r><x>stdin</x></r>"))
  expect_identical(
    run_get(c("//x", "-"), character(), list(), stdin_inputs),
    list(out = "stdin\n", code = 0)
  )
})
