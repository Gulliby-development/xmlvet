test_that("run_canon serializes named and stdin inputs", {
  file_io <- make_io(
    files = list("named.xml" = "<a z=\"1\" b=\"2\"><b/></a>")
  )
  named <- run_canon(
    "named.xml",
    character(),
    list(),
    new_inputs(file_io)
  )
  expect_identical(
    named,
    list(out = "<a b=\"2\" z=\"1\"><b></b></a>\n", code = 0)
  )
  expect_identical(file_io$state$out, "")
  expect_identical(file_io$state$err, "")

  commented <- "<!--before--><?p d?><a><!--inside-->x</a><?q?><!--after-->"
  plain_io <- make_io(stdin = commented)
  plain <- run_canon("-", character(), list(), new_inputs(plain_io))
  expect_identical(
    plain,
    list(out = "<?p d?>\n<a>x</a>\n<?q?>\n", code = 0)
  )
  expect_identical(plain_io$state$out, "")
  expect_identical(plain_io$state$err, "")

  comments_io <- make_io(stdin = commented)
  with_comments <- run_canon(
    "-",
    "--comments",
    list(),
    new_inputs(comments_io)
  )
  expect_identical(
    with_comments,
    list(
      out = paste0(
        "<!--before-->\n<?p d?>\n",
        "<a><!--inside-->x</a>\n<?q?>\n<!--after-->\n"
      ),
      code = 0
    )
  )
  expect_identical(comments_io$state$out, "")
  expect_identical(comments_io$state$err, "")
})

test_that("run_canon validates arity before reading", {
  state <- new.env(parent = emptyenv())
  state$reads <- character()
  reader <- new_inputs(make_io(files = list("doc.xml" = "<a/>")))
  inputs <- list(
    read = function(arg) {
      state$reads <- c(state$reads, arg)
      reader$read(arg)
    }
  )

  expect_identical(
    failure_of(run_canon(character(), character(), list(), inputs)),
    "missing FILE"
  )
  expect_length(state$reads, 0L)

  expect_identical(
    failure_of(
      run_canon(c("doc.xml", "doc.xml"), character(), list(), inputs)
    ),
    "too many arguments"
  )
  expect_length(state$reads, 0L)

  result <- run_canon("doc.xml", character(), list(), inputs)
  expect_identical(result, list(out = "<a></a>\n", code = 0))
  expect_identical(state$reads, "doc.xml")
})

test_that("run_canon preserves parse and input failures", {
  file_io <- make_io(files = list("bad.xml" = "<a>\n  <b>\n</a>"))
  file_failure <- failure_of(
    run_canon("bad.xml", character(), list(), new_inputs(file_io))
  )
  expect_identical(
    file_failure,
    paste0(
      "bad.xml:3:1: XV017 end tag \"a\" does not match start tag ",
      "\"b\" opened at 2:3"
    )
  )

  stdin_io <- make_io(stdin = "<a></b>")
  stdin_failure <- failure_of(
    run_canon("-", character(), list(), new_inputs(stdin_io))
  )
  expect_identical(
    stdin_failure,
    paste0(
      "<stdin>:1:4: XV017 end tag \"b\" does not match start tag ",
      "\"a\" opened at 1:1"
    )
  )

  missing_failure <- failure_of(
    run_canon("missing.xml", character(), list(), new_inputs(make_io()))
  )
  expect_identical(missing_failure, "missing.xml: cannot read the file")
})
