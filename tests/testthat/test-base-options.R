test_that("empty options and positional arguments are preserved", {
  expect_equal(
    parse_options(character(), character(), character()),
    list(flags = character(), settings = list(), args = character())
  )
  expect_equal(
    parse_options(c("-", "-5"), character(), character()),
    list(flags = character(), settings = list(), args = c("-", "-5"))
  )
})

test_that("flags are unique and retain first-seen order", {
  expect_equal(
    parse_options(
      c("--quiet", "x", "--quiet", "-", "-5"),
      flags = "--quiet",
      valued = "--depth"
    ),
    list(
      flags = "--quiet",
      settings = list(),
      args = c("x", "-", "-5")
    )
  )
  expect_equal(
    parse_options(
      c("--other", "--quiet", "--other"),
      flags = c("--quiet", "--other"),
      valued = character()
    ),
    list(
      flags = c("--other", "--quiet"),
      settings = list(),
      args = character()
    )
  )
})

test_that("equals values, repeats, and last values are handled", {
  expect_equal(
    parse_options(c("--depth=1", "--depth", "2"), character(), "--depth"),
    list(
      flags = character(),
      settings = list("--depth" = "2"),
      args = character()
    )
  )
  expect_equal(
    parse_options("--depth=", character(), "--depth"),
    list(
      flags = character(),
      settings = list("--depth" = ""),
      args = character()
    )
  )
  expect_equal(
    parse_options("--depth=a=b", character(), "--depth"),
    list(
      flags = character(),
      settings = list("--depth" = "a=b"),
      args = character()
    )
  )
})

test_that("valued options consume the next argument and take precedence", {
  expect_equal(
    parse_options(c("--depth", "--quiet"), "--quiet", "--depth"),
    list(
      flags = character(),
      settings = list("--depth" = "--quiet"),
      args = character()
    )
  )
  expect_equal(
    parse_options(c("--depth", "-5"), "--quiet", "--depth"),
    list(
      flags = character(),
      settings = list("--depth" = "-5"),
      args = character()
    )
  )
  expect_equal(
    parse_options(c("--quiet", "yes"), "--quiet", "--quiet"),
    list(
      flags = character(),
      settings = list("--quiet" = "yes"),
      args = character()
    )
  )
})

test_that("missing and unknown options fail in order with safe diagnostics", {
  expect_equal(
    failure_of(parse_options("--depth", character(), "--depth")),
    "--depth needs a value"
  )
  expect_equal(
    failure_of(parse_options("--", character(), "--depth")),
    'unknown option "--"'
  )
  expect_equal(
    failure_of(parse_options("--help", character(), "--depth")),
    'unknown option "--help"'
  )
  expect_equal(
    failure_of(parse_options("--bare=1", character(), "--depth")),
    'unknown option "--bare=1"'
  )
  expect_equal(
    failure_of(parse_options("--Quiet", character(), "--depth")),
    'unknown option "--Quiet"'
  )
  expect_equal(
    failure_of(
      parse_options(c("--bad", "--depth"), character(), "--depth")
    ),
    'unknown option "--bad"'
  )
  expect_equal(
    failure_of(parse_options("--d\u00e9pth", character(), "--depth")),
    'unknown option "--d?pth"'
  )
})
