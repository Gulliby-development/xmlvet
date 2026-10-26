test_that("path steps preserve axes and node tests", {
  cases <- list(
    list(
      path = "/a",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list()
      ))
    ),
    list(
      path = "//a/b",
      expected = list(
        list(
          axis = "descendant",
          test = "element",
          name = "a",
          predicates = list()
        ),
        list(
          axis = "child",
          test = "element",
          name = "b",
          predicates = list()
        )
      )
    ),
    list(
      path = "/a//b",
      expected = list(
        list(
          axis = "child",
          test = "element",
          name = "a",
          predicates = list()
        ),
        list(
          axis = "descendant",
          test = "element",
          name = "b",
          predicates = list()
        )
      )
    ),
    list(
      path = "/*",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "*",
        predicates = list()
      ))
    ),
    list(
      path = "//@id",
      expected = list(list(
        axis = "descendant",
        test = "attribute",
        name = "id",
        predicates = list()
      ))
    ),
    list(
      path = "/a/text()",
      expected = list(
        list(
          axis = "child",
          test = "element",
          name = "a",
          predicates = list()
        ),
        list(
          axis = "child",
          test = "text",
          name = NULL,
          predicates = list()
        )
      )
    ),
    list(
      path = "//text()",
      expected = list(list(
        axis = "descendant",
        test = "text",
        name = NULL,
        predicates = list()
      ))
    ),
    list(
      path = "/text",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "text",
        predicates = list()
      ))
    )
  )
  for (case in cases) {
    expect_identical(parse_path(case$path), case$expected, info = case$path)
  }
})

test_that("predicates preserve types, values, and source order", {
  cases <- list(
    list(
      path = "/a[1]",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(type = "position", value = 1L))
      ))
    ),
    list(
      path = "//a[2]/b[1]",
      expected = list(
        list(
          axis = "descendant",
          test = "element",
          name = "a",
          predicates = list(list(type = "position", value = 2L))
        ),
        list(
          axis = "child",
          test = "element",
          name = "b",
          predicates = list(list(type = "position", value = 1L))
        )
      )
    ),
    list(
      path = "/a[@id]",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(type = "has_attribute", name = "id"))
      ))
    ),
    list(
      path = "/a[@id='v']",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(
          type = "attribute_equals",
          name = "id",
          value = "v"
        ))
      ))
    ),
    list(
      path = "/a[@id=\"v\"]",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(
          type = "attribute_equals",
          name = "id",
          value = "v"
        ))
      ))
    ),
    list(
      path = "/a[@id='']",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(
          type = "attribute_equals",
          name = "id",
          value = ""
        ))
      ))
    ),
    list(
      path = "/a[1][@id]",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(
          list(type = "position", value = 1L),
          list(type = "has_attribute", name = "id")
        )
      ))
    ),
    list(
      path = "/a[@x][@y=\"1\"]",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(
          list(type = "has_attribute", name = "x"),
          list(type = "attribute_equals", name = "y", value = "1")
        )
      ))
    ),
    list(
      path = "/a[999999]",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(type = "position", value = 999999L))
      ))
    )
  )
  for (case in cases) {
    expect_identical(parse_path(case$path), case$expected, info = case$path)
  }
})

test_that("XML names and quoted contents are literal code points", {
  cases <- list(
    list(
      path = "/a-b.c:d",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a-b.c:d",
        predicates = list()
      ))
    ),
    list(
      path = "/\u00E9/@x:y",
      expected = list(
        list(
          axis = "child",
          test = "element",
          name = "\u00E9",
          predicates = list()
        ),
        list(
          axis = "child",
          test = "attribute",
          name = "x:y",
          predicates = list()
        )
      )
    ),
    list(
      path = "/a[@x='&amp;']",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(
          type = "attribute_equals",
          name = "x",
          value = "&amp;"
        ))
      ))
    ),
    list(
      path = "/a[@x='a/b[]']",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(
          type = "attribute_equals",
          name = "x",
          value = "a/b[]"
        ))
      ))
    ),
    list(
      path = "/a[@x='a\\b']",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(
          type = "attribute_equals",
          name = "x",
          value = "a\\b"
        ))
      ))
    ),
    list(
      path = "/a[@x='\n']",
      expected = list(list(
        axis = "child",
        test = "element",
        name = "a",
        predicates = list(list(
          type = "attribute_equals",
          name = "x",
          value = "\n"
        ))
      ))
    )
  )
  for (case in cases) {
    expect_identical(parse_path(case$path), case$expected, info = case$path)
  }
  expect_identical(
    path_fault_of(parse_path("/\u00E9 b")),
    list(pos = 3L, message = "unexpected character")
  )
})

test_that("invalid path starts and step separators have exact faults", {
  cases <- list(
    list(
      path = "",
      expected = list(pos = 1L, message = "a path starts with \"/\"")
    ),
    list(
      path = "a",
      expected = list(pos = 1L, message = "a path starts with \"/\"")
    ),
    list(path = "/", expected = list(
      pos = 2L,
      message = "a step is missing"
    )),
    list(path = "//", expected = list(
      pos = 3L,
      message = "a step is missing"
    )),
    list(path = "/a/", expected = list(
      pos = 4L,
      message = "a step is missing"
    )),
    list(path = "///a", expected = list(
      pos = 3L,
      message = "a step is missing"
    )),
    list(path = "/1", expected = list(
      pos = 2L,
      message = "unexpected character"
    )),
    list(path = "/-", expected = list(
      pos = 2L,
      message = "unexpected character"
    )),
    list(path = "/ ", expected = list(
      pos = 2L,
      message = "unexpected character"
    )),
    list(path = "/|", expected = list(
      pos = 2L,
      message = "unexpected character"
    )),
    list(path = "/a b", expected = list(
      pos = 3L,
      message = "unexpected character"
    )),
    list(path = "/@", expected = list(
      pos = 3L,
      message = "an attribute name is missing"
    ))
  )
  for (case in cases) {
    expect_identical(path_fault_of(parse_path(case$path)), case$expected)
  }
})

test_that("text and attribute steps are terminal", {
  cases <- list(
    list(path = "/text()[1]", expected = list(
      pos = 8L,
      message = "a predicate is not allowed here"
    )),
    list(path = "/@id[1]", expected = list(
      pos = 5L,
      message = "a predicate is not allowed here"
    )),
    list(path = "/@id/a", expected = list(
      pos = 5L,
      message = "nothing may follow a text() or attribute step"
    )),
    list(path = "/text()/a", expected = list(
      pos = 8L,
      message = "nothing may follow a text() or attribute step"
    ))
  )
  for (case in cases) {
    expect_identical(path_fault_of(parse_path(case$path)), case$expected)
  }
})

test_that("predicate syntax faults have exact precedence and positions", {
  cases <- list(
    list(
      path = "/a[0]",
      message = "a position or an attribute test is missing",
      pos = 4L
    ),
    list(
      path = "/a[-1]",
      message = "a position or an attribute test is missing",
      pos = 4L
    ),
    list(
      path = "/a[]",
      message = "a position or an attribute test is missing",
      pos = 4L
    ),
    list(
      path = "/a[ ]",
      message = "a position or an attribute test is missing",
      pos = 4L
    ),
    list(
      path = "/a[x]",
      message = "a position or an attribute test is missing",
      pos = 4L
    ),
    list(
      path = "/a[",
      message = "a position or an attribute test is missing",
      pos = 4L
    ),
    list(path = "/a[@]", message = "an attribute name is missing", pos = 5L),
    list(path = "/a[@", message = "an attribute name is missing", pos = 5L),
    list(path = "/a[1234567]", message = "the position is too large", pos = 4L),
    list(path = "/a[1", message = "a predicate is not closed", pos = 3L),
    list(path = "/a[@id", message = "a predicate is not closed", pos = 3L),
    list(path = "/a[@id='v'", message = "a predicate is not closed", pos = 3L),
    list(path = "/a[1]]", message = "unexpected character", pos = 6L),
    list(path = "/a[1x]", message = "unexpected character", pos = 5L),
    list(path = "/a[@id=1]", message = "a quoted value is missing", pos = 8L),
    list(path = "/a[@id=", message = "a quoted value is missing", pos = 8L),
    list(
      path = "/a[@id='v]",
      message = "a quoted value is not closed",
      pos = 8L
    ),
    list(path = "/a[@id='v'x]", message = "unexpected character", pos = 11L)
  )
  for (case in cases) {
    expect_identical(
      path_fault_of(parse_path(case$path)),
      list(pos = case$pos, message = case$message),
      info = case$path
    )
  }
})
