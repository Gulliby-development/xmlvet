test_that("text escaping follows the canonical map", {
  expect_identical(escape_text(""), "")
  expect_identical(
    escape_text("&<>\r\n\t\"'\\\u00E9\U0001F600"),
    "&amp;&lt;&gt;&#xD;\n\t\"'\\\u00E9\U0001F600"
  )
  expect_identical(escape_text("&amp;"), "&amp;amp;")
})

test_that("attribute escaping follows its distinct canonical map", {
  expect_identical(escape_attr(""), "")
  expect_identical(
    escape_attr("&<\"\t\n\r>'\\\u00E9\U0001F600"),
    "&amp;&lt;&quot;&#x9;&#xA;&#xD;>'\\\u00E9\U0001F600"
  )
  expect_identical(escape_attr("&quot;"), "&amp;quot;")
})

test_that("elements sort paired attributes and close every element", {
  ascii <- new_element(
    "a", c("z", "b", "B", "a"), c("1", "2", "3", "4"), list(),
    1L, 1L, 1L
  )
  expect_identical(
    canon_node(ascii, FALSE),
    "<a B=\"3\" a=\"4\" b=\"2\" z=\"1\"></a>"
  )

  unicode <- new_element(
    "a", c("\U00010000", "\u00E9", "aa", "a", "Z"),
    c("5", "4", "3", "2", "1"), list(), 1L, 1L, 1L
  )
  expect_identical(
    canon_node(unicode, FALSE),
    "<a Z=\"1\" a=\"2\" aa=\"3\" \u00E9=\"4\" \U00010000=\"5\"></a>"
  )

  nested <- new_element(
    "a", character(), character(),
    list(
      new_element("b", character(), character(), list(), 1L, 4L, 2L),
      new_element("c", character(), character(), list(), 1L, 8L, 3L)
    ),
    1L, 1L, 1L
  )
  expect_identical(
    canon_node(nested, FALSE),
    "<a><b></b><c></c></a>"
  )

  escaped <- new_element(
    "a", "b", "1\t2\n3\r4", list(new_text("\r\n\t", 2L)),
    1L, 1L, 1L
  )
  expect_identical(
    canon_node(escaped, FALSE),
    "<a b=\"1&#x9;2&#xA;3&#xD;4\">&#xD;\n\t</a>"
  )
})

test_that("processing instructions and comments serialize literally", {
  expect_identical(canon_node(new_pi("p", ""), FALSE), "<?p?>")
  expect_identical(
    canon_node(new_pi("p", "d  e  "), FALSE),
    "<?p d  e  ?>"
  )
  expect_identical(canon_node(new_pi("q", "d?b"), TRUE), "<?q d?b?>")
  expect_identical(canon_node(new_comment("c"), FALSE), "")
  expect_identical(canon_node(new_comment("c"), TRUE), "<!--c-->")
  expect_identical(canon_node(new_comment(""), TRUE), "<!---->")
})

test_that("documents filter comments before joining retained items", {
  document <- list(
    prolog = list(new_comment("a"), new_pi("p", "d"), new_comment("b")),
    root = new_element(
      "a", character(), character(),
      list(new_text("x", 1L), new_comment("c"), new_text("y", 2L)),
      1L, 1L, 1L
    ),
    epilog = list(new_pi("q", ""), new_comment("d"))
  )
  expect_identical(
    canon_document(document, FALSE),
    "<?p d?>\n<a>xy</a>\n<?q?>"
  )
  expect_identical(
    canon_document(document, TRUE),
    "<!--a-->\n<?p d?>\n<!--b-->\n<a>x<!--c-->y</a>\n<?q?>\n<!--d-->"
  )

  root_only <- list(
    prolog = list(),
    root = new_element("a", character(), character(), list(), 1L, 1L, 1L),
    epilog = list()
  )
  expect_identical(canon_document(root_only, FALSE), "<a></a>")
  expect_identical(canon_document(root_only, TRUE), "<a></a>")

  comments_only_outside <- list(
    prolog = list(new_comment("x")),
    root = new_element("a", character(), character(), list(), 1L, 1L, 1L),
    epilog = list(new_comment("y"))
  )
  expect_identical(
    canon_document(comments_only_outside, FALSE),
    "<a></a>"
  )
})

test_that("mixed nested traversal preserves order without mutation", {
  node <- new_element(
    "a", c("z", "b"), c("last", "first"),
    list(
      new_text("A&", 1L),
      new_element(
        "b", character(), character(),
        list(
          new_text("B<", 2L),
          new_element(
            "c", character(), character(),
            list(new_text("C>", 3L)), 1L, 6L, 4L
          )
        ),
        1L, 4L, 2L
      ),
      new_pi("p", "inside"),
      new_text("D", 5L),
      new_comment("note")
    ),
    1L, 1L, 1L
  )
  original <- node

  expect_identical(
    canon_node(node, FALSE),
    "<a b=\"first\" z=\"last\">A&amp;<b>B&lt;<c>C&gt;</c></b><?p inside?>D</a>"
  )
  expect_identical(
    canon_node(node, TRUE),
    paste0(
      "<a b=\"first\" z=\"last\">A&amp;<b>B&lt;<c>C&gt;</c></b>",
      "<?p inside?>D<!--note--></a>"
    )
  )
  expect_identical(canon_node(new_text("x & y", 1L), FALSE), "x &amp; y")
  expect_identical(node, original)
})
