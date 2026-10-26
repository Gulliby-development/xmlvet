test_that("count_paths preserves first path appearance", {
  first_title <- new_element(
    "title", character(), character(), list(), 1L, 1L, 3L
  )
  first_book <- new_element(
    "book", c("id", "lang"), c("b1", "en"), list(first_title),
    1L, 1L, 2L
  )
  second_title <- new_element(
    "title", character(), character(), list(), 1L, 1L, 5L
  )
  author <- new_element("author", "a", "1", list(), 1L, 1L, 6L)
  second_book <- new_element(
    "book", "id", "b2", list(second_title, author), 1L, 1L, 4L
  )
  note <- new_element("note", character(), character(), list(), 1L, 1L, 7L)
  catalog <- new_element(
    "catalog", character(), character(),
    list(first_book, second_book, note), 1L, 1L, 1L
  )

  expect_identical(
    count_paths(catalog),
    list(
      paths = c(
        "/catalog", "/catalog/book", "/catalog/book/title",
        "/catalog/book/author", "/catalog/note"
      ),
      levels = c(1L, 2L, 3L, 3L, 2L),
      counts = c(1L, 2L, 2L, 1L, 1L),
      attrs = list(
        character(), c("id", "lang"), character(), "a",
        character()
      )
    )
  )

  nested_a <- new_element("a", character(), character(), list(), 1L, 1L, 3L)
  branch <- new_element(
    "b", character(), character(), list(nested_a), 1L, 1L, 2L
  )
  direct_a <- new_element(
    "a", character(), character(), list(), 1L, 1L, 4L
  )
  repeated_root <- new_element(
    "a", character(), character(), list(branch, direct_a), 1L, 1L, 1L
  )

  expect_identical(
    count_paths(repeated_root),
    list(
      paths = c("/a", "/a/b", "/a/b/a", "/a/a"),
      levels = c(1L, 2L, 3L, 2L),
      counts = c(1L, 1L, 1L, 1L),
      attrs = list(character(), character(), character(), character())
    )
  )
})

test_that("count_paths unions attribute names in first-seen order", {
  first_item <- new_element(
    "i", c("z", "a"), c("1", "2"), list(), 1L, 1L, 2L
  )
  second_item <- new_element(
    "i", c("a", "B"), c("3", "4"), list(), 1L, 1L, 3L
  )
  third_item <- new_element(
    "i", "z", "5", list(), 1L, 1L, 4L
  )
  root <- new_element(
    "r", character(), character(),
    list(first_item, second_item, third_item), 1L, 1L, 1L
  )

  expect_identical(
    count_paths(root),
    list(
      paths = c("/r", "/r/i"),
      levels = c(1L, 2L),
      counts = c(1L, 3L),
      attrs = list(character(), c("z", "a", "B"))
    )
  )
})

test_that("count_paths ignores non-elements and traverses 100 levels", {
  text_before <- new_text("x", 2L)
  comment <- new_comment("c")
  instruction <- new_pi("p", "d")
  child <- new_element("b", character(), character(), list(), 1L, 1L, 5L)
  text_after <- new_text("y", 6L)
  mixed_root <- new_element(
    "a", character(), character(),
    list(text_before, comment, instruction, child, text_after), 1L, 1L, 1L
  )

  expect_identical(
    count_paths(mixed_root),
    list(
      paths = c("/a", "/a/b"),
      levels = c(1L, 2L),
      counts = c(1L, 1L),
      attrs = list(character(), character())
    )
  )

  level_100 <- new_element(
    "a", character(), character(), list(), 1L, 1L, 100L
  )

  level_99 <- new_element(
    "a", character(), character(), list(level_100), 1L, 1L, 99L
  )

  level_98 <- new_element(
    "a", character(), character(), list(level_99), 1L, 1L, 98L
  )

  level_97 <- new_element(
    "a", character(), character(), list(level_98), 1L, 1L, 97L
  )

  level_96 <- new_element(
    "a", character(), character(), list(level_97), 1L, 1L, 96L
  )

  level_95 <- new_element(
    "a", character(), character(), list(level_96), 1L, 1L, 95L
  )

  level_94 <- new_element(
    "a", character(), character(), list(level_95), 1L, 1L, 94L
  )

  level_93 <- new_element(
    "a", character(), character(), list(level_94), 1L, 1L, 93L
  )

  level_92 <- new_element(
    "a", character(), character(), list(level_93), 1L, 1L, 92L
  )

  level_91 <- new_element(
    "a", character(), character(), list(level_92), 1L, 1L, 91L
  )

  level_90 <- new_element(
    "a", character(), character(), list(level_91), 1L, 1L, 90L
  )

  level_89 <- new_element(
    "a", character(), character(), list(level_90), 1L, 1L, 89L
  )

  level_88 <- new_element(
    "a", character(), character(), list(level_89), 1L, 1L, 88L
  )

  level_87 <- new_element(
    "a", character(), character(), list(level_88), 1L, 1L, 87L
  )

  level_86 <- new_element(
    "a", character(), character(), list(level_87), 1L, 1L, 86L
  )

  level_85 <- new_element(
    "a", character(), character(), list(level_86), 1L, 1L, 85L
  )

  level_84 <- new_element(
    "a", character(), character(), list(level_85), 1L, 1L, 84L
  )

  level_83 <- new_element(
    "a", character(), character(), list(level_84), 1L, 1L, 83L
  )

  level_82 <- new_element(
    "a", character(), character(), list(level_83), 1L, 1L, 82L
  )

  level_81 <- new_element(
    "a", character(), character(), list(level_82), 1L, 1L, 81L
  )

  level_80 <- new_element(
    "a", character(), character(), list(level_81), 1L, 1L, 80L
  )

  level_79 <- new_element(
    "a", character(), character(), list(level_80), 1L, 1L, 79L
  )

  level_78 <- new_element(
    "a", character(), character(), list(level_79), 1L, 1L, 78L
  )

  level_77 <- new_element(
    "a", character(), character(), list(level_78), 1L, 1L, 77L
  )

  level_76 <- new_element(
    "a", character(), character(), list(level_77), 1L, 1L, 76L
  )

  level_75 <- new_element(
    "a", character(), character(), list(level_76), 1L, 1L, 75L
  )

  level_74 <- new_element(
    "a", character(), character(), list(level_75), 1L, 1L, 74L
  )

  level_73 <- new_element(
    "a", character(), character(), list(level_74), 1L, 1L, 73L
  )

  level_72 <- new_element(
    "a", character(), character(), list(level_73), 1L, 1L, 72L
  )

  level_71 <- new_element(
    "a", character(), character(), list(level_72), 1L, 1L, 71L
  )

  level_70 <- new_element(
    "a", character(), character(), list(level_71), 1L, 1L, 70L
  )

  level_69 <- new_element(
    "a", character(), character(), list(level_70), 1L, 1L, 69L
  )

  level_68 <- new_element(
    "a", character(), character(), list(level_69), 1L, 1L, 68L
  )

  level_67 <- new_element(
    "a", character(), character(), list(level_68), 1L, 1L, 67L
  )

  level_66 <- new_element(
    "a", character(), character(), list(level_67), 1L, 1L, 66L
  )

  level_65 <- new_element(
    "a", character(), character(), list(level_66), 1L, 1L, 65L
  )

  level_64 <- new_element(
    "a", character(), character(), list(level_65), 1L, 1L, 64L
  )

  level_63 <- new_element(
    "a", character(), character(), list(level_64), 1L, 1L, 63L
  )

  level_62 <- new_element(
    "a", character(), character(), list(level_63), 1L, 1L, 62L
  )

  level_61 <- new_element(
    "a", character(), character(), list(level_62), 1L, 1L, 61L
  )

  level_60 <- new_element(
    "a", character(), character(), list(level_61), 1L, 1L, 60L
  )

  level_59 <- new_element(
    "a", character(), character(), list(level_60), 1L, 1L, 59L
  )

  level_58 <- new_element(
    "a", character(), character(), list(level_59), 1L, 1L, 58L
  )

  level_57 <- new_element(
    "a", character(), character(), list(level_58), 1L, 1L, 57L
  )

  level_56 <- new_element(
    "a", character(), character(), list(level_57), 1L, 1L, 56L
  )

  level_55 <- new_element(
    "a", character(), character(), list(level_56), 1L, 1L, 55L
  )

  level_54 <- new_element(
    "a", character(), character(), list(level_55), 1L, 1L, 54L
  )

  level_53 <- new_element(
    "a", character(), character(), list(level_54), 1L, 1L, 53L
  )

  level_52 <- new_element(
    "a", character(), character(), list(level_53), 1L, 1L, 52L
  )

  level_51 <- new_element(
    "a", character(), character(), list(level_52), 1L, 1L, 51L
  )

  level_50 <- new_element(
    "a", character(), character(), list(level_51), 1L, 1L, 50L
  )

  level_49 <- new_element(
    "a", character(), character(), list(level_50), 1L, 1L, 49L
  )

  level_48 <- new_element(
    "a", character(), character(), list(level_49), 1L, 1L, 48L
  )

  level_47 <- new_element(
    "a", character(), character(), list(level_48), 1L, 1L, 47L
  )

  level_46 <- new_element(
    "a", character(), character(), list(level_47), 1L, 1L, 46L
  )

  level_45 <- new_element(
    "a", character(), character(), list(level_46), 1L, 1L, 45L
  )

  level_44 <- new_element(
    "a", character(), character(), list(level_45), 1L, 1L, 44L
  )

  level_43 <- new_element(
    "a", character(), character(), list(level_44), 1L, 1L, 43L
  )

  level_42 <- new_element(
    "a", character(), character(), list(level_43), 1L, 1L, 42L
  )

  level_41 <- new_element(
    "a", character(), character(), list(level_42), 1L, 1L, 41L
  )

  level_40 <- new_element(
    "a", character(), character(), list(level_41), 1L, 1L, 40L
  )

  level_39 <- new_element(
    "a", character(), character(), list(level_40), 1L, 1L, 39L
  )

  level_38 <- new_element(
    "a", character(), character(), list(level_39), 1L, 1L, 38L
  )

  level_37 <- new_element(
    "a", character(), character(), list(level_38), 1L, 1L, 37L
  )

  level_36 <- new_element(
    "a", character(), character(), list(level_37), 1L, 1L, 36L
  )

  level_35 <- new_element(
    "a", character(), character(), list(level_36), 1L, 1L, 35L
  )

  level_34 <- new_element(
    "a", character(), character(), list(level_35), 1L, 1L, 34L
  )

  level_33 <- new_element(
    "a", character(), character(), list(level_34), 1L, 1L, 33L
  )

  level_32 <- new_element(
    "a", character(), character(), list(level_33), 1L, 1L, 32L
  )

  level_31 <- new_element(
    "a", character(), character(), list(level_32), 1L, 1L, 31L
  )

  level_30 <- new_element(
    "a", character(), character(), list(level_31), 1L, 1L, 30L
  )

  level_29 <- new_element(
    "a", character(), character(), list(level_30), 1L, 1L, 29L
  )

  level_28 <- new_element(
    "a", character(), character(), list(level_29), 1L, 1L, 28L
  )

  level_27 <- new_element(
    "a", character(), character(), list(level_28), 1L, 1L, 27L
  )

  level_26 <- new_element(
    "a", character(), character(), list(level_27), 1L, 1L, 26L
  )

  level_25 <- new_element(
    "a", character(), character(), list(level_26), 1L, 1L, 25L
  )

  level_24 <- new_element(
    "a", character(), character(), list(level_25), 1L, 1L, 24L
  )

  level_23 <- new_element(
    "a", character(), character(), list(level_24), 1L, 1L, 23L
  )

  level_22 <- new_element(
    "a", character(), character(), list(level_23), 1L, 1L, 22L
  )

  level_21 <- new_element(
    "a", character(), character(), list(level_22), 1L, 1L, 21L
  )

  level_20 <- new_element(
    "a", character(), character(), list(level_21), 1L, 1L, 20L
  )

  level_19 <- new_element(
    "a", character(), character(), list(level_20), 1L, 1L, 19L
  )

  level_18 <- new_element(
    "a", character(), character(), list(level_19), 1L, 1L, 18L
  )

  level_17 <- new_element(
    "a", character(), character(), list(level_18), 1L, 1L, 17L
  )

  level_16 <- new_element(
    "a", character(), character(), list(level_17), 1L, 1L, 16L
  )

  level_15 <- new_element(
    "a", character(), character(), list(level_16), 1L, 1L, 15L
  )

  level_14 <- new_element(
    "a", character(), character(), list(level_15), 1L, 1L, 14L
  )

  level_13 <- new_element(
    "a", character(), character(), list(level_14), 1L, 1L, 13L
  )

  level_12 <- new_element(
    "a", character(), character(), list(level_13), 1L, 1L, 12L
  )

  level_11 <- new_element(
    "a", character(), character(), list(level_12), 1L, 1L, 11L
  )

  level_10 <- new_element(
    "a", character(), character(), list(level_11), 1L, 1L, 10L
  )

  level_9 <- new_element(
    "a", character(), character(), list(level_10), 1L, 1L, 9L
  )

  level_8 <- new_element(
    "a", character(), character(), list(level_9), 1L, 1L, 8L
  )

  level_7 <- new_element(
    "a", character(), character(), list(level_8), 1L, 1L, 7L
  )

  level_6 <- new_element(
    "a", character(), character(), list(level_7), 1L, 1L, 6L
  )

  level_5 <- new_element(
    "a", character(), character(), list(level_6), 1L, 1L, 5L
  )

  level_4 <- new_element(
    "a", character(), character(), list(level_5), 1L, 1L, 4L
  )

  level_3 <- new_element(
    "a", character(), character(), list(level_4), 1L, 1L, 3L
  )

  level_2 <- new_element(
    "a", character(), character(), list(level_3), 1L, 1L, 2L
  )

  level_1 <- new_element(
    "a", character(), character(), list(level_2), 1L, 1L, 1L
  )

  deep_found <- NULL
  expect_silent(deep_found <- count_paths(level_1))
  expect_identical(deep_found$levels[[100L]], 100L)
  expect_identical(deep_found$counts[[100L]], 1L)
})

test_that("render_outline filters inclusive depths and ends lines with LF", {
  deep <- new_element("d", character(), character(), list(), 1L, 1L, 4L)
  first_c <- new_element(
    "c", character(), character(), list(deep), 1L, 1L, 3L
  )
  second_c <- new_element("c", character(), character(), list(), 1L, 1L, 5L)
  first_b <- new_element(
    "b", character(), character(), list(first_c, second_c), 1L, 1L, 2L
  )
  third_c <- new_element("c", "z", "3", list(), 1L, 1L, 7L)
  second_b <- new_element(
    "b", c("x", "y"), c("1", "2"), list(third_c), 1L, 1L, 6L
  )
  e <- new_element("e", character(), character(), list(), 1L, 1L, 8L)
  root <- new_element(
    "a", character(), character(), list(first_b, second_b, e),
    1L, 1L, 1L
  )
  found <- count_paths(root)

  expect_identical(
    render_outline(found, NULL),
    "/a 1\n/a/b 2 @x @y\n/a/b/c 3 @z\n/a/b/c/d 1\n/a/e 1\n"
  )
  expect_identical(
    render_outline(found, 2),
    "/a 1\n/a/b 2 @x @y\n/a/e 1\n"
  )
  expect_identical(render_outline(found, 1), "/a 1\n")
  expect_identical(
    render_outline(
      list(
        paths = character(), levels = integer(), counts = integer(),
        attrs = list()
      ),
      NULL
    ),
    ""
  )

  catalog_title_one <- new_element(
    "title", character(), character(), list(), 1L, 1L, 3L
  )
  catalog_book_one <- new_element(
    "book", c("id", "lang"), c("b1", "en"), list(catalog_title_one),
    1L, 1L, 2L
  )
  catalog_title_two <- new_element(
    "title", character(), character(), list(), 1L, 1L, 5L
  )
  catalog_author <- new_element(
    "author", "a", "1", list(), 1L, 1L, 6L
  )
  catalog_book_two <- new_element(
    "book", "id", "b2", list(catalog_title_two, catalog_author),
    1L, 1L, 4L
  )
  catalog_note <- new_element(
    "note", character(), character(), list(), 1L, 1L, 7L
  )
  catalog <- new_element(
    "catalog", character(), character(),
    list(catalog_book_one, catalog_book_two, catalog_note), 1L, 1L, 1L
  )

  expect_identical(
    render_outline(count_paths(catalog), NULL),
    paste0(
      "/catalog 1\n",
      "/catalog/book 2 @id @lang\n",
      "/catalog/book/title 2\n",
      "/catalog/book/author 1 @a\n",
      "/catalog/note 1\n"
    )
  )
})

test_that("render_outline sorts attributes by code point without mutation", {
  root <- new_element(
    "a:b",
    c("z", "\u00e9", "a", "B", "\U00010000", "\ue000", "aa"),
    c("z", "e", "a", "b", "high", "private", "aa"),
    list(), 1L, 1L, 1L
  )
  root_before <- root
  found <- count_paths(root)
  found_before <- found
  expected <- "/a:b 1 @B @a @aa @z @\u00e9 @\ue000 @\U00010000\n"

  first_render <- render_outline(found, NULL)
  second_render <- render_outline(found, NULL)
  found_again <- count_paths(root)

  expect_identical(first_render, expected)
  expect_identical(second_render, expected)
  expect_identical(found_again, found_before)
  expect_identical(root, root_before)
  expect_identical(found, found_before)
})
