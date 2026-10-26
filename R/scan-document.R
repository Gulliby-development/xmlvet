parse_document <- function(text) {
  doc <- make_doc(text)
  parsed <- tryCatch(
    {
      position <- 1L
      prolog <- list()
      epilog <- list()
      root <- NULL
      root_started <- FALSE
      doctype_seen <- FALSE
      ordinal <- 0L
      frames <- list()

      append_text_value <- function(current_frames, value, current_ordinal) {
        if (identical(value, "")) {
          list(frames = current_frames, ordinal = current_ordinal)
        } else {
          frame_index <- length(current_frames)
          frame <- current_frames[[frame_index]]
          kid_count <- length(frame$kids)
          has_text <- kid_count > 0L &&
            identical(frame$kids[[kid_count]]$type, "text")
          if (!has_text) {
            current_ordinal <- current_ordinal + 1L
          }
          frame$kids <- append_text(
            frame$kids,
            value,
            current_ordinal
          )
          current_frames[[frame_index]] <- frame
          list(frames = current_frames, ordinal = current_ordinal)
        }
      }

      attach_node <- function(current_frames, current_root, node) {
        if (length(current_frames) > 0L) {
          frame_index <- length(current_frames)
          frame <- current_frames[[frame_index]]
          frame$kids[[length(frame$kids) + 1L]] <- node
          current_frames[[frame_index]] <- frame
        } else {
          current_root <- node
        }
        list(frames = current_frames, root = current_root)
      }

      append_misc <- function(current_frames, current_prolog, current_epilog,
                              item, started) {
        if (length(current_frames) > 0L) {
          frame_index <- length(current_frames)
          frame <- current_frames[[frame_index]]
          frame$kids[[length(frame$kids) + 1L]] <- item
          current_frames[[frame_index]] <- frame
        } else if (started) {
          current_epilog[[length(current_epilog) + 1L]] <- item
        } else {
          current_prolog[[length(current_prolog) + 1L]] <- item
        }
        list(
          frames = current_frames,
          prolog = current_prolog,
          epilog = current_epilog
        )
      }

      if (doc_starts(doc, 1L, "<?xml")) {
        has_sixth_name <- doc$n >= 6L && doc$name_char[[6L]]
        if (!has_sixth_name) {
          position <- read_declaration(doc)
        }
      }

      done <- FALSE
      while (!done) {
        inside <- length(frames) > 0L
        if (inside && position > doc$n) {
          frame <- frames[[length(frames)]]
          syntax_fault(
            doc$n + 1L,
            "XV018",
            paste0(
              "unclosed element ",
              show_text(frame$node$name),
              " opened at ",
              frame$node$line,
              ":",
              frame$node$col
            )
          )
        }
        if (!inside) {
          position <- skip_space(doc, position)
          if (position > doc$n) {
            if (is.null(root)) {
              syntax_fault(
                doc$n + 1L,
                "XV002",
                "missing root element"
              )
            }
            done <- TRUE
          } else if (doc_at(doc, position) != 60L) {
            syntax_fault(
              position,
              "XV003",
              "text outside the root element"
            )
          }
        }
        if (!done && doc_at(doc, position) == 60L) {
          start <- position
          is_doctype <- doc_starts(doc, start, "<!") &&
            doc_word_at(doc, start + 2L, "doctype")
          if (doc_starts(doc, start, "<?")) {
            pi <- read_pi(doc, start)
            item <- new_pi(pi$target, pi$data)
            position <- pi$pos
            added <- append_misc(
              frames,
              prolog,
              epilog,
              item,
              root_started
            )
            frames <- added$frames
            prolog <- added$prolog
            epilog <- added$epilog
          } else if (doc_starts(doc, start, "<!--")) {
            comment <- read_comment(doc, start)
            item <- new_comment(comment$text)
            position <- comment$pos
            added <- append_misc(
              frames,
              prolog,
              epilog,
              item,
              root_started
            )
            frames <- added$frames
            prolog <- added$prolog
            epilog <- added$epilog
          } else if (is_doctype) {
            if (doctype_seen || root_started) {
              syntax_fault(start, "XV009", "malformed DOCTYPE")
            }
            position <- read_doctype(doc, start)
            doctype_seen <- TRUE
          } else if (doc_starts(doc, start, "<![CDATA[")) {
            if (!inside) {
              syntax_fault(
                start,
                "XV003",
                "text outside the root element"
              )
            }
            cdata <- read_cdata(doc, start)
            added <- append_text_value(frames, cdata$text, ordinal)
            frames <- added$frames
            ordinal <- added$ordinal
            position <- cdata$pos
          } else if (doc_starts(doc, start, "<!")) {
            syntax_fault(start, "XV015", "malformed start tag")
          } else if (doc_starts(doc, start, "</")) {
            if (!inside) {
              syntax_fault(start, "XV016", "malformed end tag")
            }
            name_start <- start + 2L
            name_end <- read_name(doc, name_start)
            if (name_end == name_start) {
              syntax_fault(name_start, "XV016", "malformed end tag")
            }
            end_name <- doc_text(doc, name_start, name_end - 1L)
            end_position <- skip_space(doc, name_end)
            if (doc_at(doc, end_position) != 62L) {
              syntax_fault(end_position, "XV016", "malformed end tag")
            }
            frame_index <- length(frames)
            frame <- frames[[frame_index]]
            if (!identical(end_name, frame$node$name)) {
              syntax_fault(
                start,
                "XV017",
                paste0(
                  "end tag ",
                  show_text(end_name),
                  " does not match start tag ",
                  show_text(frame$node$name),
                  " opened at ",
                  frame$node$line,
                  ":",
                  frame$node$col
                )
              )
            }
            node <- frame$node
            node$kids <- frame$kids
            frames[[frame_index]] <- NULL
            attached <- attach_node(frames, root, node)
            frames <- attached$frames
            root <- attached$root
            position <- end_position + 1L
          } else if (
            start < doc$n &&
              doc$name_start[[start + 1L]]
          ) {
            if (inside && length(frames) >= 100L) {
              syntax_fault(
                start,
                "XV024",
                "nesting is deeper than 100 levels"
              )
            }
            if (!inside && root_started) {
              syntax_fault(
                start,
                "XV004",
                "more than one root element"
              )
            }
            ordinal <- ordinal + 1L
            tag <- read_start_tag(doc, start, ordinal)
            position <- tag$pos
            root_started <- TRUE
            if (tag$empty) {
              attached <- attach_node(frames, root, tag$node)
              frames <- attached$frames
              root <- attached$root
            } else {
              frames[[length(frames) + 1L]] <- list(
                node = tag$node,
                kids = list()
              )
            }
          } else {
            syntax_fault(
              start + 1L,
              "XV015",
              "malformed start tag"
            )
          }
        } else if (!done && inside && doc_at(doc, position) == 38L) {
          reference <- read_reference(doc, position)
          added <- append_text_value(frames, reference$text, ordinal)
          frames <- added$frames
          ordinal <- added$ordinal
          position <- reference$pos
        } else if (!done && inside) {
          less_than <- doc_find(doc, position, "<")
          ampersand <- doc_find(doc, position, "&")
          terminator <- doc_find(doc, position, "]]>")
          boundaries <- c(less_than, ampersand)
          boundaries <- boundaries[!is.na(boundaries)]
          next_markup <- if (length(boundaries) > 0L) {
            min(boundaries)
          } else {
            doc$n + 1L
          }
          if (!is.na(terminator) && terminator < next_markup) {
            syntax_fault(
              terminator,
              "XV022",
              paste0(show_text("]]>"), " is not allowed in text")
            )
          }
          value <- doc_text(doc, position, next_markup - 1L)
          added <- append_text_value(frames, value, ordinal)
          frames <- added$frames
          ordinal <- added$ordinal
          position <- next_markup
        }
      }
      list(ok = TRUE, doc = list(prolog = prolog, root = root, epilog = epilog))
    },
    xmlvet_syntax = function(problem) problem
  )

  problem_at <- function(index, code, message) {
    list(
      ok = FALSE,
      line = doc$line[[index]],
      col = doc$col[[index]],
      code = code,
      message = message
    )
  }

  invalid_index <- doc$first_bad
  if (inherits(parsed, "xmlvet_syntax")) {
    syntax_index <- parsed$index
    invalid_wins <- !is.na(invalid_index) &&
      invalid_index <= syntax_index
    if (invalid_wins) {
      point <- doc$x[[invalid_index]]
      problem_at(
        invalid_index,
        "XV001",
        paste0("invalid character U+", sprintf("%04X", point))
      )
    } else {
      problem_at(syntax_index, parsed$code, conditionMessage(parsed))
    }
  } else if (!is.na(invalid_index)) {
    point <- doc$x[[invalid_index]]
    problem_at(
      invalid_index,
      "XV001",
      paste0("invalid character U+", sprintf("%04X", point))
    )
  } else {
    parsed
  }
}
