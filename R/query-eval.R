query_index <- function(root) {
  count <- 0L
  pending <- list(root)

  while (length(pending) > 0L) {
    last <- length(pending)
    current <- pending[[last]]
    pending[[last]] <- NULL

    if (identical(current$type, "element")) {
      count <- count + 1L + length(current$attr_names)
      kids <- current$kids
      for (i in rev(seq_along(kids))) {
        pending[[length(pending) + 1L]] <- kids[[i]]
      }
    } else if (identical(current$type, "text")) {
      count <- count + 1L
    }
  }

  entry_types <- character(count)
  entry_names <- character(count)
  entry_values <- character(count)
  owners <- integer(count)
  attr_positions <- integer(count)
  parents <- integer(count)
  subtree_end <- integer(count)
  nodes <- vector("list", count)
  children <- vector("list", count)
  attributes <- vector("list", count)
  element_ids <- integer(count)
  element_count <- 0L
  virtual_children <- integer()
  next_id <- 0L
  pending <- list(list(node = root, parent = 0L, exit = FALSE, id = 0L))

  while (length(pending) > 0L) {
    last <- length(pending)
    task <- pending[[last]]
    pending[[last]] <- NULL

    if (task$exit) {
      subtree_end[[task$id]] <- next_id
    } else {
      current <- task$node
      parent <- task$parent

      if (identical(current$type, "element")) {
        next_id <- next_id + 1L
        id <- next_id
        entry_types[[id]] <- "element"
        entry_names[[id]] <- current$name
        parents[[id]] <- parent
        nodes[[id]] <- current
        children[[id]] <- integer()
        attributes[[id]] <- integer()
        element_count <- element_count + 1L
        element_ids[[element_count]] <- id

        if (parent == 0L) {
          virtual_children <- c(virtual_children, id)
        } else {
          children[[parent]] <- c(children[[parent]], id)
        }

        attr_names <- current$attr_names
        attr_values <- current$attr_values
        attr_ids <- integer(length(attr_names))
        for (i in seq_along(attr_names)) {
          next_id <- next_id + 1L
          attr_id <- next_id
          entry_types[[attr_id]] <- "attribute"
          entry_names[[attr_id]] <- attr_names[[i]]
          entry_values[[attr_id]] <- attr_values[[i]]
          owners[[attr_id]] <- id
          attr_positions[[attr_id]] <- i
          parents[[attr_id]] <- id
          attr_ids[[i]] <- attr_id
        }
        attributes[[id]] <- attr_ids

        pending[[length(pending) + 1L]] <- list(
          node = NULL,
          parent = 0L,
          exit = TRUE,
          id = id
        )
        kids <- current$kids
        for (i in rev(seq_along(kids))) {
          pending[[length(pending) + 1L]] <- list(
            node = kids[[i]],
            parent = id,
            exit = FALSE,
            id = 0L
          )
        }
      } else if (identical(current$type, "text")) {
        next_id <- next_id + 1L
        id <- next_id
        entry_types[[id]] <- "text"
        entry_values[[id]] <- current$value
        parents[[id]] <- parent
        nodes[[id]] <- current

        if (parent == 0L) {
          virtual_children <- c(virtual_children, id)
        } else {
          children[[parent]] <- c(children[[parent]], id)
        }
      }
    }
  }

  list(
    types = entry_types,
    names = entry_names,
    values = entry_values,
    owners = owners,
    attr_positions = attr_positions,
    parents = parents,
    subtree_end = subtree_end,
    nodes = nodes,
    children = children,
    attributes = attributes,
    element_ids = element_ids[seq_len(element_count)],
    virtual_children = virtual_children
  )
}

query_matches <- function(candidate, step, index) {
  if (identical(step$test, "attribute")) {
    if (candidate == 0L) {
      integer()
    } else {
      attr_ids <- index$attributes[[candidate]]
      attr_ids[index$names[attr_ids] == step$name]
    }
  } else {
    if (candidate == 0L) {
      child_ids <- index$virtual_children
    } else {
      child_ids <- index$children[[candidate]]
    }

    if (identical(step$test, "element")) {
      element_ids <- child_ids[index$types[child_ids] == "element"]
      if (identical(step$name, "*")) {
        element_ids
      } else {
        element_ids[index$names[element_ids] == step$name]
      }
    } else {
      child_ids[index$types[child_ids] == "text"]
    }
  }
}

query_filter <- function(matches, predicate, index) {
  if (identical(predicate$type, "position")) {
    if (length(matches) >= predicate$value) {
      matches[predicate$value]
    } else {
      integer()
    }
  } else {
    keep <- logical(length(matches))
    for (i in seq_along(matches)) {
      attr_ids <- index$attributes[[matches[[i]]]]
      attr_names <- index$names[attr_ids]
      named <- attr_names == predicate$name

      if (identical(predicate$type, "has_attribute")) {
        keep[[i]] <- any(named)
      } else {
        attr_values <- index$values[attr_ids]
        keep[[i]] <- any(named & attr_values == predicate$value)
      }
    }
    matches[keep]
  }
}

evaluate_path <- function(root, steps) {
  if (length(steps) == 0L) {
    character(0)
  } else {
    index <- query_index(root)
    contexts <- 0L

    for (step in steps) {
      chunks <- list()
      for (context in contexts) {
        if (identical(step$axis, "child")) {
          candidates <- context
        } else if (context == 0L) {
          candidates <- c(0L, index$element_ids)
        } else {
          end <- index$subtree_end[[context]]
          in_subtree <- index$element_ids >= context &
            index$element_ids <= end
          candidates <- index$element_ids[in_subtree]
        }

        for (candidate in candidates) {
          matches <- query_matches(candidate, step, index)
          for (predicate in step$predicates) {
            matches <- query_filter(matches, predicate, index)
          }
          if (length(matches) > 0L) {
            chunks[[length(chunks) + 1L]] <- matches
          }
        }
      }

      selected <- as.integer(unlist(chunks, use.names = FALSE))
      contexts <- sort(unique(selected), method = "radix")
    }

    values <- character(length(contexts))
    for (i in seq_along(contexts)) {
      id <- contexts[[i]]
      if (identical(index$types[[id]], "element")) {
        values[[i]] <- string_value(index$nodes[[id]])
      } else {
        values[[i]] <- index$values[[id]]
      }
    }

    unname(values)
  }
}
