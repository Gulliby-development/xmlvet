count_paths <- function(root) {
  paths <- character()
  levels <- integer()
  counts <- integer()
  attrs <- list()
  stack <- list(
    list(node = root, parent_path = "", level = 1L)
  )

  while (length(stack) > 0L) {
    stack_size <- length(stack)
    current <- stack[[stack_size]]
    stack[[stack_size]] <- NULL
    node <- current$node

    if (identical(node$type, "element")) {
      if (current$level == 1L) {
        path <- paste0("/", node$name)
      } else {
        path <- paste0(current$parent_path, "/", node$name)
      }

      path_index <- match(path, paths)
      if (is.na(path_index)) {
        path_index <- length(paths) + 1L
        paths[[path_index]] <- path
        levels[[path_index]] <- current$level
        counts[[path_index]] <- 0L
        attrs[[path_index]] <- character()
      }
      counts[[path_index]] <- counts[[path_index]] + 1L

      for (attribute in node$attr_names) {
        seen <- attribute %in% attrs[[path_index]]
        if (!seen) {
          attrs[[path_index]] <- c(attrs[[path_index]], attribute)
        }
      }

      kids <- node$kids
      for (i in rev(seq_along(kids))) {
        child <- kids[[i]]
        if (identical(child$type, "element")) {
          stack[[length(stack) + 1L]] <- list(
            node = child,
            parent_path = path,
            level = current$level + 1L
          )
        }
      }
    }
  }

  list(paths = paths, levels = levels, counts = counts, attrs = attrs)
}
