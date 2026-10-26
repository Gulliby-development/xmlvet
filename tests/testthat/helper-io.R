# Helpers shared by the test files; they build inputs and read results and never
# compute an expected value.

as_bytes <- function(x) {
  if (is.null(x)) raw(0) else if (is.raw(x)) x else charToRaw(x)
}

# An in-memory io. files is a named list of file contents (a string or raw
# bytes); stdin is a string or raw bytes. The io records what it is given.
make_io <- function(files = list(), stdin = NULL) {
  state <- new.env()
  state$out <- ""
  state$err <- ""
  list(
    read_stdin = function() as_bytes(stdin),
    read_file = function(path) {
      if (is.null(files[[path]])) {
        stop("no such file")
      }
      as_bytes(files[[path]])
    },
    out = function(text) state$out <- paste0(state$out, text),
    err = function(text) state$err <- paste0(state$err, text),
    state = state
  )
}

# Runs main() on an in-memory io: the exit code and what was written.
run_main <- function(argv, files = list(), stdin = NULL) {
  io <- make_io(files, stdin)
  code <- main(argv, io)
  list(code = code, out = io$state$out, err = io$state$err)
}

# What reading one argument gives from an in-memory io.
read_once <- function(arg, files = list(), stdin = NULL) {
  new_inputs(make_io(files, stdin))$read(arg)
}

# The texts read when the same arguments are read one after the other.
read_all <- function(args, files = list(), stdin = NULL) {
  inputs <- new_inputs(make_io(files, stdin))
  vapply(
    args, function(arg) inputs$read(arg)$text, character(1L),
    USE.NAMES = FALSE
  )
}

# The message of the failure an expression raises, NA when it raises none.
failure_of <- function(expr) {
  tryCatch(
    {
      expr
      NA_character_
    },
    xmlvet_failure = function(e) conditionMessage(e)
  )
}

# The index and code of the syntax fault an expression raises, NULL when none.
fault_of <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    xmlvet_syntax = function(e) list(index = e$index, code = e$code)
  )
}

# The position and code of the path fault an expression raises.
path_fault_of <- function(expr) {
  tryCatch(
    {
      expr
      NULL
    },
    xmlvet_path = function(e) list(pos = e$pos, message = conditionMessage(e))
  )
}

# The first problem of a text as list(line, col, code, message), NULL if none.
problem_of <- function(text) {
  parsed <- parse_document(text)
  if (parsed$ok) {
    return(NULL)
  }
  parsed[c("line", "col", "code", "message")]
}

# The document of a text that is well formed.
doc_of <- function(text) {
  parse_document(text)$doc
}
