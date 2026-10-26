decode_bytes <- function(bytes) {
  octets <- as.integer(bytes)
  size <- length(octets)
  valid <- !any(octets == 0L)
  position <- 1L
  while (position <= size && valid) {
    first <- octets[[position]]
    if (first <= 127L) {
      position <- position + 1L
    } else if (first >= 194L && first <= 223L) {
      if (position + 1L <= size) {
        second <- octets[[position + 1L]]
        valid <- second >= 128L && second <= 191L
      } else {
        valid <- FALSE
      }
      if (valid) {
        position <- position + 2L
      }
    } else if (first >= 224L && first <= 239L) {
      if (position + 2L <= size) {
        second <- octets[[position + 1L]]
        third <- octets[[position + 2L]]
        second_valid <- second >= 128L && second <= 191L
        if (first == 224L) {
          second_valid <- second >= 160L && second <= 191L
        }
        if (first == 237L) {
          second_valid <- second >= 128L && second <= 159L
        }
        valid <- second_valid && third >= 128L && third <= 191L
      } else {
        valid <- FALSE
      }
      if (valid) {
        position <- position + 3L
      }
    } else if (first >= 240L && first <= 244L) {
      if (position + 3L <= size) {
        second <- octets[[position + 1L]]
        third <- octets[[position + 2L]]
        fourth <- octets[[position + 3L]]
        second_valid <- second >= 128L && second <= 191L
        if (first == 240L) {
          second_valid <- second >= 144L && second <= 191L
        }
        if (first == 244L) {
          second_valid <- second >= 128L && second <= 143L
        }
        valid <- second_valid && third >= 128L && third <= 191L &&
          fourth >= 128L && fourth <= 191L
      } else {
        valid <- FALSE
      }
      if (valid) {
        position <- position + 4L
      }
    } else {
      valid <- FALSE
    }
  }
  if (valid) {
    text <- rawToChar(bytes)
    if (validUTF8(text)) {
      Encoding(text) <- "UTF-8"
      text
    } else {
      NULL
    }
  } else {
    NULL
  }
}

new_inputs <- function(io) {
  state <- new.env(parent = emptyenv())
  state$stdin_used <- FALSE
  read <- function(arg) {
    if (identical(arg, "-")) {
      label <- "<stdin>"
      if (state$stdin_used) {
        fail("standard input is named twice")
      }
      state$stdin_used <- TRUE
      bytes <- tryCatch(
        io$read_stdin(),
        error = function(error) {
          fail(paste0(label, ": cannot read the file"))
        }
      )
    } else {
      label <- plain_text(arg)
      bytes <- tryCatch(
        io$read_file(arg),
        error = function(error) {
          fail(paste0(label, ": cannot read the file"))
        }
      )
    }
    text <- decode_bytes(bytes)
    if (is.null(text)) {
      fail(paste0(label, ": input is not valid UTF-8"))
    }
    list(label = label, text = text)
  }
  list(read = read)
}
