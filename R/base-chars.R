# Character predicates for the XML 1.0 and XML name character sets.

in_ranges <- function(x, low, high) {
  result <- rep(FALSE, length(x))
  for (i in seq_along(low)) {
    inside <- !is.na(x) & x >= low[[i]] & x <= high[[i]]
    result[inside] <- TRUE
  }
  result
}

is_xml_char <- function(x) {
  in_ranges(
    x,
    c(9L, 10L, 13L, 32L, 57344L, 65536L),
    c(9L, 10L, 13L, 55295L, 65533L, 1114111L)
  )
}

is_name_start <- function(x) {
  in_ranges(
    x,
    c(
      58L, 65L, 95L, 97L, 192L, 216L, 248L, 880L, 895L, 8204L,
      8304L, 11264L, 12289L, 63744L, 65008L, 65536L
    ),
    c(
      58L, 90L, 95L, 122L, 214L, 246L, 767L, 893L, 8191L, 8205L,
      8591L, 12271L, 55295L, 64975L, 65533L, 983039L
    )
  )
}

is_name_char <- function(x) {
  is_name_start(x) | in_ranges(
    x,
    c(45L, 46L, 48L, 183L, 768L, 8255L),
    c(45L, 46L, 57L, 183L, 879L, 8256L)
  )
}

is_space <- function(x) {
  in_ranges(x, c(9L, 10L, 32L), c(9L, 10L, 32L))
}
