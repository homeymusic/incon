vogel_1976_modified_euler <- function(x) {
  UseMethod("vogel_1976_modified_euler")
}

#' @rdname vogel_1976_modified_euler
#' @export
vogel_1976_modified_euler.default <- function(x) {
  x <- hrep::pi_chord(x)
  do.call(vogel_1976_modified_euler, as.list(environment()))
}

#' @rdname vogel_1976_modified_euler
#' @export
vogel_1976_modified_euler.pi_chord <- function(x) {
  chord <- as.numeric(x)
  root = min(chord)
  chord = chord - root
  c(1, 12, 9, 9, 7, 5, 9.5, 4, 8, 8, 10, 11, 2)[chord[2]+1]
}

