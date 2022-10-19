euler_1739_gradus_suavitatis <- function(x) {
  UseMethod("euler_1739_gradus_suavitatis")
}

#' @rdname euler_1739_gradus_suavitatis
#' @export
euler_1739_gradus_suavitatis.default <- function(x) {
  x <- hrep::pi_chord(x)
  do.call(euler_1739_gradus_suavitatis, as.list(environment()))
}

#' @rdname euler_1739_gradus_suavitatis
#' @export
euler_1739_gradus_suavitatis.pi_chord <- function(x) {
  chord <- as.numeric(x)
  root = min(chord)
  chord = chord - root
  c(1, 11, 8, 8, 7, 5, 8.5, 4, 8, 7, 9, 10, 2)[chord[2]+1]
}

