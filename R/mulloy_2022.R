mulloy_2022_primes <- function(x) {
  UseMethod("mulloy_2022_primes")
}

#' @rdname mulloy_2022_primes
#' @export
mulloy_2022_primes.default <- function(x) {
  x <- hrep::pi_chord(x)
  do.call(mulloy_2022_primes, as.list(environment()))
}

#' @rdname mulloy_2022_primes
#' @export
mulloy_2022_primes.pi_chord <- function(x) {
  chord <- as.numeric(x)
  root = min(chord)
  chord = chord - root
  c(0,16,12,10,9,7,12,5,11,8,14,14,2)[chord[2]+1]
}

