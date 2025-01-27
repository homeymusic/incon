#' Simultaneous consonance models
#'
#' This \code{\link[tibble]{tibble}} summarises the
#' consonance models available in the \code{incon} package.
#' * \code{label}: Label for the model, used by \code{\link{incon}}.
#' * \code{citation}: Citation for the model.
#' * \code{class}: Organises the models by psychological theory.
#' * \code{package}: Identifies the package in which the model is implemented.
#' * \code{consonance}:
#' \code{TRUE} if the model should positively correlate with consonance,
#' \code{FALSE} if the model should negatively correlate with consonance.
#' * \code{spectrum_sensitive}: Whether the model is sensitive to
#' the spectral characteristics of the input,
#' in particular the number of harmonics
#' and the roll-off rate.
#' * \code{continuous_pitch}: Whether the model can take continuous pitch inputs.
#' * \code{f}: Function to call the model.
#' @docType data
#' @keywords data
#' @md
#' @export
incon_models <- list()

#' List models
#'
#' Lists the consonance/dissonance models that can be selected in
#' \code{\link{incon}},
#' @return A character vector of consonance/dissonance models.
#' @export
list_models <- function() {
  incon_models$label
}

add_model <- function(label,
                      citation,
                      class,
                      package,
                      consonance,
                      spectrum_sensitive,
                      continuous_pitch,
                      f,
                      qual.major_minor_polarity=NA,
                      qual.octave_complementarity=NA,
                      qual.tonic_octave_similarity=NA,
                      qual.thirds_sixths_brightness=NA,
                      qual.brightness_symmetry=NA,
                      qual.intuitive_affinity_order=NA) {
  checkmate::qassert(class, "S1")
  checkmate::qassert(citation, "S1")
  checkmate::qassert(package, "S1")
  checkmate::qassert(spectrum_sensitive, "B1")
  checkmate::qassert(continuous_pitch, "B1")
  checkmate::qassert(consonance, "B1")
  checkmate::qassert(label, "S1")
  stopifnot(is.function(f),
            identical(methods::formalArgs(f),
                      c("x", "num_harmonics", "roll_off", "...")))
  incon_models[[length(incon_models) + 1L]] <<- tibble::tibble(
    label,
    citation,
    class,
    package,
    consonance,
    spectrum_sensitive,
    continuous_pitch,
    f = list(f),
    qual.major_minor_polarity,
    qual.octave_complementarity,
    qual.tonic_octave_similarity,
    qual.thirds_sixths_brightness,
    qual.brightness_symmetry,
    qual.intuitive_affinity_order
  )
}

add_model("gill_09_harmonicity",
          "Gill & Purves (2009)",
          "Periodicity/harmonicity",
          "bowl18",
          consonance = TRUE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            bowl18::gill09_harmonicity(x, ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=1,
          qual.intuitive_affinity_order=1)

add_model("har_18_harmonicity",
          "Harrison & Pearce (2018)",
          "Periodicity/harmonicity",
          "har18",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            har18::pc_harmonicity(x,
                                  method = "kl",
                                  num_harmonics = num_harmonics,
                                  rho = roll_off * 0.75,
                                  ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("milne_13_harmonicity",
          "Milne (2013)",
          "Periodicity/harmonicity",
          "har18",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            har18::pc_harmonicity(x,
                                  method = "peak",
                                  num_harmonics = num_harmonics,
                                  rho = roll_off * 0.75,
                                  ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("parn_88_root_ambig",
          "Parncutt (1988)",
          "Periodicity/harmonicity",
          "parn88",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            parn88::root_ambiguity(x, ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("parn_94_complex",
          "Parncutt & Strasburger (1994)",
          "Periodicity/harmonicity",
          "parn94",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            parn94::complex_sonor(x,
                                  num_harmonics = num_harmonics,
                                  roll_off = roll_off,
                                  ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=0,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("stolz_15_periodicity",
          "Stolzenburg (2015)",
          "Periodicity/harmonicity",
          "stolz15",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            stolz15::smooth_log_periodicity(x, ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=1,
          qual.brightness_symmetry=1,
          qual.intuitive_affinity_order=1)

add_model("bowl_18_min_freq_dist",
          "Bowling et al. (2018)",
          "Interference",
          "bowl18",
          consonance = TRUE,
          spectrum_sensitive = FALSE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            bowl18::bowl18_min_freq_dist(x, ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=0,
          qual.tonic_octave_similarity=0,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("huron_94_dyadic",
          "Huron (1994)",
          "Interference",
          "incon",
          consonance = TRUE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            huron_1994(x, ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("hutch_78_roughness",
          "Hutchinson & Knopoff (1978)",
          "Interference",
          "dycon",
          consonance = FALSE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            dycon::roughness_hutch(x,
                                   num_harmonics = num_harmonics,
                                   roll_off = roll_off,
                                   ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=1,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=1)

add_model("parn_94_pure",
          "Parncutt & Strasburger (1994)",
          "Interference",
          "parn94",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            parn94::pure_sonor(x,
                               num_harmonics = num_harmonics,
                               roll_off = roll_off,
                               ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=1)

add_model("seth_93_roughness",
          "Sethares (1993)",
          "Interference",
          "dycon",
          consonance = FALSE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            dycon::roughness_seth(x,
                                  num_harmonics = num_harmonics,
                                  roll_off = roll_off,
                                  ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=1)

add_model("vass_01_roughness",
          "Vassilakis (2001)",
          "Interference",
          "dycon",
          consonance = FALSE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            dycon::roughness_vass(x,
                                  num_harmonics = num_harmonics,
                                  roll_off = roll_off,
                                  ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("wang_13_roughness",
          "Wang et al. (2013)",
          "Interference",
          "wang13",
          consonance = FALSE,
          spectrum_sensitive = TRUE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            wang13::roughness_wang(x,
                                   num_harmonics = num_harmonics,
                                   roll_off = roll_off,
                                   msg = NULL,
                                   ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=1)

add_model("jl_12_tonal",
          "Johnson-Laird et al. (2012)",
          "Culture",
          "jl12",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            jl12::jl_tonal_dissonance(x, ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=0,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0
          )

add_model("har_19_corpus",
          "Harrison & Pearce (2019)",
          "Culture",
          "corpdiss",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            corpdiss::corpus_dissonance(x, ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=1,
          qual.intuitive_affinity_order=0)

add_model("parn_94_mult",
          "Parncutt & Strasburger (1994)",
          "Numerosity",
          "parn94",
          consonance = TRUE, # TODO: should this be FALSE?
          spectrum_sensitive = TRUE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            parn94::multiplicity(x,
                                 num_harmonics = num_harmonics,
                                 roll_off = roll_off,
                                 ...),
          qual.major_minor_polarity=0,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=0,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("har_19_composite",
          "Harrison & Pearce (2019)",
          "Composite",
          "incon",
          consonance = TRUE,
          spectrum_sensitive = TRUE,
          continuous_pitch = FALSE,
          f = function(x, num_harmonics, roll_off, ...)
            har_19_composite(x,
                             num_harmonics = num_harmonics,
                             roll_off = roll_off,
                             ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=1,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=0,
          qual.intuitive_affinity_order=0)

add_model("mulloy_22_primes",
          "Mulloy (2022)",
          "Periodicity/harmonicity",
          "incon",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            mulloy_2022_primes(x, ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=0,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=1,
          qual.intuitive_affinity_order=1)

add_model("euler_1739_gradus_suavitatis",
          "Euler (1739)",
          "Periodicity/harmonicity",
          "incon",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            euler_1739_gradus_suavitatis(x, ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=0,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=1,
          qual.intuitive_affinity_order=1)

add_model("vogel_1976_modified_euler",
          "Vogel (1976)",
          "Periodicity/harmonicity",
          "incon",
          consonance = FALSE,
          spectrum_sensitive = FALSE,
          continuous_pitch = TRUE,
          f = function(x, num_harmonics, roll_off, ...)
            vogel_1976_modified_euler(x, ...),
          qual.major_minor_polarity=1,
          qual.octave_complementarity=1,
          qual.tonic_octave_similarity=0,
          qual.thirds_sixths_brightness=0,
          qual.brightness_symmetry=1,
          qual.intuitive_affinity_order=1)

incon_models <- dplyr::bind_rows(incon_models)
qual_score <- ((incon_models %>%
                 dplyr::select(label,
                               qual.major_minor_polarity,
                               qual.octave_complementarity,
                               qual.tonic_octave_similarity,
                               qual.thirds_sixths_brightness,
                               qual.intuitive_affinity_order,
                               qual.brightness_symmetry) %>%
                 dplyr::mutate(qual.score=(6*qual.major_minor_polarity+
                                             5*qual.octave_complementarity+
                                             4*qual.thirds_sixths_brightness+
                                             3*qual.tonic_octave_similarity+
                                             2*qual.intuitive_affinity_order+
                                             1*qual.brightness_symmetry))) %>% dplyr::arrange(desc(qual.score)))
stopifnot(!anyDuplicated(c("any", incon_models$label)))
