#' Normalise a codebook into a named character vector of definitions
#'
#' A codebook may be supplied either as an unnamed character vector of code
#' labels (e.g. `c("UI_Confusion", "Feature_Request")`) or as a named vector
#' whose names are code labels and whose values are definitions given to the
#' LLM (e.g. `c(UI_Confusion = "The user says they were lost or confused")`).
#'
#' @param codebook Character vector, optionally named.
#' @return A named character vector: names are syntactic column names, values
#'   are definitions.
#' @noRd
normalise_codebook <- function(codebook, call = rlang::caller_env()) {
  if (!is.character(codebook) || length(codebook) == 0) {
    cli::cli_abort("{.arg codebook} must be a non-empty character vector.", call = call)
  }

  codes <- names(codebook)
  if (is.null(codes)) {
    codes <- codebook
    codebook <- paste("The text expresses or indicates:", gsub("_", " ", codebook))
  }
  missing_name <- is.na(codes) | codes == ""
  if (any(missing_name)) {
    cli::cli_abort("Every code in {.arg codebook} needs a name.", call = call)
  }

  bad <- codes != make.names(codes)
  if (any(bad)) {
    cli::cli_abort(c(
      "Code names must be syntactic R column names.",
      x = "Invalid: {.val {codes[bad]}}.",
      i = "Try {.val {make.names(codes[bad])}}."
    ), call = call)
  }
  if (anyDuplicated(codes)) {
    cli::cli_abort("Code names in {.arg codebook} must be unique.", call = call)
  }

  stats::setNames(unname(codebook), codes)
}
