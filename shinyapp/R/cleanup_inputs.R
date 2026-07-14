

#' Remove Inputs
#'
#' @param id the namespace of the module
#' @param .input the Shiny session input object
#'
#' @export

cleanup_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
}
