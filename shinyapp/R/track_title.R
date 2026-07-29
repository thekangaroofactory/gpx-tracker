

#' Track Title
#'
#' @param file the GPX file of the track
#'
#' @returns a character value
#' @export
#'
#' @examples
#' \dontrun{
#' track_title(file = "./foo.gpx")
#' }

track_title <- function(file) {
  sf::read_sf(file, layer = "tracks")$name
}
