#' Parse a Canvas URL
#'
#' Extract a named vector of IDs from a Canvas URL.
#' @param url
#'
#' @return
#' @export
#'
#' @examples
parseCanvasUrl <- function(url) {
        require(httr)
        require(stringr)

        url_parsed <- httr::parse_url(url)

        ## Extract the key id numbers
        path <- url_parsed$path
        path <- gsub("(\\d)/(\\w)", "\\1SPLITHERE\\2", path)
        path <- stringr::str_split(path, "SPLITHERE")

        id_pairs <- path[[1]][grepl("/\\d*", path[[1]])]

        id_pairs_final <- gsub("(\\w*)/(\\d*)", "\\2", id_pairs)
        names(id_pairs_final) <- gsub("(\\w*)/(\\d*)", "\\1", id_pairs)

        return(id_pairs_final)

}
