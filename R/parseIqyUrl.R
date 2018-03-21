#' Live Excel IQY Parser
#'
#' @param path Character of the path to an Informer generated "Live Excel" .iqy file.
#'
#' @return
#' @export
#'
#' @examples url <- parseIqyUrl("../path/to/query.iqy")
parseIqyUrl <- function(path) {
        require(httr)
        q <- readLines(path)
        qurl <- paste(q[3], q[4], sep = "?")
        qurl <- parse_url(qurl)
        return(qurl)
}
