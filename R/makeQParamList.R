#' Create a Live Excel Parameter List
#'
#' @description This function uses a parsed URL to identify the needed parameters for a query.  This is used
#' to supply the search parameters without being prompted.
#'
#' @param parsedUrl Parsed url from the parseIqyUrl function
#'
#' @return
#' @export
#'
#' @examples
#' url <- parseIqyUrl("../path/to/query.iqy")
#' parameters <- makeQParamList(url)
#' parameters  ##To look at the requirements
#'
#' parameters$parameter_0 <- "2018"
makeQParamList <- function(parsedUrl) {
        QParamList <- parsedUrl$query[grepl("parameter", x = names(parsedUrl$query))]
        return(QParamList)
}
