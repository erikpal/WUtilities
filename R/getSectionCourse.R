#' Check Section for Course
#'
#' Takes a Canvas SIS course ID and checks it as a section ID to determine what
#' course it is a part of.
#' @param wcrid Canvas course SIS ID
#' @param server Beta, test, or production
#' @param verbose Boolean to see API calls in the console
#'
#' @return
#' @export
#'
#' @examples
getSectionCourse <- function(wcrid, server = "test", verbose = FALSE) {
        require(bRush)
        require(dplyr)

        x <- getSection(wcrid %>%
                                paste0("-sec") %>%
                                makeSisIDs(type = "section"),
                        server = server,
                        verbose = verbose)

        if ("message" %in% names(x)) {
                result <- x$message
        } else {
                result <- x[[1]]$sis_course_id
        }

        return(result)

}
