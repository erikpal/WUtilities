#' Convert CX timestamp
#'
#' Convert CX time stampe strings to HH:MM:SS
#' @param h CX 3-4 digit time stamp
#'
#' @return
#' @export
#'
#' @examples
CXtoHMS <- function(h) {
        require(lubridate)

        h <- as.character(h)
        h[h == "0"] <- "0000"
        h[nchar(h) == 1] <- paste0(h[nchar(h) == 1], "00")
        h[nchar(h) == 3] <- paste0("0", h[nchar(h) == 3])
        h <- gsub("(\\d{2})(\\d{2})", "\\1:\\2", h)
        h <- hm(h, quiet = TRUE)

        return(h)
}
