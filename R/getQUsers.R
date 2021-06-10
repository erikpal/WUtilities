#' Get all Qualtrics users
#'
#' Get all user details
#' @param url From Renviron OR alternative name in R.environ OR url of server
#' @export

getQUsers <- function(url = "QualtricsUrl") {

        require(httr)
        require(jsonlite)

        url <- "QualtricsUrl"
        key <- Sys.getenv("QualtricsApiKey")

        if (grepl("^http\\w{0,1}://", url)) {
                url <- url
        } else if (Sys.getenv(url) == "") {
                stop("Please place a url file in .Renviron")
        } else {
                url <- Sys.getenv(url)
        }

        url <- parse_url(url)

        url$path <- "API/v3/users"

        continue <- TRUE

        results <- list()
        page <- 0

        while(continue == TRUE) {

                page <- page + 1

                print(url)
                response <- GET(url, add_headers("x-api-token" = key))
                response <- content(response, as = "text")
                response <- jsonlite::fromJSON(response)

                results[[page]] <- response$result$elements
                url <- response$result$nextPage

                if(is.null(url)) {
                        continue <- FALSE
                }

        }

        content <- do.call(rbind, results)

        return(content)
}
