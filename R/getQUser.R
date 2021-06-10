#' Get a specific Qualtrics user
#'
#' Get user details for the provided user id.
#' @param userID User ID to get details for
#' @param url From Renviron OR alternative name in R.environ OR url of server
#' @export

getQUser <- function(userID, url = "QualtricsUrl") {

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

        url$path <- "API/v3/users/userID"
        url$path <- sub("userID", userID, url$path)

        response <- GET(url, add_headers("x-api-token" = key))
        status <- http_status(response)
        print(status$message)

        content <- content(response, as = "text")
        content <- jsonlite::fromJSON(content, flatten = TRUE)

        return(content)
}
