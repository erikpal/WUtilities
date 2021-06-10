#' Update a specific Qualtrics user
#'
#' Get user details for the provided user id.
#' @param userID User ID to update details for
#' @param userType The type of user which determines privileges.  "Default access" is the default state for students.  "Standard - Webster University" is the default for faculty/staff.
#' @param accountExpirationDate Set the date that the access expires (yyyy-mm-dd)
#' @param url From Renviron OR alternative name in R.environ OR url of server
#' @export

updateQUser <- function(userID,
                        userType = "Standard - Webster University",
                        accountExpirationDate = NULL,
                        url = "QualtricsUrl") {

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

        body <- list(
                userType = userType,
                accountExpirationDate = accountExpirationDate
        )


        body <- jsonlite::toJSON(body, auto_unbox = TRUE, POSIXt = "ISO8601")


        response <- PUT(url,
                        add_headers("x-api-token" = key),
                        content_type_json(),
                        body = body)

        status <- http_status(response)
        print(status$message)

        content <- content(response, as = "text")
        content <- jsonlite::fromJSON(content, flatten = TRUE)

        return(content)
}
