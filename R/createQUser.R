#' Create a specific Qualtrics user
#'
#' Get user details for the provided user id.
#' @param username User ID
#' @param firstName User first name
#' @param lastName User last name
#' @param userType The internal id of the user type. Default is id for "default access".
#' @param email User email
#' @param password User password
#' @param divisionId The internal id of the user's divison. Default is id for "Webster Worldwide".
#' @param language User language
#' @param url From Renviron OR alternative name in R.environ OR url of server
#' @export

createQUser <- function(username,
                        firstName,
                        lastName,
                        userType = "UT_6D93CHF3IYryJjn",
                        email,
                        password = "default",
                        language = "en",
                        divisionId = "DV_bf2LTv8JZn54Uh7",
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

        url$path <- "API/v3/users"

        body <- list(
                username = username,
                firstName = firstName,
                lastName = lastName,
                userType = userType,
                email = email,
                password = password,
                divisionId = divisionId,
                language = language
        )


        body <- jsonlite::toJSON(body, auto_unbox = TRUE, POSIXt = "ISO8601")


        response <- POST(url,
                        add_headers("x-api-token" = key),
                        content_type_json(),
                        body = body)

        status <- http_status(response)
        print(status$message)

        content <- content(response, as = "text")
        content <- jsonlite::fromJSON(content, flatten = TRUE)

        return(content)
}
